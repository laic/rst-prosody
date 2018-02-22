import sys
import logging
import optparse
from os.path import basename
import re
import glob

import pandas  as pd

from collections import OrderedDict
import simplejson as json


logger = logging.getLogger(__name__)
logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')




## Depth first traversal
def augment_tree(t, xdf, xid='sid', tid='name'):
	## If not a leaf
	if 'children' in t.keys():
		for tchild in t['children']:
			augment_tree(tchild, xdf)
		return
	else: ## If we're at a leaf, add some info 
		#print "----leaf----"
		currtid = t[tid]
		currfeats = xdf[xdf[xid].astype(type(currtid)) == currtid]
		if currfeats.size > 0:
			for k in currfeats.columns.values: 
				t[k] = currfeats[k].max()

	return 

def collect_ancestors(t, nodedict):
	currid = t['name'] 
	currdepth = t['depth'] 
	if 'children' in t.keys():
		for tchild in t['children']:
			child_id = tchild['name'] 
			currdf = pd.DataFrame({'parents':[currid], 	
						'depths':[currdepth]}, index=[child_id])
			nodedict[child_id] = pd.concat([nodedict[currid], currdf]) 

			collect_ancestors(tchild, nodedict)

		return				
	else:
		return	



def getTreeDist(x, ancestors):
	currid = x['sid'].iloc[0]
	currdepth = x['depth'].iloc[0]

	nextid = x['next.sid'].iloc[0]
	nextdepth = x['next.depth'].iloc[0]

	## Last leaf
	if nextid == "NONE":
		logger.debug(nextid)
		return pd.DataFrame({'cdepth':[-1], 'treedist':[-1]}, index=[currid])
 
	## find common ancestors and get the one with greatest depth	
	curranc = ancestors[currid]
	nextanc = ancestors[nextid]
	manc = pd.merge(curranc, nextanc, on='parents', how='inner')
	manc = manc.sort_values('depths_x', ascending=False)	

	commond = pd.DataFrame(manc.iloc[0]).transpose()
	cdepth = commond['depths_x'].iloc[0]
	treedist = abs(cdepth - currdepth) + abs(cdepth - nextdepth)

	return pd.DataFrame({'cdepth':[cdepth], 'treedist':[treedist]}, index=[currid])

def add_range(xdf):
	print xdf.columns.values
	q1feats = filter(lambda x: re.search('q1', x), xdf.columns.values)
	if len(q1feats) == 0:
		return xdf
	else:
		print "HERE!"
		print q1feats

	for feat in q1feats:
		rfeat = feat.replace('q1', 'range') 	
		efeat = feat.replace('q1', 'q99')
		xdf[rfeat] = xdf[efeat] - xdf[feat]

	return xdf

def main():
        usage = "python augment-pros.py [OPTIONS]\nAdd prosodic info back into RST structure?"

        fmt = optparse.IndentedHelpFormatter(max_help_position=50, width=100)
        parser = optparse.OptionParser(usage=usage, formatter=fmt)
        group = optparse.OptionGroup(parser, 'Arguments',
                         'These options define arguments and parameters.')
        group.add_option('--infile', metavar='DIR', default='/disk/data3/clai/data/ted-trans/derived/segs/merged-edu/0002.alignedu.allpros.txt', 
			dest='infile', help='Input directory')
        group.add_option('--jsonfile', metavar='DIR', default='/disk/data3/clai/data/ted-trans/RST/ParsedProc0002.rst.json', 
			dest='jsonfile', help='Input directory')
        group.add_option('--featfile', metavar='DIR', default='/disk/data3/clai/data/ted-trans/featsets/edu-pros.txt', dest='featfile',
                     help='featfile')
        group.add_option('--metafile', metavar='DIR', default='/disk/data3/clai/data/ted-trans/derived/alignedu/0002.alignedu.txt.full.txt', dest='metafile',
                     help='featfile')
        group.add_option('--outfile', metavar='FILE', default="tmp.rst.pros", dest='outfile',
                     help='Output file')
        group.add_option('--idfeat', metavar='edu.id', default="edu.id", dest='idfeat',
                     help='name of id feature')
        group.add_option('-d', '--debug', action='count', default=0,
                     help='Enable verbose logging to stderr. Repeated options increase detail of debug output.')

        parser.add_option_group(group)
        options, _ = parser.parse_args()

        program = basename(sys.argv[0])
        logger = logging.getLogger(program)
        logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')

        logging.root.setLevel(level=logging.DEBUG)
        logger.debug("running %s" % ' '.join(sys.argv))

	##------------------------------------------------------------------------##
	## Get relevant features names and prosodic features 
	feats = pd.read_csv(options.featfile, sep="\t", header=None) 

	try:
		prosdf = pd.read_csv(options.infile, sep="\t", usecols=feats[0].tolist())
	except:
		#print feats[0].tolist()
		prosdf = pd.read_csv(options.infile, sep="\t")
		print set(feats[0].tolist()) - set(prosdf.columns.values)  

		raise SystemExit	

	prosdf['xid'] = prosdf[options.idfeat].apply(lambda x: x.split(".")[-1])
	prosdf = add_range(prosdf)
	print prosdf.head()

	#print prosdf[['q1.normF0', 'q99.normF0', 'range.normF0']]
	#raise SystemExit()

	## Get metadata
	mfeats = ['sid', 'edu.id', 'pid', 'depth', 'sibno', 'parent', 'starttime', 
 		'endtime', 'parent.id', 'spk', 'conv'] #, 'fulltext'] 
	metadf = pd.read_csv(options.metafile, sep="\t", usecols=mfeats)
	metadf = metadf.rename(columns={'parent':'parent_rel'})

	## add metadata for prosodic features
	mprosdf = pd.merge(metadf, prosdf, on=[options.idfeat, 'conv'], how='left')				
	mprosdf = mprosdf.sort_values('sid')

	## Add paragraph change indicators	
	mprosdf.insert(1, 'next.pid',  mprosdf['pid'].tail(-1).tolist() + ["NONE"]) 
	mprosdf.insert(1, 'prev.pid', ["NONE"] + mprosdf['pid'].head(-1).tolist())  
	mprosdf.insert(1, 'para.last', (mprosdf['pid'] != mprosdf['next.pid']).astype(int) )
	mprosdf.insert(1, 'para.start', (mprosdf['pid'] != mprosdf['prev.pid']).astype(int) )
	mprosdf.insert(1, 'para.change', (mprosdf['pid'] != mprosdf['next.pid']).astype(int) )

	## Add depths of previous and next leaves
	mprosdf.insert(9, 'prev.depth', [0] + mprosdf['depth'].head(-1).tolist())  
	mprosdf.insert(9, 'next.depth', mprosdf['depth'].tail(-1).tolist()+[0] )
	mprosdf.insert(1, 'next.sid',  mprosdf['sid'].tail(-1).tolist() + ["NONE"]) 

	mprosdf = mprosdf.drop(['xid','participant','next.pid','prev.pid'], axis=1)
	print mprosdf.head()

	## Get the full tree representation
	with open(options.jsonfile, "r") as f:
		rsttree = json.load(f)		

	## Get ancestor information (for finding common ancestors)
	ancestors = OrderedDict() 
	rootid = rsttree['name']
	ancestors[rootid] = pd.DataFrame()
	collect_ancestors(rsttree, ancestors)

	## Get distances (number of edges) between consecutive leaves and merge in
	td =  mprosdf.groupby('sid').apply(lambda x: getTreeDist(x, ancestors))
	td = td.reset_index().drop('level_1', axis=1)
	mprosdf = pd.merge(td, mprosdf, on='sid')
	mprosdf = mprosdf.fillna(0)
	logger.debug(mprosdf.shape)	

	mprosdf.to_csv(options.outfile, index=False, sep="\t")

	fixednames = [re.sub(r'[.]', '_', fname) for fname in mprosdf.columns.values ]
	mprosdf = mprosdf.rename(columns=dict(zip(mprosdf.columns.values, fixednames)))

	## Add info to json tree for d3 visualization 
	augment_tree(rsttree, mprosdf, xid='sid')
	outfile = options.outfile.replace(".txt", ".json")
	logger.debug(outfile)
        with open(outfile, "w") as f:
                f.write(json.dumps(rsttree, indent=4, separators=(',', ': ')))


		

if __name__ == "__main__":
        sys.exit(main())

