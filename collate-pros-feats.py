import sys
import logging
import optparse
from os.path import basename
#from collections import OrderedDict
#import Levenshtein
#import numpy
import re

import pandas  as pd

from collections import OrderedDict

logger = logging.getLogger(__name__)
logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')


##  These are the features we want from each feature file type
def get_featdict():
	featdict = OrderedDict()
	featdict['f0-diff'] = {'suffix':'aggs.alignXTYPE.txt.XTYPEdiff.txt',
			'replace':{'niteid':'XTYPE.id', 'wstart':'starttime','wend':'endtime'},
			'join':None}
	featdict['i0-diff'] = {'suffix':'aggs.alignXTYPE.txt.XTYPEdiff.txt',
				'replace':{'niteid':'XTYPE.id', 'wstart':'starttime', 'wend':'endtime'},
				'join':None}
	#			'join':['XTYPE.id','starttime','endtime','conv','participant','nxt_agent']}
	featdict['f0-boundary'] = {'suffix':'aggs.alignXTYPE.txt.boundary.XTYPE.txt', 
				'replace':{'sent.id':'XTYPE.id','prev.sent.id':'prev.XTYPE.id','next.sent.id':'next.XTYPE.id','pause.dur':'pause.dur_b'}, 
				'join':['XTYPE.id','starttime','endtime']}
	featdict['i0-boundary'] = {'suffix':'aggs.alignXTYPE.txt.boundary.XTYPE.txt', 
				'replace':{'sent.id':'XTYPE.id','prev.sent.id':'prev.XTYPE.id','next.sent.id':'next.XTYPE.id'}, 
				'join':['XTYPE.id','starttime','endtime']}

	return featdict


	#'i0-boundary-edu':{'suffix':'aggs.alignedu.txt.boundary.edu.txt', 
	#		'replace':{'sent.id':'edu.id'}, 
	#		'join':None}
	#'f0-diff-edu':'aggs.alignedu.txt.edudiff.txt',
	#'i0-diff-edu':'aggs.alignedu.txt.edudiff.txt'

def convert_featstruct(featstruct, segtype, segvar='XTYPE'):

	if type(featstruct) == str:
		return featstruct.replace(segvar, segtype) 
	if type(featstruct) == list:
		return [x.replace(segvar, segtype) for x in featstruct]
	if type(featstruct) in [dict, OrderedDict] :
		for ftype, fdict in featstruct.iteritems():
			featstruct[ftype] = convert_featstruct(fdict, segtype, segvar)	

	return featstruct  
 
def main():

	## Gather various prosodic features, 1 row per segment (e.g. edu, sent) 
        usage = "python collate-pros-feats.py [OPTIONS]"

        fmt = optparse.IndentedHelpFormatter(max_help_position=50, width=100)
        parser = optparse.OptionParser(usage=usage, formatter=fmt)
        group = optparse.OptionGroup(parser, 'Arguments',
                         'These options define arguments and parameters.')
        group.add_option('--conv', metavar='conv', default="0001", dest='conv',
                     help='conversation to collate')
        group.add_option('--segtype', metavar='conv', default="sent", dest='segtype',
                     help='segment type (suffix)')
        #group.add_option('--info', metavar='FILE', default='/disk/data3/clai/data/ted-trans/info/id_dname_year_sname', dest='info',
        #             help='Info file')
        group.add_option('--datadir', metavar='DIR', default='/disk/data3/clai/data/ted-trans/derived/segs/', dest='datadir',
                     help='Word timings')
        group.add_option('-d', '--debug', action='count', default=0,
                     help='Enable verbose logging to stderr. Repeated options increase detail of debug output.')

        group.add_option('--outfile', metavar='FILE', default="./tmp.pros.edu.txt", dest='outfile',
                     help='Output file')

        parser.add_option_group(group)
        options, _ = parser.parse_args()

	#=============================================================================

        if len(sys.argv) == 1:
                parser.print_help()
                raise SystemExit

        program = basename(sys.argv[0])
        logger = logging.getLogger(program)
        logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')

        logging.root.setLevel(level=logging.DEBUG)
        logger.debug("running %s" % ' '.join(sys.argv))

	#=============================================================================

	## Get the talk name
        conv = options.conv
        logger.debug(conv)

	convfeats = pd.DataFrame() 
	featdict = get_featdict()
	print
	print featdict
	print 
	featdict = convert_featstruct(featdict, options.segtype)
	print featdict


	#raise SystemExit()

	for k, v in featdict.iteritems():
		print "----------------"
		print k
		currfile = options.datadir + "/" + k + "-" + options.segtype + "/" + conv +"." + v['suffix']#.replace('XTYPE') 
		print currfile
		currfeats = pd.read_csv(currfile, sep=" ", na_values=['NA']) 	
		print currfeats.shape
		currfeats = currfeats.rename(columns=v['replace'])
		targetfeats = filter(lambda x: re.search(r'.slope$', x) == None, currfeats.columns.values)
		#print targetfeats
		currfeats = currfeats[targetfeats]
		if convfeats.size == 0:
			convfeats = currfeats  
		else:
			if v['join']:
				convfeats = pd.merge(convfeats, currfeats, how='outer', on=v['join'])
			else:
				convfeats = pd.merge(convfeats, currfeats, how='outer')

		print convfeats.shape
		#repfeats = filter(lambda x: re.search(r'_y$', x), convfeats.columns.values)
		repfeats = filter(lambda x: re.search(r'_x$', x), convfeats.columns.values)
		dropfeats = filter(lambda x: re.search(r'_y$', x), convfeats.columns.values)
		#print repfeats

		convfeats = convfeats.drop(dropfeats, axis=1)
		convfeats = convfeats.rename(columns=dict(zip(repfeats, [re.sub('_x$', '', x) for x in repfeats])))
			

	#print convfeats.columns.values
#	print convfeats.head()
	print convfeats.shape
	convfeats = convfeats.sort_values('starttime')

        ## This should get pushed back at some point
        convfeats = convfeats.rename(columns={'pause.dur':'next.pause.dur'})
	convfeats['prev.pause.dur'] = convfeats['starttime'] - convfeats['prev.wend']
        convfeats['spkrate'] = convfeats['nwords']/(convfeats['endtime']-convfeats['starttime'])
        convfeats['dur'] = (convfeats['endtime']-convfeats['starttime'])
        convfeats['next.change'] = (convfeats['participant'] != convfeats['next.participant']) * 1.0
        convfeats['prev.change'] = (convfeats['participant'] != convfeats['prev.participant']) * 1.0

	## Add the range features
        convfeats['range.normF0'] = (convfeats['q99.normF0'] - convfeats['q1.normF0'])
        convfeats['range.normI0'] = (convfeats['q99.normI0'] - convfeats['q1.normI0'])
        convfeats['next.range.normI0'] = list(convfeats['range.normI0'].tail(-1)) + [0.]
        convfeats['next.range.normF0'] = list(convfeats['range.normF0'].tail(-1)) + [0.]
        convfeats['prev.range.normI0'] = [0.] + list(convfeats['range.normI0'].head(-1) )
        convfeats['prev.range.normF0'] = [0.] + list(convfeats['range.normF0'].head(-1) )
        convfeats['pdiff.range.normF0'] = convfeats['prev.range.normF0'] - convfeats['range.normF0']
        convfeats['ndiff.range.normF0'] = convfeats['range.normF0'] - convfeats['next.range.normF0']
        convfeats['pdiff.range.normI0'] = convfeats['prev.range.normI0'] - convfeats['range.normI0']
        convfeats['ndiff.range.normI0'] = convfeats['range.normI0'] - convfeats['next.range.normF0']


	#convfeats = convfeats.fillna(-100.)
	convfeats = convfeats.fillna(0.)
	print options.outfile
	convfeats.to_csv(options.outfile, sep="\t", index=False, na_rep="NaN")


if __name__ == "__main__":
        sys.exit(main())

