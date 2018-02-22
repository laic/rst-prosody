import optparse
import logging
import re
import sys
import os
import glob
import json

from collections import defaultdict

import pandas as pd


#def Tree():
#	return defaultdict(Tree)	

class Tree(defaultdict):
    def __call__(self):
        return Tree(self)

    def __init__(self, parent):
        self.parent = parent
        self.default_factory = self

def get_words(x):
	#print "get_words"
	words = x['text'].str.split()
	wdf = pd.DataFrame({'word':words.tolist()[0]}, index=range(len(words.tolist()[0]))) 
	return wdf 

def get_ancestor(t, node, diffdepth):
	ancnode = node		
	if diffdepth == 0: ## sibling
		ancnode = ancnode.parent
	elif diffdepth > 0: ## Go back up the tree 	
		for i in range(diffdepth+1):
			ancnode = ancnode.parent
	else:  ## add child to current node
		pass		
 	
	return ancnode

def format_line(x):
	line = x.replace(' (LeftToRight)', '_LeftToRight')
	line = line.replace(' (RightToLeft)', '_RightToLeft')
	line = line.replace('Some(topic-change', 'Root_topic-change')
	line = re.sub(r'^[)]', '', line)
	return line

def count_spaces(x):
	match = re.search("^[ ]+", x)
	if match:
		return match.end() - match.start()	
	
	return 0

def format_trans(trans):
	trans = re.sub(r"([a-zA-Z]) [']([stmd]) ", r"\1'\2 ", trans) 
	trans = re.sub(r"([a-zA-Z]) [']([stmd])$", r"\1'\2", trans) 
	trans = re.sub(r"([a-zA-Z]) [']ll ", r"\1'll ", trans) 
	trans = re.sub(r"([a-zA-Z]) [']ll$", r"\1'll$", trans) 
	trans = re.sub(r"([a-zA-Z]) [']ve ", r"\1've ", trans) 
	trans = re.sub(r"([a-zA-Z]) [']ve$", r"\1've$", trans) 
	trans = re.sub(r"([a-zA-Z]) [']re ", r"\1're ", trans) 
	trans = re.sub(r"([a-zA-Z]) [']re$", r"\1're$", trans) 
	trans = re.sub(r"([a-zA-Z]) n[']t", r"\1n't", trans) 

	return trans

def write_rs3_header(x, f):
	relations = x[['parent.rel','parenttype']].drop_duplicates() 
	relstrs =  "\t\t\t<rel name=\"" + relations['parent.rel'] + "\" type=\"" + relations['parenttype'] + "\"/>"  
	#print relations

	f.write("\t<header>\n")		
	f.write("\t\t<relations>\n")		
		
	f.write("\n".join(relstrs))  
		
	f.write("\n\t\t</relations>\n")		
	f.write("\t</header>\n")		

	return



def get_sibdf(x):
	pardf = x[['edu.id','parent.id','parenttype','sibno']]

	leftdf = pardf[pardf['sibno'] == 0]
	rightdf = pardf[pardf['sibno'] == 1]

	sibdf = pd.merge(leftdf, rightdf, on='parent.id')
	sibdf = sibdf.rename(columns=dict(zip(sibdf.columns.values, [u.replace("_x", "_left") for u in sibdf.columns.values])))
	sibdf = sibdf.rename(columns=dict(zip(sibdf.columns.values, [u.replace("_y", "_right") for u in sibdf.columns.values])))

	return sibdf

def write_rs3_body(x, f):

	x['segtype'] = "group"
	x['segtype'][x['istext']] = "segment" 
	x['endstr'] = "/>"
	
	sibdf = get_sibdf(x)	
	xpar = pd.merge(x, sibdf, on='parent.id')  

	## Set parent of satellite to be the nucleus,
	xpar['rs3par'] = xpar['parent.id']
	xpar['rs3par'][ (xpar.nucleus==0) & (xpar.reldir == "Right")]  = xpar['edu.id_left'][(xpar.nucleus==0) & (xpar.reldir == "Right")] 
	xpar['rs3par'][ (xpar.nucleus==0) & (xpar.reldir == "Left")]  = xpar['edu.id_right'][(xpar.nucleus==0) & (xpar.reldir == "Left")] 

	xpar = xpar.sort_values('edu.id')
	## and we want relname to be the parent 
	xpar['rs3rel'] = xpar['parent.rel']

	## but for the nucleus, the relname should be a span?  
	xpar['rs3rel'][(xpar.nucleus==1) & (xpar.parenttype=="rst")]  = "span"

	## If it's a group node, the reltype will be "span" unless it's multinoc
	xpar['rs3type'] = xpar['parenttype'].copy()
	xpar['rs3type'][(xpar.segtype == "group") & (xpar.parenttype != "multinuc")] = "span"
	xpar['rs3type'][(xpar.segtype == "group") & (xpar.reltype == "multinuc")] = "multinuc"


	nodelist = xpar['edu.id'].tolist()
	xpar['hasparent'] = xpar['rs3par'].apply(lambda v: v in nodelist)

	xpar[['hasparent','edu.id','rs3par','nucleus','rs3type','parent.id','parenttype','rs3rel','parent','sibno']].to_csv("tmp.txt", sep="\t", na_rep="NA")

#	print "*--------- HERE ---------*"

	## Only group nodes have the type attribute
	typestr = " type=\"" + xpar['rs3type'] + "\" " 
	typestr[xpar['istext']] = ""

	## Parent id
	parstr = " parent=\"" + xpar['rs3par'].astype(str) + "\" "

	## How to close the node
	endstr = xpar['endstr'] 
	endstr[xpar['istext']] = ">" + xpar['text'][xpar['istext']] + "</segment>" 

	segstr = "\t<" + xpar['segtype'] + " id=\"" + xpar['edu.id'].astype(str) + "\"" + typestr + parstr + "relname=\"" + xpar['rs3rel'] + "\""
	#segstr = "\t<" + xpar['segtype'] + " id=\"" + xpar['edu.id'].astype(str) + "\"" + " relname=\"" + "elaboration" + "\""

	segstr = segstr + endstr 

#	print "\n".join(segstr)

	f.write("\t<body>\n")
	f.write("\n".join(segstr))
	f.write("\n\t</body>\n")

	return

def print_rs3(x, outfile): 
	print "**** print_rs3 *****"
	#print x.head()
	print outfile
	## remove fake root
	x = x[x['parent.id'] >= 0]

	with open(outfile, "w") as f:
		f.write("<rst>\n")
		write_rs3_header(x, f)
		write_rs3_body(x, f)
		f.write("</rst>\n")

	#raise SystemExit()

if __name__ == '__main__':

	usage = "python proc-rst-parse.py [OPTIONS]\nConvert RST parse into a flat file with word and edu ids"

	fmt = optparse.IndentedHelpFormatter(max_help_position=50, width=100)
	parser = optparse.OptionParser(usage=usage, formatter=fmt)

	group = optparse.OptionGroup(parser, 'Arguments',
			 'These options define arguments and parameters.')

	group.add_option('--input', metavar='FILE', default='/disk/data3/clai/data/ted-trans/TedFastNLPProcessor/ParsedProc0001.txt', dest='input',
	     	help='Input tree file space delimited')
	group.add_option('--outdir', metavar='FILE', default='./', dest='outdir',
	#group.add_option('--outdir', metavar='FILE', default='./', dest='outdir',
	     	help='Output directory')

        parser.add_option_group(group)
        options, _ = parser.parse_args()

        program = os.path.basename(sys.argv[0])
        logger = logging.getLogger(program)
        logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')

	t = Tree(None) 
	t['name'] = -1
	t['parent'] = None 
	t['value'] = 'root' 
	t['prevdepth'] = -2 
	t['depth'] = -1 
	currnode = t
	prevdepth = -1
	tlist= [] 
	with open(options.input, "r") as f:
		for i, line in enumerate(f):
			tline = format_line(line)
			currdepth = count_spaces(tline)/2

			currparent = get_ancestor(t, currnode, prevdepth-currdepth) 
			currwidth = len(currparent)-1	
			if len(currparent['children']) == 0:
				currparent['children'] = []

			sibno = len(currparent['children']) 

			trans = re.sub("^ *", "", tline).strip("\n")
			trans = format_trans(trans)
			currinfo = {'edu.id':i, 'value':trans, 
				'parent':currparent['value'], 'parent.id':currparent['name'], 'prevdepth':prevdepth, 'depth':currdepth, 'sibno':sibno}
			tlist.append(currinfo)
			
			currparent['children'].append(Tree(currparent))
			#currparent['children'][-1]['info'] = currinfo
			currparent['children'][-1]['name'] = i
			currparent['children'][-1]['parent'] = currparent['name'] 
			currparent['children'][-1]['prevdepth'] = prevdepth
			currparent['children'][-1]['depth'] = currdepth
			currparent['children'][-1]['sibno'] = sibno 
			currparent['children'][-1]['value'] = re.sub("^ *", "", tline).strip("\n") 
			prevdepth = currdepth
			currnode = currparent['children'][-1]

	#print t


	conv = os.path.basename(options.input).split(".")[0]
	outfile = options.outdir + "/" + conv + ".rst.json" 
	with open(outfile, "w") as f:
		f.write(json.dumps(t, indent=4, separators=(',', ': ')))


	tdf = pd.DataFrame(tlist)
	tdf = tdf.sort_values('edu.id')
	tdf['istext'] = tdf['value'].str.contains(r'^TEXT:')
	tdf['text'] = tdf['value'].str.replace(r'^TEXT: *', '')
	tdf['text'][tdf['istext']==False] = ""

	tdf['reltype'] = "multinuc"
	tdf['reltype'][tdf['value'].str.contains("Left")] = "rst"
	tdf['reltype'][tdf['value'].str.contains("TEXT")] = "text"

	tdf['relation'] = tdf['value'].str.replace(r'[_].*$', '')
	tdf['relation'][tdf['value'].str.contains("TEXT")] = "text"


	tdf['parent.rel'] = tdf['parent'].str.replace(r'[_].*$', '')
	tdf['parent.rel'][tdf['parent'].str.contains("TEXT")] = "text"

	tdf['parenttype'] = "multinuc"
	tdf['parenttype'][tdf['parent'].str.contains("Left")] = "rst"
	#tdf['parenttype'][tdf['parent'].str.contains("TEXT")] = "text"

	tdf['reldir'] = None 
	tdf['reldir'][tdf['parenttype']=='rst'] = tdf['parent'].str.replace("^.*To", "")   

	## Mark nodes as RST nuclei 
	tdf['nucleus'] = 0 
	#print tdf.columns.values
	tdf['nucleus'][ (tdf.reldir == "Right") &  (tdf.sibno == 0) & (tdf['parenttype'] == "rst") ] = 1  
	tdf['nucleus'][ (tdf.reldir == "Left") &  (tdf.sibno == 1) & (tdf['parenttype'] == "rst")]  = 1

	#tdf['nucleus'][ (tdf.reltype == "multinuc") ] = 1  


	#print tdf.head()
	outfile = options.outdir + "/" + conv + ".edu.txt" 
	print(outfile)
	tdf.to_csv(outfile, sep="\t", index=False)

	rs3file = options.outdir + "/" + conv.replace("ParsedProc","") + ".rs3" 
	print_rs3(tdf, rs3file)

	#outfile = options.outdir + "/" + conv + ".edu.json" 
	#tdf.to_json(outfile, orient="index")

	words = tdf[tdf['istext']].groupby('edu.id').apply(get_words)
	words = words.reset_index()
	words = words.reset_index()
	words = words.rename(columns={'index':'rst.word.id', 'level_1':'edu.word.id'})

	#print words.head()

	#words = pd.merge(words, tdf, on='edu.id')

	outfile = options.outdir + "/" + conv + ".edu.words.txt" 
	print(outfile)
	words.to_csv(outfile, sep="\t", index=False)

