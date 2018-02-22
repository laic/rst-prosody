import sys
import logging
import optparse
from os.path import basename
from collections import OrderedDict
#import Levenshtein
import numpy
import re

logger = logging.getLogger(__name__)
logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')

wpunc = re.compile('([^\w\s{}]$|^["])')
sattr = re.compile('^{[A-Z].*}$')

import pandas as pd

def getDistMat(s,t, subcost=2):
#	print s
#	print t
	d = numpy.zeros((len(s),len(t)), dtype=numpy.int)	

	for i in range(1,len(s)):
      		d[i, 0] = i

	for j in range(1,len(t)):
      		d[0, j] = j 
 
	for j in range(1, len(t)):
      		for i in range(1,len(s)):
          		if s[i][0] == t[j][0]:
            			d[i, j] = d[i-1, j-1]              
          		else:
            			d[i, j] = min(d[i-1, j] + 1,     #  a deletion
                               			d[i, j-1] + 1,   # an insertion
                               			d[i-1, j-1] + subcost) # a substitution

	#logger.debug(d)
#	print d
	return d

def levdist(s, t, subcost=2):

	d = getDistMat(s,t, subcost=subcost)

	## traceback 
	i = len(s)-1
	j = len(t)-1
	tb = []
	lastop = "none"

	while (i > 0 or j > 0): 
		logger.debug("---------")
		bdel = i > 0	
		binc = j > 0 	
		x = numpy.array([d[(max(i-1,0),max(j-1,0))], d[(max(i-1,0),j)],d[(i, max(j-1,0))]])
		ix = x.argmin()
		logger.debug("%s %s" % (s[i], t[j]))
		logger.debug("%d %d" % (i, j))
		logger.debug(bdel)
		logger.debug(binc)

		## Simple sub
		if ix == 0 and bdel and binc:
			logger.debug("sub")
			logger.debug("%s %s" % (s[i], t[j]))
			tb = [("*", s[i],t[j])] + tb	
			i = max(i - 1,0)	
			j = max(j - 1,0)
			lastop = "sub"
		## Edge case substitution: I expect there's a better way of doing this
		elif ix == 0 and not (bdel and binc) and lastop == "sub":
			logger.debug("sub")
			logger.debug("%s %s" % (s[i], t[j]))
			tb = [("*", s[i],t[j])] + tb	
			i = max(i - 1,0)	
			j = max(j - 1,0)
			lastop = "edgesub"
		elif ix == 1 or binc == False: 	
			logger.debug("del")
			tb = [("-", s[i],"-")] + tb	
			i = i - 1
			lastop = "del"
		else: 
			logger.debug("inc")
			tb = [("+", "+",t[j])] + tb	
			j = j - 1
			lastop = "inc"



	## Corner case
	if binc and bdel:
		tb = [("*", s[i],t[j])] + tb	
	elif binc and not bdel:
		tb = [("+", "+",t[j])] + tb	
	elif bdel and not binc:
		tb = [("-", s[i],"-")] + tb	
	else:
		logger.error("not a sub, ins, or del")
		raise SystemExit

	logger.debug(tb)
  	return tb

	
def getCTMline(line, currid):
	sline = line.split()
	curr = (None, None, None, None) 
	if sline[0] != "#":
		#print sline
		wstart = round(float(sline[2]),2)
		wend = wstart + round(float(sline[3]), 2)
		currword = sline[4].lower()
		curr = (currword, currid, wstart, wend) 
	
	return curr

	
def getp2faline(line, currid):
	sline = line.split()
	curr = (None, None, None, None) 
	if sline[1] != "sp":
		wstart = float(sline[2])
		wend = float(sline[3])
		currword = sline[1].lower()
		curr = (currword, currid, wstart, wend) 

	return curr

def getTransWords(filename, wordvar="word", idvar="word.id"):
	transwords = []
	transmeta = {}
	with open(filename) as f:
		transheader = f.readline().split()	
		logger.debug(transheader) 
		for line in f:
			sline = line.split()
			a = OrderedDict(zip(transheader, sline))
			if wordvar in a.keys(): 
				currword = a[wordvar].replace("_", " ").strip()
			#if "_word_" in a.keys():
			#	currword = a["_word_"].replace("_", " ").strip()
			#elif "word" in a.keys():
			#	currword = a["word"].replace("_", " ").strip()
			else:
				logger.debug("No wordvar?: %s" % wordvar)
				logger.debug(a)
				currword = ""
                		#raise SystemExit
	
			currword  = re.sub(r'[^A-Za-z0-9]$', "", currword)		

			if sattr.match(currword) == None: 
				currword = wpunc.sub("", currword.lower())
				subwords = currword.split()
				if len(subwords) > 1:
					j = 1 
					for sword in subwords:
						wid=a[idvar]+"."+str(j)
						transwords.append((sword, wid))
						transmeta[wid] = a
						j += 1
				else:
					transwords.append((currword, a[idvar]))
					transmeta[a[idvar]] = a

	return (transwords, transmeta)



def getAlignWords(alignfile, format="ctm"):
	alignwords = []
	i = 0
	with open(alignfile) as f:
		for line in f:
			if format == "ctm":
				curr = getCTMline(line, i)
			else:
				curr = getp2faline(line, i)
			if curr[0] != None: 
				alignwords.append(curr)
				i += 1

	return alignwords



def getWordIds(idfile, sep=" ", wordvar='word', idvar='word.id'):
	xdf = pd.read_csv(idfile, sep=sep)
	#print xdf.head()

	#xdf['word'] = re.sub(r'[^A-Za-z0-9]$', "", xdf['word'])		

	#if sattr.match(xdf['word']) == None: 
	xdf['word'] = xdf['word'].str.lower()
	xdf['word'] = xdf['word'].apply(lambda x: wpunc.sub("", x.strip()))

	xids = zip(xdf['word'], xdf['word.id'], xdf['startoffset'], xdf['endoffset'], xdf['pid'])
	#print xids
	return xids	

def writeAlignment(outfile, d, transmeta):
	metahead = transmeta[transmeta.keys()[0]].keys()
	#print transmeta[transmeta.keys()[0]]
	#print metahead
	with open(outfile, "w") as f:
		outheader = "op\ttrans\talign\twstart\twend\tword.id\tpid\ttrans.word.id\t" + "\t".join(metahead) + "\n"
		f.write(outheader)
		for x in d: 
			curr = x[0] + "\t" +  x[2][0] + "\t" + x[1][0] 
			if x[0] == "+":
				curr = "%s\tNA\tNA\tNA\tNA" % (curr)
			else:
				curr = "%s\t%f\t%f\t%s\t%s" % (curr, x[1][2], x[1][3], x[1][1], x[1][4])

			if x[0] == "-":
				curr = curr + "\tNA"
			else:
				curr = curr + "\t" +  x[2][1]
				#print transmeta[x[2][1]]
				for k,v in transmeta[x[2][1]].iteritems():
					curr = curr + "\t" + str(v) 
	
			curr = curr + "\n"			
			f.write(curr)

	return

def main():
        usage = "python levalign.py [OPTIONS]"

        fmt = optparse.IndentedHelpFormatter(max_help_position=50, width=100)
        parser = optparse.OptionParser(usage=usage, formatter=fmt)
        group = optparse.OptionGroup(parser, 'Arguments',
                         'These options define arguments and parameters.')
        group.add_option('-f', '--file', metavar='FILE', default=None, dest='filename',
                     help='Location of transcript file')
        group.add_option('-a', '--alignfile', metavar='FILE', default=None, dest='alignfile',
                     help='Location of alignment file')
        group.add_option('--idfile', metavar='FILE', default=None, dest='idfile',
                     help='Word identifier file')
        group.add_option('--metafile', metavar='FILE', default=None, dest='metafile',
                     help='Additional metadata to join in')
        group.add_option('-o', '--outdir', metavar='OUTDIR', default=None, dest='outdir',
                     help='Output directory')
        group.add_option('-d', '--debug', action='count', default=0,
                     help='Enable verbose logging to stderr. Repeated options increase detail of debug output.')
        group.add_option('--ctm', action='count', default=0, dest='ctm',
                     help='Alignfile has ctm format.')
        parser.add_option_group(group)
        options, _ = parser.parse_args()

        if len(sys.argv) == 1:
                parser.print_help()
                raise SystemExit

        program = basename(sys.argv[0])
        logger = logging.getLogger(program)
        logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')

        if options.debug > 0:
                logging.root.setLevel(level=logging.DEBUG)
                logger.debug("running %s" % ' '.join(sys.argv))
        else:
                logging.root.setLevel(level=logging.ERROR)

        fstem = basename(options.filename)
	logger.debug(fstem)	

	#######################################################################
 	transwords, transmeta = getTransWords(options.filename, wordvar="word", idvar="rst.word.id")
#	print transwords

 	idwords = getWordIds(options.idfile)
 	#alignwords = getAlignWords(options.alignfile)

	d = levdist(idwords, transwords, subcost=2)

	#print d

	fstem = fstem.replace("ParsedProc", "")
	outfile = options.outdir + "/" + fstem + ".wid"
	logger.debug(outfile)

	writeAlignment(outfile, d, transmeta)
	#######################################################################

if __name__ == "__main__":
        sys.exit(main())


