import sys
import logging
import optparse
from os.path import basename
#from collections import OrderedDict
#import Levenshtein
#import numpy
import re

logger = logging.getLogger(__name__)
logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')

import pandas as pd


def getSegTimes(x):
	if pd.isnull(x['starttime'].iloc[0]):
		print x['edu.id'].iloc[0]

	starttime = x['starttime'].min() 
	endtime = x['endtime'].max() 
	spk = x['spk'].iloc[0]
	pid = x['pid'].iloc[0]	
	trans = "|" + " ".join(x['word']) +"|"
	xseg = pd.DataFrame({'spk':spk, 'starttime':starttime, 'endtime':endtime, 'pid':pid, 'trans':trans}, index=[int(x['edu.id'].iloc[0])])
	return xseg

def main():
        usage = "python get-rst-segs.py [OPTIONS]"

        fmt = optparse.IndentedHelpFormatter(max_help_position=50, width=100)
        parser = optparse.OptionParser(usage=usage, formatter=fmt)
        group = optparse.OptionGroup(parser, 'Arguments',
                         'These options define arguments and parameters.')
        group.add_option('-f', '--file', metavar='FILE', default=None, dest='filename',
                     help='Location of transcript file')
        #group.add_option('--info', metavar='FILE', default='/disk/data3/clai/data/ted-trans/info/id_dname_year_sname', dest='info',
        #             help='Info file')
        group.add_option('--alignfile', metavar='FILE', default=None, dest='alignfile',
                     help='Word timings')
        group.add_option('--metafile', metavar='FILE', default=None, dest='metafile',
                     help='RST info')
        group.add_option('-d', '--debug', action='count', default=0,
                     help='Enable verbose logging to stderr. Repeated options increase detail of debug output.')

        group.add_option('--outfile', metavar='FILE', default="./tmp.alignrst.txt", dest='outfile',
                     help='Output file')

        parser.add_option_group(group)
        options, _ = parser.parse_args()

        if len(sys.argv) == 1:
                parser.print_help()
                raise SystemExit

        program = basename(sys.argv[0])
        logger = logging.getLogger(program)
        logging.basicConfig(format='%(asctime)s: %(levelname)s: %(message)s')

	logging.root.setLevel(level=logging.DEBUG)
	logger.debug("running %s" % ' '.join(sys.argv))

        fstem = basename(options.filename)
	logger.debug(fstem)	

	#######################################################################
	wdf = pd.read_csv(options.alignfile, sep=" ")

	xdf = pd.read_csv(options.filename, sep="\t")
	xdf = xdf[pd.notnull(xdf['word.id'])]
	xdf = xdf[pd.notnull(xdf['edu.id'])]

	xdf = pd.merge(xdf, wdf, on='word.id', how='left')
	#xdf['rst.id'] = xdf['conv'] + ".edu." + xdf['edu.id'].astype(int).astype(str)
	xdf = xdf.sort_values('starttime')

	xseg = xdf.groupby('edu.id', sort=False).apply(getSegTimes)
	xseg = xseg.reset_index()
	xseg = xseg.rename(columns={'level_1':'sid'})
	xseg.sort_values('sid', inplace=True)

	xseg['conv'] = xdf['conv'].iloc[0]

	xseg['spk'][pd.isnull(xseg['spk'])] = xseg['spk'][pd.notnull(xseg['spk'])].iloc[0]
	xseg['part'] = xseg['spk']
	xseg['wavfile'] = xseg['conv'] + "-light.wav"
	xseg['chno'] = "NA"  
	#xseg = xseg.rename(columns={'rst.id':'edu.id'})
	xseg['edu.id'] = xseg['conv'] + "." + xseg['spk'] + ".edu." + xseg['sid'].astype(str)

	xsegshort = xseg[['conv','spk','part','sid','chno','starttime','endtime','edu.id','wavfile']] 
	xsegshort = xsegshort[pd.notnull(xsegshort['starttime'])]
	xsegshort.to_csv(options.outfile, sep=" ", index=False)

	#print "---XSEG------------------"	
	#print xseg.head()
		
	if options.metafile:
	#	print "---META------------------"	
		meta = pd.read_csv(options.metafile, sep="\t")
		meta = meta.rename(columns={'edu.id':'sid','text':'fulltext'})
		meta = meta[meta['istext']==True]
	#	print meta.head()
		xseg = pd.merge(meta, xseg, on='sid', how='left')

	xseg = xseg.drop(['value','istext'], axis=1)

	fvals = ["sid", "edu.id", "pid", "depth", "prevdepth", "sibno", "parent","starttime", "endtime", 
		"parent.id", "spk", "part", "conv", "wavfile", "chno",
		"fulltext","trans"]

	xseg = xseg[fvals]
	xseg.to_csv(options.outfile + ".full.txt", sep="\t", index=False,na_rep="NA")



	#######################################################################

if __name__ == "__main__":
        sys.exit(main())


