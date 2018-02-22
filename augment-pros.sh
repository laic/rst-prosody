#!/bin/bash



RSTDIR=/disk/data3/clai/data/ted-rst/
PROSDIR=$RSTDIR/derived/segs/merged-edu
JSONDIR=$RSTDIR/RST/
METADIR=$RSTDIR/derived/alignedu/
OUTDIR=$RSTDIR/derived/segs/merged-edu-meta
JSONPREF=ParsedProc

FILTER=0001
featfile=$RSTDIR/featset/edu-pros.txt


TEMP=`getopt -o -i: --long datadir:,jsonpref:,prosdir:,jsondir:,metadir:,outdir:,featfile:,filter: -n 'augment-pros.sh' -- "$@"`
eval set -- "$TEMP"

# extract options and their arguments into variables.
while true ; do
    case "$1" in
        --datadir)      INDIR="$2"; shift 2;;
        --prosdir)      PROSDIR="$2"; shift 2;;
        --jsondir)      JSONDIR="$2"; shift 2;;
        --metadir)      METADIR="$2"; shift 2;;
        --outdir)       OUTDIR="$2"; shift 2;;
        --featfile)     featfile="$2"; shift 2;;
        --jsonpref)     JSONPREF="$2"; shift 2;;
        --filter)       FILTER="$2"; shift 2;;
        --) shift ; break ;;
        *) echo "Unknown option?" ; echo $1 ; exit 1 ;;
    esac
done


mkdir -p $OUTDIR

for file in `ls $PROSDIR/*$FILTER*.alignedu.allpros.txt`
do
	echo "-------------------------"
	conv=`basename $file | cut -d "." -f 1`
	fsetname=`basename $featfile .txt`
	outfile=$OUTDIR/$conv.alignedu.$fsetname.meta.txt	
	jsonfile=$JSONDIR/${JSONPREF}${conv}.rst.json
	metafile=$METADIR/$conv.alignedu.txt.full.txt

	echo $conv		
	echo $file 
	echo $outfile
	echo $jsonfile
	echo $metafile
	python augment-pros.py --infile=$file --outfile=$outfile  --jsonfile=$jsonfile --metafile=$metafile --featfile=$featfile
done
