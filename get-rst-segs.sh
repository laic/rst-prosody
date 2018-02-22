#!/bin/bash


INDIR=/disk/data3/clai/data/ted-rst/rst-align/
ALIGNDIR=/disk/data3/clai/data/ted-rst/derived/alignword/
METADIR=/disk/data3/clai/data/ted-rst/RST/
OUTDIR=/disk/data3/clai/data/ted-rst/derived/alignedu
METAPREFIX=ParsedProc
FILTER=""

TEMP=`getopt -o -i: --long indir:,aligndir:,outdir:,metadir:,filter: -n 'rst-prosdy.sh' -- "$@"`
eval set -- "$TEMP"

# extract options and their arguments into variables.
while true ; do
    case "$1" in
        --indir)        INDIR="$2"; shift 2;;
        --aligndir)     ALIGNDIR="$2"; shift 2;;
        --metadir)      METADIR="$2"; shift 2;;
        --outdir)       OUTDIR="$2"; shift 2;;
        --metaprefix)   METAPREFIX="$2"; shift 2;;
        --filter)       FILTER="$2"; shift 2;;
        --) shift ; break ;;
        *) echo "Unknown option?" ; echo $1 ; exit 1 ;;
    esac
done


mkdir -p $OUTDIR

for EDUFILE in `ls $INDIR/*$FILTER*edu.words.txt.wid*` 
do
	ALTCONV=`basename $EDUFILE .edu.words.txt.wid`
	ALIGNFILE=$ALIGNDIR/$ALTCONV.alignword.txt
	OUTFILE=$OUTDIR/$ALTCONV.alignedu.txt
	METAFILE=$METADIR/$METAPREFIX$ALTCONV.edu.txt
	echo $ALTCONV $ALIGNFILE $OUTFILE $METAFILE
	python get-rst-segs.py --file=$EDUFILE --alignfile=$ALIGNFILE --outfile=$OUTFILE --metafile=$METAFILE
done
