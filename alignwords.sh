#!/bin/bash

INDIR=/disk/data3/clai/data/ted-rst/RST/
ALIGNDIR=/disk/data3/clai/data/ted-rst/txt-sent/
OUTDIR=/disk/data3/clai/data/ted-rst/rst-align/
FILTER=""

TEMP=`getopt -o -i: --long indir:,aligndir:,outdir:,filter: -n 'rst-prosdy.sh' -- "$@"`
eval set -- "$TEMP"

# extract options and their arguments into variables.
while true ; do
    case "$1" in
        --indir)        INDIR="$2"; shift 2;;
        --aligndir)     ALIGNDIR="$2"; shift 2;;
        --outdir)       OUTDIR="$2"; shift 2;;
        --filter)       FILTER="$2"; shift 2;;
        --) shift ; break ;;
        *) echo "Unknown option?" ; echo $1 ; exit 1 ;;
    esac
done



mkdir -p $OUTDIR

for WORDFILE in `ls $INDIR/*$FILTER*.edu.words.txt `
do
	ALTCONV=`basename $WORDFILE .edu.words.txt | sed 's/ParsedProc//'`	

	## Use the existing alignment
	IDFILE=$ALIGNDIR/$ALTCONV.word.txt.norm
	echo $WORDFILE $ALTCONV $IDFILE

	python levalign.py --file $WORDFILE  --outdir=$OUTDIR  --idfile=$IDFILE
done
