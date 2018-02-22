#!/bin/bash

INDIR=/disk/data3/clai/data/ted-trans/TedFastNLPProcessor/
OUTDIR=/disk/data3/clai/data/ted-rst/RST
FILTER=""

TEMP=`getopt -o -i: --long indir:,outdir:,filter: -n 'rst-prosdy.sh' -- "$@"`
eval set -- "$TEMP"

# extract options and their arguments into variables.
while true ; do
    case "$1" in
        --indir)   	INDIR="$2"; shift 2;;
        --outdir)       OUTDIR="$2"; shift 2;;
        --filter)       FILTER="$2"; shift 2;;
        --) shift ; break ;;
        *) echo "Unknown option?" ; echo $1 ; exit 1 ;;
    esac
done



mkdir -p $OUTDIR

for file in `ls $INDIR/Parsed*$FILTER.txt`
do
	echo $file
	python proc-rst-parse.py --input=$file --outdir=$OUTDIR
done
