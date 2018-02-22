#!/bin/bash

SEGTYPE=$1
DATADIR=$2 #/disk/data3/clai/data/ted-rst/
prefix=$3 

PROSDIR=$DATADIR/derived/segs/i0-diff-$SEGTYPE
OUTDIR=$DATADIR/derived/segs/merged-$SEGTYPE

mkdir -p $OUTDIR

for file in `ls $PROSDIR/*$prefix*`
do
	conv=`basename $file | cut -d "." -f 1`
	outfile=$OUTDIR/$conv.align$SEGTYPE.allpros.txt
	echo $conv $outfile
	echo "python collate-pros-feats.py --conv=$conv --outfile=$outfile --segtype=$SEGTYPE"
	python collate-pros-feats.py --conv=$conv --outfile=$outfile --segtype=$SEGTYPE --datadir=$DATADIR/derived/segs
done
