#!/bin/bash

RSCRIPTS=./

conv=$1 #0001
feat=$2
segtype=$3 #edu  
datadir=$4 #~/lubbock/data/ted-trans/derived/
echo $datadir
featdir=$datadir/segs/$feat/

mkdir -p $datadir/segs/$feat-boundary-$segtype

sentfile=$featdir/$conv.aggs.align$segtype.txt
wordfile=$featdir/$conv.aggs.alignword.txt

Rscript $RSCRIPTS/get-last-word-pros.r $sentfile $wordfile $datadir $feat $segtype

