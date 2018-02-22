#!/bin/bash
conv=$1 #0001
feat=$2 #F0
segtype=$3
datadir=$4 #~/data/ted-trans/derived/

featdir=$datadir/segs/$feat/
sentfeat=$featdir/$conv.aggs.align$segtype.txt

mkdir -p $datadir/segs/$feat-diff-$segtype

Rscript ./get-sent-diffs.r $sentfeat $datadir $feat $segtype

