#!/bin/bash

## There was some weird alternate name business in TED that I never got 
## round to fixing properly.  You can definitely avoid this!
ALTCONV=0001
CONV="AlGore_2006"

TEDDIR=/disk/data3/clai/data/ted-trans/
RSTDIR=/disk/data3/clai/data/ted-rst/
PROSSCRIPTS=./prosody/
WORKDIR=$RSTDIR/derived/

## 1) RST hierachical format (FastNLP) to flat word file  
## !!! You'll need to change this to deal with other formats !!!
## output = RST/ALTCONV.edu.words.txt
#
## TODO: This throws a lot of pandas warnings at the moment.  
#  It's not actually causing errors but maybe fix the syntax so the warnings go
#  away?
./proc-rst-parse.sh --indir=$TEDDIR/TedFastNLPProcessor/ --outdir=$RSTDIR/RST --filter=$ALTCONV


## 2) Match words in EDUs to word level timings
## this uses an existing word alignment txt-sent/ALTCONV.word.txt.norm
## You'll want to do something with your TextGrids
## output = rst-align/ALTCONV.edu.words.txt.wid
./alignwords.sh --indir=$RSTDIR/RST/ --outdir=$RSTDIR/rst-align/ --aligndir=$RSTDIR/txt-sent/ --filter=$ALTCONV 

## 3) Now get edu timings 
## output = derived/alignedu/X.alignedu.txt
./get-rst-segs.sh --filter=0001


## 4) Extract aggregate prosodic features the normal way (whatever that is!)

(cd $PROSSCRIPTS
echo `pwd`
./proc-conv-rst.sh --conv=$ALTCONV 
)



# 5) Get difference and boundary features
## Old naming problem with TED, try to avoid this with a new dataset

## 5a) Get sentence differences #./get-sent-diffs.sh $CONV f0 edu $WORKDIR
./get-sent-diffs.sh $CONV f0 edu $WORKDIR
./get-sent-diffs.sh $CONV i0 edu $WORKDIR

## 5b) Get boundary word differences
## Note this will find you the first word that has an timing alignment 
## There may be unaligned words that are missed.

./get-last-word.sh $CONV f0 edu $WORKDIR
./get-last-word.sh $CONV i0 edu $WORKDIR

## 6) Now collect everything together
## output = derived/segs/merged-edu/$CONV.alignedu.allpros.txt

./collate-pros-feats.sh edu $RSTDIR $CONV 

## 6b) Deal with the wacky name in our TED data

cp $WORKDIR/segs/merged-edu/$CONV.alignedu.allpros.txt $WORKDIR/segs/merged-edu/$ALTCONV.alignedu.allpros.txt 

## 7) Put the RST relations and other metadata back in.
## The file you want is probably: 
#  $RSTDIR//derived/segs/merged-edu-meta/0001.alignedu.edu-pros.meta.txt
## This also produces a .json representation that you can potentially use for 
## visualizing the RST tree with javascript d3 or similar.
## The featfile options determines which features will be kept in the final output.

./augment-pros.sh --featfile=$RSTDIR/featsets/edu-pros.txt --filter=$ALTCONV



