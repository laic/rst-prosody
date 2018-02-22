#!/bin/bash

ALTCONV=$1 
SEGTYPE=$2

PROSODY=~/prosody/
RSCRIPTS=$PROSODY/

DATADIR=~/lubbock/data/ted-trans/derived/
SEGSDIR=$DATADIR/segs/
mkdir -p $SEGSDIR




## Get aggregate features over given segment 
echo "*** seg aggs***"
SEGFILE=$DATADIR/$SEGTYPE/$ALTCONV.$SEGTYPE.txt


#CONV=`head -n 2 $SEGFILE | tail -n 1 | cut -d " " -f 1`
#echo $ALTCONV $CONV


Rscript $RSCRIPTS/get-pros-window.r $ALTCONV f0 $SEGSDIR $SEGFILE
Rscript $RSCRIPTS/get-pros-window.r $ALTCONV i0 $SEGSDIR $SEGFILE

#)




