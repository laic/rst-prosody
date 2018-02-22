#!/bin/bash

ALTCONV=$1 # e.g. APPENUKENG003583, should match $ALTCONV.alignseg.txt
DATADIR=$2 #~/lubbock/data/ted-trans/derived/
#DATADIR=~/lubbock/data/ted-trans/derived/

PROSODY=~/lubbock/prosody/
RSCRIPTS=$PROSODY

SEGSDIR=$DATADIR/segs/

mkdir -p $SEGSDIR
mkdir -p $SEGSDIR/f0
mkdir -p $SEGSDIR/i0


echo "prosody/proc-conv.sh"
echo "LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
echo "DATADIR: $DATADIR"


## Get the raw frame level features from praat 
echo "*** raw pros***"
./extract-spurt-feats.sh "$DATADIR/alignseg/${ALTCONV}.alignseg.txt" $DATADIR/prosfeats/ $DATADIR/wav/  #> feat.log.txt


## Do speaker normalization
echo "*** normalize ***"
SPURTSFILE=$DATADIR/alignseg/$ALTCONV.alignseg.txt
if [ -h $SEGSDIR/conv ]
then
	echo "**** $SEGSDIR/conv EXISTS"  
	rm $SEGSDIR/conv
	ln -s $DATADIR/prosfeats $SEGSDIR/conv 
else
	
	echo "**** $SEGSDIR/conv DOESN'T EXIST" 
	ln -s $DATADIR/prosfeats $SEGSDIR/conv 

fi

CONV=`head -n 2 $SPURTSFILE | tail -n 1 | cut -d " " -f 1`
echo $ALTCONV $CONV
Rscript $RSCRIPTS/get-pros-norm.r $CONV f0 $SEGSDIR $SPURTSFILE
Rscript $RSCRIPTS/get-pros-norm.r $CONV i0 $SEGSDIR $SPURTSFILE


## Extend to add other features

## Get aggregate features over various segment size
echo "*** word aggs***"
WORDFILE=$DATADIR/alignword/$ALTCONV.alignword.txt
Rscript $RSCRIPTS/get-pros-window.r $CONV f0 $SEGSDIR $WORDFILE
Rscript $RSCRIPTS/get-pros-window.r $CONV i0 $SEGSDIR $WORDFILE

#SEGFILE=$DATADIR/alignseg/$ALTCONV.alignseg.txt
#Rscript $RSCRIPTS/get-pros-window.r $CONV f0 $SEGSDIR $SEGFILE
#Rscript $RSCRIPTS/get-pros-window.r $CONV i0 $SEGSDIR $SEGFILE

SENTFILE=$DATADIR/alignsent/$ALTCONV.alignsent.txt
Rscript $RSCRIPTS/get-pros-window.r $CONV f0 $SEGSDIR $SENTFILE
Rscript $RSCRIPTS/get-pros-window.r $CONV i0 $SEGSDIR $SENTFILE




