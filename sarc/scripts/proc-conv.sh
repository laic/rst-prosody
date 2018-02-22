#!/bin/bash
## proc-conv.sh: A wrapper for processing BCTS conversations for 
## quotation analysis
## USAGE: proc-conv.sh CONV

CONV=$1 # e.g. APPENUKENG003583
SSCRIPTS=~/majhome/work/sarc/scripts/

## Using previous feature extraction code from the inEvent project 
INEVENT=~/majhome/work/inevent/summarization/
DATADIR=~/majhome/work/sarc/data/
# Prosodic features end up here:
SEGSDIR=$DATADIR/segs/


echo "*** flatten textgrids ***"
python $SSCRIPTS/TextGrid.py $DATADIR/labels/${CONV}_BoundariesSamples.TextGrid

## Convert turns and words info into a format compatible with prosody extraction scripts 

echo "*** turns ***"
Rscript $SSCRIPTS/get-annot-spurts.r $DATADIR/labels/${CONV}_BoundariesSamples.TextGrid.Turns_1.txt
echo "*** words ***"
Rscript get-words.r ../data/labels/${CONV}_BoundariesSamples.TextGrid.Channel_1.txt 

Rscript get-word-meta.r ../data/words/${CONV}01.words.txt ../data/labels/${CONV}_BoundariesSamples.TextGrid.AR_1.txt ../data/turns/${CONV}01.turns.txt 

## Similarly for the other side:
if [ $CONV != "APPENUKENG004552" ]
then
	echo "*** side 2 ***"
	Rscript $SSCRIPTS/get-annot-spurts.r $DATADIR/labels/${CONV}_BoundariesSamples.TextGrid.Turns_2.txt
	Rscript get-words.r ../data/labels/${CONV}_BoundariesSamples.TextGrid.Channel_2.txt 
	Rscript get-word-meta.r ../data/words/${CONV}02.words.txt ../data/labels/${CONV}_BoundariesSamples.TextGrid.AR_2.txt ../data/turns/${CONV}02.turns.txt 
fi

## Put all the turns from the conv together
cat $DATADIR/turns/${CONV}*.turns.txt > $DATADIR/turns/$CONV.turns.all.txt
Rscript ~/majhome/work/sarc/scripts/collate-words.r $CONV ~/majhome/work/sarc/data/words/

## Do prosody stuff in a subshell because of relative path dependencies in the code.
## Features ended up n ../data/segs/f0 for pitch ../data/segs/i0 for intensity 
(cd $INEVENT/ineventwrappers/
CONV=$1 #APPENUKENG003583
SSCRIPTS=~/majhome/work/sarc/scripts/
INEVENT=~/majhome/work/inevent/summarization/

DATADIR=~/majhome/work/sarc/data/
SEGSDIR=$DATADIR/segs/

echo "*** extract raw pros***"
../bashscripts/extract-spurt-feats.sh "$DATADIR/turns/${CONV}.turns.all.txt" $DATADIR/prosfeats/ $DATADIR/wav/ > feat.log.txt

RSCRIPTS=$INEVENT/rscripts/
echo "*** normalize ***"
SPURTSFILE=$DATADIR/turns/$CONV.turns.all.txt
Rscript $RSCRIPTS/get-pros-norm.r $CONV f0 $SEGSDIR $SPURTSFILE
Rscript $RSCRIPTS/get-pros-norm.r $CONV i0 $SEGSDIR $SPURTSFILE

echo "*** word aggs***"
WDIR=$DATADIR/words/
WTYPE=words
Rscript $RSCRIPTS/get-pros-window.r $CONV f0 $SEGSDIR $WDIR $WTYPE
Rscript $RSCRIPTS/get-pros-window.r $CONV i0 $SEGSDIR $WDIR $WTYPE

)





