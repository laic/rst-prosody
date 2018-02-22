#!/bin/bash



RSCRIPTS=./
ALTCONV=0001
RSTDIR=/disk/data3/clai/data/ted-rst/
WAVDIR=/disk/data3/clai/data/ted-rst/wav/
DATADIR=/disk/data3/clai/data/ted-rst/derived
SPURTSFILE=/disk/data3/clai/data/ted-rst/derived/alignseg/${ALTCONV}.alignseg.txt
#ALTCONV=$1  ## the conversation name e.g. 0001


TEMP=`getopt -o -i: --long conv:,rscripts:,rstdir:,aligndir:,outdir:,spurtsfile:,filter: -n 'rst-prosdy.sh' -- "$@"`
eval set -- "$TEMP"

# extract options and their arguments into variables.
while true ; do
    case "$1" in
        --rscripts)      RSCRIPTS="$2"; shift 2;;
        --rstdir)        RSTDIR="$2"; shift 2;;
        --wavdir)        WAVDIR="$2"; shift 2;;
        --workdir)       DATADIR="$2"; shift 2;;
        --spurtsfile)    SPURTSFILE="$2"; shift 2;;
        --conv)          ALTCONV="$2"; shift 2;;
        --) shift ; break ;;
        *) echo "Unknown option?" ; echo $1 ; exit 1 ;;
    esac
done

echo "XXXX prosody/proc-conv-rst.sh"

## Output here
SEGSDIR=$DATADIR/segs/
mkdir -p $SEGSDIR
mkdir -p $SEGSDIR/f0
mkdir -p $SEGSDIR/i0

## Get the raw frame level features from praat 
## We need some initial alignment to base praat parameter adjustments on 
## This should be something like a spurt or a phonological phrase

echo "*** raw pros***" 
echo SPURTSFILE = $SPURTSFILE
./extract-spurt-feats.sh $SPURTSFILE $DATADIR/prosfeats/ $WAVDIR  #> feat.log.txt

## Do speaker normalization
echo "*** normalize ***"

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
## F0 
Rscript $RSCRIPTS/get-pros-norm.r $CONV f0 $SEGSDIR $SPURTSFILE
## Intensity
Rscript $RSCRIPTS/get-pros-norm.r $CONV i0 $SEGSDIR $SPURTSFILE


## TO DO: Extend to add other features

## Get aggregate features over various segment size
echo "*** word aggs***"
WORDFILE=$DATADIR/alignword/$ALTCONV.alignword.txt
Rscript $RSCRIPTS/get-pros-window.r $CONV f0 $SEGSDIR $WORDFILE
Rscript $RSCRIPTS/get-pros-window.r $CONV i0 $SEGSDIR $WORDFILE

echo "*** edu aggs***"
EDUFILE=$DATADIR/alignedu/$ALTCONV.alignedu.txt
Rscript $RSCRIPTS/get-pros-window.r $CONV f0 $SEGSDIR $EDUFILE
Rscript $RSCRIPTS/get-pros-window.r $CONV i0 $SEGSDIR $EDUFILE





