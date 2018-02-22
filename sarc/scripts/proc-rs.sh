#!/bin/bash

CONV=$1 #APPENUKENG003583
SSCRIPTS=~/majhome/work/sarc/scripts/
INEVENT=~/majhome/work/inevent/summarization/

DATADIR=~/majhome/work/sarc/data/
SEGSDIR=$DATADIR/segs/


(cd $INEVENT/ineventwrappers/
CONV=$1 #APPENUKENG003583
SSCRIPTS=~/majhome/work/sarc/scripts/
INEVENT=~/majhome/work/inevent/summarization/
RSCRIPTS=~/majhome/work/inevent/summarization/rscripts/

DATADIR=~/majhome/work/sarc/data/
SEGSDIR=$DATADIR/segs/

echo "*** rs aggs***"
WDIR=$DATADIR/rs/
WTYPE=rs
Rscript $RSCRIPTS/get-pros-window.r $CONV f0 $SEGSDIR $WDIR $WTYPE
Rscript $RSCRIPTS/get-pros-window.r $CONV i0 $SEGSDIR $WDIR $WTYPE

)





