#!/bin/bash

## Get raw F0 and intensity features using praat.  

#PRAAT=/exports/home/clai/.local/bin/praat		#/exports/home/clai/.local/bin/praat
#PRAAT=/disk/data3/clai/venv/bin/praat
PRAAT=/home/clai/bin/praat
echo "USING `which praat`"
spurtfile=$1		## Segmentation file, see for example ~/lubbock/data/ted-trans/derived/alignseg/${ALTCONV}.alignseg.txt" 
spurtdir=$2 		## This is the output directory 	
indir=$3		## i.e. where the wav files are    

echo "EXTRACT FEATS" 
#echo $LD_LIBRARY_PATH

conv=`tail -n1 $spurtfile | cut -d " " -f 1` 
outdir="$spurtdir/$conv/"
echo "outdir: $outdir "

if [  -e $outdir ]
then
	echo "removing $outdir"
	rm -rf $outdir
fi

mkdir -p $outdir
mkdir -p $outdir/$conv-f0
mkdir -p $outdir/$conv-int

tail -n +2 $spurtfile |
while read line
do
	conv=`echo $line | cut -d " " -f 1`
	spk=`echo $line | cut -d " " -f 2`
	part=`echo $line | cut -d " " -f 3`
	sid=`echo $line | cut -d " " -f 4`
	chno=`echo $line | cut -d " " -f 5`
	#vidsrc=`echo $line | cut -d " " -f 6`
	start=`echo $line | cut -d " " -f 6`
	end=`echo $line | cut -d " " -f 7`
	niteid=`echo $line | cut -d " " -f 8`
	wavfile=`echo $line | cut -d " " -f 9`
	outfile=$niteid

	#echo  "./extract-feats.praat $wavfile $outfile $start $end $indir $outdir $conv"
	$PRAAT  ./extract-feats.praat $wavfile $outfile $start $end $indir $outdir $conv 

done  

echo "extract-spurt-feats done"


#exit 0
