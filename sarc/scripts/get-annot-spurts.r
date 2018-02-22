## get-annot-spurts.r: 
## Usage: Rscript get-annot-spurts.r filename

library(data.table)
source("../scripts/basics.r")

########################################################
# Get command line arguments
args=(commandArgs(TRUE))
print(args)
if(length(args)==0){ stop("No arguments supplied. Exiting.") }

########################################################

## Read in the interval times:
filename <- args[1]
print(filename)
x <- data.table(read.table(filename, header=F))
setnames(x, c("tier", "label", "starttime", "endtime"))
#x <- x[label != "_NA_"]
x <- x[order(starttime)]

## Get the conv and spk names 
conv <- strsplit(basename(filename), split="_")[[1]][1]
spk <- tail(strsplit(basename(filename), split="\\.")[[1]],2)[1]
spk <- gsub("[^0-9]", "", spk)

## Make a table compatible with inEvent prosody extraction scripts
turns <- data.table(conv=conv, spk=spk, part=spk, sid=1:nrow(x), chno=spk, vidsrc="bcts", 
		starttime=x$starttime, endtime=x$endtime, 
		niteid=paste(conv, spk, 1:nrow(x), sep="."), 
		vconv=paste(conv, "0", spk, sep=""), 
		wavfile=paste(conv, "0", spk, ".wav", sep=""), 
		vidfile=paste(conv, "0", spk, ".wav", sep="") )

outdir <- dirname(filename)
write.table(turns, file=paste(outdir, "/../turns/", conv, "0", spk, ".turns.txt", sep=""), quote=F, row.names=F, col.names=F)
