library(data.table)
library(plyr)
#############################################################################

get.nwords <- function(x, wordfeats, eps=0.0001) {
	words <- wordfeats[wstart > (x$starttime-eps)][wend < (x$endtime+eps)]
	return(data.table(nwords=nrow(words)))

} 

get.last.word <- function(x, wordfeats, eps=0.0001) {
	lastword <- wordfeats[wstart > (x$starttime-eps)][wend < (x$endtime+eps)][order(wstart, decreasing=T)][1]
	return(lastword)

} 

get.first.word <- function(x, wordfeats, eps=0.0001) {
	lastword <- wordfeats[wstart > (x$starttime-eps)][wend < (x$endtime+eps)][order(wstart, decreasing=F)][1]
	return(lastword)

} 
make.na <- function(x) {NA} 

#############################################################################

args=(commandArgs(TRUE))
print(args)
if(length(args)==0){ stop("No arguments supplied. Exiting.") }

sentfile <- args[1]
wordfile <- args[2]
datadir <- args[3]
feature <- toupper(args[4])
segtype <- args[5]


print("feature:")
print(feature)
print("segtype:")
print(segtype)

## Get word level features
wordfeats0  <- fread(wordfile)


## Remove words with no id 
if (feature == "I0") {
	wordfeats <- wordfeats0[!is.na(niteid)]#[!is.na(mean.normI0)]
}  else {
	wordfeats <- wordfeats0[!is.na(niteid)]#[!is.na(mean.normF0)]
}

## Get segment features, e.g. sentences
sents0  <- fread(sentfile)
sents <- sents0[,list(sent.id=niteid, starttime=wstart, endtime=wend),]

## Get ids of previous and next sentences
sents <- sents[order(starttime)]
nextsents <- c(sents[, tail(sent.id, -1)], "NONE")
prevsents <- c("NONE", sents[, head(sent.id, -1)])
sents <- data.table(sents, prev.sent.id=prevsents, next.sent.id=nextsents) 
#write.table(sents, file="/tmp/sents.txt")

## Count words
nwords <- data.table(ddply(sents, .(sent.id), get.nwords, wordfeats=wordfeats0))

## Find first and last words of sentences
lastword <- data.table(ddply(sents, .(sent.id, starttime, endtime, prev.sent.id, next.sent.id), get.last.word, wordfeats=wordfeats))
firstword <- data.table(ddply(sents, .(sent.id, starttime, endtime, prev.sent.id, next.sent.id), get.first.word, wordfeats=wordfeats))

## Get feature names
wmeta <- c("niteid","wstart","wend","conv","participant","nxt_agent") 
fvalnames <- c(grep(feature, names(firstword), value=T))
featnames <- c(wmeta, fvalnames)

#print(featnames)
## Set prefixes for feature names
setnames(firstword, featnames, paste("first.", featnames, sep="") )
setnames(lastword, featnames, paste("last.", featnames, sep="") )

## Join together info on first and last words for the current sentence
setkey(lastword, sent.id, starttime, endtime, prev.sent.id, next.sent.id)
setkey(firstword, sent.id, starttime, endtime, prev.sent.id, next.sent.id)
firstlast <- firstword[lastword]

## Find segment initial words, these are the first "next" words
firstfeats <- firstword[,c("sent.id", paste("first.", featnames, sep="")), with=F]
setnames(firstfeats, names(firstfeats), gsub("^first", "next", names(firstfeats)))

## Find last word before a sentence boundary, label prev
lastfeats <- lastword[,c("sent.id", paste("last.", featnames, sep="")), with=F]
setnames(lastfeats, names(lastfeats), gsub("^last", "prev", names(lastfeats)))

## Make a line for the case where there's nothing following or preceding.
## Following 
nofeats <- copy(firstfeats[1])
nofeats <- data.table(colwise(make.na)(nofeats))
nofeats$sent.id <- "NONE"
firstfeats <- rbindlist(list(firstfeats, nofeats))

## Preceding
nofeats <- copy(lastfeats[1])
nofeats <- data.table(colwise(make.na)(nofeats))
nofeats$sent.id <- "NONE"
lastfeats <- rbindlist(list(nofeats, lastfeats))

## Join feats in: get first word features for the next sentence 
setkey(firstlast, next.sent.id)
setkey(firstfeats, sent.id)
nextwords0 <- firstlast[firstfeats][!is.na(sent.id)][order(starttime)]

## Get last word features for the previous sentence
setkey(nextwords0, prev.sent.id)
setkey(lastfeats, sent.id)
nextwords <- nextwords0[lastfeats][!is.na(sent.id)][order(starttime)]

## Now calculate difference features
lndiffs <- nextwords[,list(sent.id, starttime, endtime, pause.dur=next.wstart-last.wend)]

prevfeats <- grep("^prev", names(nextwords), value=T)
nextfeats <- grep("^next", names(nextwords), value=T)
#print(c(prevfeats, nextfeats))

lndiffs <- data.table(lndiffs, nextwords[,c(prevfeats, nextfeats), with=F])
setnames(lndiffs, c("prev.niteid", "next.niteid"), c("prev.id", "next.id"))
setnames(lndiffs, names(lndiffs), gsub("^prev", "prev.word", names(lndiffs)))
setnames(lndiffs, names(lndiffs), gsub("^next", "next.word", names(lndiffs)))
#print(names(lndiffs))
#print(names(nextwords))

for (val in fvalnames) {
#	print(val)
	fval <- paste("first", val, sep=".")
	lval <- paste("last", val, sep=".")
	nval <- paste("next", val, sep=".")
	pval <- paste("prev", val, sep=".")

	dflval <- nextwords[[lval]] - nextwords[[fval]]
	dlnval <- nextwords[[nval]] - nextwords[[lval]]
	dpfval <- nextwords[[fval]] - nextwords[[pval]]


	lndiffs <- data.table(lndiffs, dpfval=dpfval, dflval=dflval, dlnval=dlnval)
	setnames(lndiffs, "dlnval", paste("lndiff", val,sep="."))
	setnames(lndiffs, "dflval", paste("fldiff", val,sep="."))
	setnames(lndiffs, "dpfval", paste("pfdiff", val,sep="."))
}

setkey(lndiffs, sent.id, starttime, endtime)
setkey(firstlast, sent.id, starttime, endtime)
fldwords <- firstlast[lndiffs]

setkey(fldwords, sent.id)
setkey(nwords, sent.id)
fldwords <- fldwords[nwords]

outfile <- paste(datadir, "/segs/", tolower(feature), "-boundary-", segtype, "/", basename(sentfile), ".boundary.", segtype, ".txt", sep="")  
print(outfile)
write.table(fldwords[order(starttime)], file=outfile, quote=F, row.names=F, col.names=T) 


#print(fldwords[,list(sent.id, prev.sent.id, next.sent.id, pfdiff.mean.normF0, first.mean.normF0, last.mean.normF0)])



