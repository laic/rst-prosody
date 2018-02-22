library(data.table)
library(plyr)
#############################################################################

make.na <- function(x) {NA} 

#############################################################################

args=(commandArgs(TRUE))
print(args)
if(length(args)==0){ stop("No arguments supplied. Exiting.") }

sentfile <- args[1]
datadir <- args[2]
feature <- toupper(args[3])
segtype <- args[4]


sents  <- fread(sentfile)
#print(sents)

sents <- sents[order(wstart)]

wmeta <- c("niteid","wstart","wend","conv","participant","nxt_agent") 
fvalnames <- c(grep(feature, names(sents), value=T))
featnames <- c(wmeta, fvalnames)

#print("*** featnames:")
#print(featnames)

## Get next and previous sentence ids
nextsents <- c(sents[, tail(niteid, -1)], "NONE")
prevsents <- c("NONE", sents[, head(niteid, -1)])

currsents <- sents[,niteid]
currpara <- unlist(lapply(strsplit(currsents, split="\\."), function(x) {as.numeric(x[4])}))
nextpara <- unlist(lapply(strsplit(nextsents, split="\\."), function(x) {as.numeric(x[4])}))

sents <- data.table(sents, para.id=currpara, next.para.id=nextpara, para.change=(currpara != nextpara), 
		prev.sent.id=prevsents, next.sent.id=nextsents) 
#write.table(sents, file="/tmp/sents.txt")

## Make a line for the case where there's nothing following.
nextfeats <- sents[niteid %in% nextsents, featnames, with=F]
nofeats <- copy(nextfeats[1])
nofeats <- data.table(colwise(make.na)(nofeats))
nofeats$niteid <- "NONE"
nextfeats <- rbindlist(list(nextfeats, nofeats))

setnames(nextfeats, names(nextfeats), gsub("^", "next.", names(nextfeats)))
#print(nextfeats)

## Join in 
setkey(sents, next.sent.id)
setkey(nextfeats, next.niteid)
nextsents <- sents[nextfeats][!is.na(niteid)][order(wstart)]


nextdiffs <- nextsents[,list(niteid, wstart, wend, pause.dur=next.wstart-wend)]

for (val in fvalnames) {
	#print(val)
	nval <- paste("next", val, sep=".")
	dval <- nextsents[[nval]] - nextsents[[val]]
	nextdiffs <- data.table(nextdiffs, dval=dval)
	setnames(nextdiffs, "dval", paste("ndiff", val,sep="."))
}

setkey(nextdiffs, niteid, wstart, wend)
setkey(nextsents, niteid, wstart, wend)
sentdiffs <- nextsents[nextdiffs]


####################################
prevfeats <- sents[niteid %in% prevsents, featnames, with=F]
nofeats <- copy(prevfeats[1])
nofeats <- data.table(colwise(make.na)(nofeats))
nofeats$niteid <- "NONE"
prevfeats <- rbindlist(list(nofeats, prevfeats))

setnames(prevfeats, names(prevfeats), gsub("^", "prev.", names(prevfeats)))

## Join in 
setkey(sentdiffs, prev.sent.id)
setkey(prevfeats, prev.niteid)
prevsents <- sentdiffs[prevfeats][!is.na(niteid)][order(wstart)]


prevdiffs <- prevsents[,list(niteid, wstart, wend, pause.dur=prev.wstart-wend)]

for (val in fvalnames) {
#	print(val)
	nval <- paste("prev", val, sep=".")
	dval <- prevsents[[nval]] - prevsents[[val]]
	prevdiffs <- data.table(prevdiffs, dval=dval)
	setnames(prevdiffs, "dval", paste("pdiff", val,sep="."))
}

setkey(prevdiffs, niteid, wstart, wend)
setkey(prevsents, niteid, wstart, wend)
sentdiffs <- prevsents[prevdiffs]

#print(names(sentdiffs))

outfile <- paste(datadir, "/segs/", tolower(feature), "-diff-", segtype, "/", basename(sentfile), ".", segtype, "diff.txt", sep="")  
print(outfile)
write.table(sentdiffs[order(wstart)], file=outfile, quote=F, row.names=F, col.names=T) 

#print(sentdiffs[order(wstart),list(niteid, para.id, next.para.id, para.change,  pause.dur, mean.normI0, pdiff.mean.normI0)][1:20])
#print(summary(lastword))



