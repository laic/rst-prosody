## get-word-meta.r: aggregate word, turn and attribution info into a 
## flat table.


library(data.table)
library(plyr)
library(reshape2)
library(intervals)
source("../scripts/basics.r")

get.overlap.size <- function(x1,x2,y1,y2) {
	size(interval_intersection(Intervals_full(matrix(c(x1,x2,y1,y2), ncol=2, byrow=T))))
}


#############################################################################
# MAIN
#############################################################################
args=(commandArgs(TRUE))
print(args)
if(length(args) < 3){ stop("Not enough arguments supplied. Exiting.") }

#wordfile <- "../data/words/APPENUKENG00621301.words.txt"
#rsfile <- "../data/labels/APPENUKENG006213_BoundariesSamples.TextGrid.AR_1.txt"
#turnfile <- "../data/turns/APPENUKENG00621301.turns.txt"

wordfile <- args[1]  ## Word timing
rsfile <- args[2]    ## Attribution info
turnfile <- args[3]  ## Turn timing

#-------------------------------------------------------------------
# Input
#-------------------------------------------------------------------
words <- data.table(read.table(wordfile, header=T))

x <- data.table(read.delim(rsfile, header=F, sep="\t"))
setnames(x, c("tier", "label", "starttime", "endtime"))
x <- x[label != "_NA_"][order(starttime)]

conv <- strsplit(basename(rsfile), split="_")[[1]][1]
spk <- tail(strsplit(basename(rsfile), split="\\.")[[1]],2)[1]
spk <- gsub("[^0-9]", "", spk)

rspeech <- data.table(conv=conv, spk=spk, 
                niteid=paste(conv, ".", spk, ".rs", 1:nrow(x), sep=""), x)

#---------------------------------------------------------------------
## Get words in Reported speech intervals
wint <- Intervals_full(as.matrix(words[,list(wstarts,wends)]), closed=F)
rsints <- Intervals_full(as.matrix(rspeech[,list(starttime,endtime)]), closed=F)
warint <- interval_overlap(rsints, wint)
names(warint) <- rspeech$niteid

## Get associated word ids
rs.words <- data.table(ldply(warint, function(x) {words[x,list(wid=niteid, spk, wstarts,wends,word)]}))
setkey(rspeech, niteid)
setkey(rs.words, .id)
v <- rspeech[rs.words]
v <- data.table(v, v[,list(ws=starttime-wstarts, we=endtime-wends)])

## find the words at RS boundaries (intervals on different intervals are not
## strictly aligned.
  
v.bound <- v[,list(
		start.wid=wid[which.min(abs(ws))], start.word=word[which.min(abs(ws))], 
		start.wstart=wstarts[which.min(abs(ws))], 
		start.wend=wends[which.min(abs(ws))], 
		end.wid=wid[which.min(abs(we))], end.word=word[which.min(abs(we))], end.wstart=wstarts[which.min(abs(we))], 
		end.wend=wends[which.min(abs(ws))] 
		),by=list(niteid, label, starttime, endtime)][order(starttime)]

## Add info to words:
words <- data.table(words, words[, list(is.rsstart=(niteid %in% v.bound$start.wid), is.rsend=(niteid %in% v.bound$end.wid))])
setnames(words, "niteid", "wid")
words <- data.table(words, rid="", label="", rstart=-1.0, rend=-1.0)
u <- v.bound[,list(sind=which(words$wid == start.wid), eind=which(words$wid == end.wid)), by=list(niteid,label,starttime,endtime)]

for (i in 1:nrow(u)) {
	words[u$sind[i]:u$eind[i]]$rid <- u$niteid[i]
	words[u$sind[i]:u$eind[i]]$label <- u$label[i]
	words[u$sind[i]:u$eind[i]]$rstart <- u$starttime[i]
	words[u$sind[i]:u$eind[i]]$rend <- u$endtime[i]
}

write.table(words[order(wstarts)], file=paste(wordfile, ".rs", sep=""))

#-------------------------------------------------------------------
turns <- data.table(read.table(turnfile, header=F))
turns <- turns[,list(conv=V1,spk=V2, starttime=V7, endtime=V8, tid=V9)]

wint <- Intervals_full(as.matrix(words[,list(wstarts,wends)]), closed=F)
tints <- Intervals_full(as.matrix(turns[,list(starttime,endtime)]), closed=F)
twint <- interval_overlap(tints, wint)
names(twint) <- turns$tid

turn.words <- data.table(ldply(twint, function(x) {words[x]}))
setkey(turns,tid)
setkey(turn.words, .id)
v <- turns[turn.words]

extra.turns <- v[,list(nturns=length(tid), tid, starttime, endtime, wstarts, wends),by=wid][nturns > 1]

if (nrow(extra.turns) > 0) {
	exclude.turns <- extra.turns[,get.overlap.size(starttime, endtime, wstarts, wends), by=list(wid, tid)][,list(tid=tid[V1 < max(V1)]),by=list(wid)]

	for (i in 1:nrow(exclude.turns)) {
		v <- v[!(wid==exclude.turns[i]$wid & tid==exclude.turns[i]$tid)]
	}
}
write.table(v[order(wstarts)], file=paste(wordfile, ".rs.tid", sep=""))

print("*****words with no turns****")
print(words[!(wid %in% v$wid)])

print("*** END ***")



