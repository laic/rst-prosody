library(data.table)
library(lme4)
library(arm)
library(ggplot2)
library(plyr)
library(LiblineaR)
library(e1071)
library(ROCR)
library(parallel)
source("../scripts/basics.r")

get.rs.spurts <- function(word.pros) {	

	rs.spurts <- word.pros[rstype!="N",list(length(niteid)),by=list(conv, xid, rid, nxt_agent, rstart, rend)]
	setnames(rs.spurts, c("rid", "nxt_agent", "rstart", "rend"), c("niteid", "spk", "wstarts", "wends"))

	for (currconv in unique(rs.spurts$conv)) {
		x.list <- dlply(rs.spurts[conv==currconv], .(xid), function(x) {x})
		save(x.list, file=paste("../data/rs/",currconv, ".conv.rs", sep="")) 
	}
	
}

get.nors.turns <- function(word.pros) {
	turns <- word.pros[,list(
			wstarts=min(wstart,na.rm=T), 
			wends=max(wstart,na.rm=T),
			num.rsword=sum(rid != "") 
			),by=list(conv, xid, tid, nxt_agent)]
	setnames(turns, c("tid", "nxt_agent"), c("niteid", "spk"))

	for (currconv in unique(turns$conv)) {
		x.list <- dlply(turns[conv==currconv], .(xid), function(x) {x})
		save(x.list, file=paste("../data/turns/",currconv, ".conv.turns", sep="")) 
	}
}


## Get various contextual features for turns
## e.g. pause durations, previous turn, previous turn with reported speech etc.
get.seg.pros.context <- function(word.pros, id.name="tid") {
	setnames(word.pros, id.name, "id")

	turn.spurts <- word.pros[,{
				nwords <- length(niteid)
				intern.pause <- 0
				if (nwords > 1) { 
					wgap <- tail(wstart[order(wstart)],-1) - head(wend[order(wstart)], -1)
					intern.pause <- sum(wgap[wgap > 0])
				}
				if (id.name != "tid") {
					first.tid <- head(tid[order(starttime)],1)
					last.tid <- tail(tid[order(starttime)],1)
				} else {
					first.tid <- id
					last.tid <- id
				}
				list(nwords=nwords, 	
				nrswords=sum(rstype != "N"),	
				intern.pause=intern.pause,
				swid=niteid[which.min(wstart)],	
				ewid=niteid[which.max(wend)],	
				starttime=min(wstart, na.rm=T),	
				endtime=max(wend, na.rm=T),	
				first.tid=first.tid, 
				last.tid=last.tid, 
				rstype=paste(unique(rstype), collapse="") 	
			)},by=list(conv, id, spk)]

	setnames(word.pros, "id", id.name)

	## Get previous and next ids
	turns.context <- turn.spurts[order(conv, starttime)][,list(id, 
			p.id=c("", head(unlevel(id),-1)), n.id=c(tail(unlevel(id),-1),"")
			),by=list(conv)]

	setkey(turn.spurts, id, conv) 
	setkey(turns.context, id, conv)

	print(length(turn.spurts[,unique(id)]))
	print(length(turns.context[,unique(id)]))
	turns.context <- turns.context[turn.spurts]	

	turns.context <- turns.context[order(conv, starttime)]

	setnames(turns.context, "id", id.name)
	setnames(turns.context, names(turns.context), gsub("\\.id", paste(".", id.name, sep=""), names(turns.context)))
 	
	return(turns.context)

}


## Collate turn level prosodic features
get.turn.pros <- function(word.pros) {

	turn.feats <- NULL
	for (conv in unique(word.pros$conv)) {
		f0file  <- paste("../data/segs/f0/", conv, ".aggs.turns.txt", sep="")
		i0file  <- paste("../data/segs/i0/", conv, ".aggs.turns.txt", sep="")

		f0 <- data.table(read.table(f0file, header=T))
		i0 <- data.table(read.table(i0file, header=T))
		setkey(f0, niteid, conv, participant, wstart, wend, nxt_agent)
		setkey(i0, niteid, conv, participant, wstart, wend, nxt_agent)

		pros <- f0[i0] 
		turn.feats <- rbindlist(list(turn.feats, pros)) 

	}

	turn.feats <- turn.feats[!is.na(mean.normI0)]
	turns.context <- get.seg.pros.context(word.pros[tid %in% unlevel(turn.feats$niteid)], id.name="tid") 
	turns.context <- turns.context[order(conv, starttime)]
	#print(turns.context)

	## Join turn prosodic features and contextual info
	setkey(turn.feats, niteid, conv)
	setkey(turns.context, tid, conv)
	turn.pros <- turns.context[turn.feats]

	## Add previous turn prosodic features
	setnames(turn.feats, "niteid", "tid")
	setnames(turn.feats, gsub("^", "x.", names(turn.feats)))
	setkey(turn.pros, p.tid)
	setkey(turn.feats, x.tid)

	turn.pros <- turn.feats[turn.pros]	
	setnames(turn.pros, gsub("^x.", "p.", names(turn.pros)))

	## Add next turn prosodic features
	setkey(turn.pros, n.tid)
	setkey(turn.feats, x.tid)
	turn.pros <- turn.feats[turn.pros]	
	setnames(turn.pros, gsub("^x.", "n.", names(turn.pros)))

	return(turn.pros)

}

## Collate prosodic features over reported speech segments 
get.rs.pros <- function(word.pros, turn.pros) {
	rs.spurts <- get.seg.pros.context(word.pros[rstype!="N"], id.name="rid") 

	rs.feats <- NULL
	for (conv in unique(word.pros$conv)) {
		f0file  <- paste("../data/segs/f0/", conv, ".aggs.rs.txt", sep="")
		i0file  <- paste("../data/segs/i0/", conv, ".aggs.rs.txt", sep="")

		f0 <- data.table(read.table(f0file, header=T))
		i0 <- data.table(read.table(i0file, header=T))
		setkey(f0, niteid, conv, participant, wstart, wend, nxt_agent)
		setkey(i0, niteid, conv, participant, wstart, wend, nxt_agent)

		pros <- f0[i0] 

		rs.feats <- rbindlist(list(rs.feats, pros)) 

	}
	setkey(rs.feats, niteid, conv)
	setkey(rs.spurts, rid, conv)
	rs.pros0 <- rs.spurts[rs.feats]

	## We actually need to join this with the full turn info
	pturn.pros <- turn.pros[,c("tid", grep("^p\\.", names(turn.pros), value=T)), with=F] 
	nturn.pros <- turn.pros[,c("tid", grep("^n\\.", names(turn.pros), value=T)), with=F] 

	## Join in previous turn info
	setkey(pturn.pros, tid)
	setkey(rs.pros0, first.tid)

	rs.pros <- pturn.pros[rs.pros0]	
	setnames(rs.pros, "tid", "first.tid")

	## Join in next turn info
	setkey(nturn.pros, tid)
	setkey(rs.pros, last.tid)
	rs.pros <- nturn.pros[rs.pros]	
	setnames(rs.pros, "tid", "last.tid")


	return(rs.pros)

}

# Join together word attribution info with word level prosodic features:
get.word.pros <- function() {
	word.pros <- NULL
	wordfiles <- list.files("../data/words/", pattern="*.words.txt.rs.tid")

	for (wordfile in wordfiles)  {
		print(wordfile)
		conv <- gsub("0[1-2].words.txt.rs.tid", "", wordfile) 
		print(conv)
		f0file  <- paste("../data/segs/f0/", conv, ".aggs.words.txt", sep="")
		i0file  <- paste("../data/segs/i0/", conv, ".aggs.words.txt", sep="")

		f0 <- data.table(read.table(f0file, header=T))
		i0 <- data.table(read.table(i0file, header=T))
		words <- data.table(read.table(paste("../data/words/", wordfile, sep=""), header=T))

		## Join the tables together
		setkey(f0, niteid)
		setkey(words, wid)
		word.f0 <- f0[words]
		word.f0 <- word.f0[order(wstarts)]
		setkey(i0, niteid)
		setkey(word.f0, niteid)
		word.if <- i0[word.f0]

		word.pros <- rbindlist(list(word.pros, word.if))
	}

	word.pros <- unique(word.pros[order(conv, spk, wstart)])

	## Something weird with this specific word, exclude it
	word.pros <- word.pros[!(niteid =="APPENUKENG003583.2.words41" & tid=="APPENUKENG003583.2.18")]

	## Add the RS type
	word.pros <- data.table(word.pros, rstype=word.pros[,substr(label,4,4)])	
	word.pros$rstype[word.pros$label==""] <- "N"
	word.pros$rstype[word.pros$rstype=="_"] <- "D"

	## Unique identifiers for speakers
	word.pros$spk <- paste(word.pros$conv, word.pros$spk, sep="-")


	return(word.pros)

}


## Add contextual information (previous and next word) to word prosody data.table
get.word.context <- function(word.pros) {

	word.meta <- unique(word.pros[,list( wid=niteid, tid=tid, 
				conv=conv, spk=spk)]) 

	## ids of surrounding words
	context.words <- unique(word.pros[,list(wid=niteid[order(wstart)], 
				pwid=c("NONE", head(unlevel(niteid[order(wstart)]),-1)), 
				nwid=c(tail(unlevel(niteid[order(wstart)]),-1), "NONE")) 
			,by=list(conv,spk)])

	setkey(word.meta, wid, conv, spk)
	setkey(context.words, wid, conv, spk)
	context.words <- word.meta[context.words]

	return(context.words)

}

## make data.table with word level pros feats of interest
get.pros.feat <- function(word.pros, context.words) {
	pros.feat <- unique(word.pros[, {
			list(wid=niteid, 
			word=word, wstart=wstarts, wend=wends, rstype=rstype,
			is.rsstart=is.rsstart, is.rsend=is.rsend, 
			rid=rid, label=label, 
			dur=wends-wstarts, 
			mean.normF0=mean.normF0, 
			sd.normF0=sd.normF0, 
			slope.normF0=slope.normF0, 
			range.normF0=max.normF0-min.normF0,
			mean.normI0=mean.normI0, 
			sd.normI0=sd.normI0, 
			slope.normI0=slope.normI0, 
			range.normI0=max.normI0-min.normI0)}])

	## Join in context info
	setkey(context.words, wid)
	setkey(pros.feat, wid)
	xpros <- context.words[pros.feat][order(conv,spk, wstart)]

	## Join in previous word features
	setnames(pros.feat, names(pros.feat), gsub("^", "x.", names(pros.feat)))
	setkey(pros.feat, x.wid)
	setkey(xpros, pwid)
	xprosp <- pros.feat[xpros][order(conv,spk,wstart)]
	setnames(xprosp, names(xprosp), gsub("^x.", "p.", names(xprosp)))
	
	
	## Join in next word features
	setkey(xprosp, nwid)
	xprospn <- pros.feat[xprosp][order(conv,spk,wstart)]
	setnames(xprospn, names(xprospn), gsub("^x.", "n.", names(xprospn)))

	## Add turn end info
	tid.words <- word.pros[,list(tid, ws=abs(starttime-wstarts), we=abs(wends-endtime), wid=niteid)]
	tid.bound <- tid.words[,list(tstart.wid=wid[which.min(ws)], tend.wid=wid[which.min(we)]),by=tid]
	xprospn <- data.table(xprospn, is.tstart=F, is.tend=F)
	xprospn$is.tstart[xprospn$wid %in% tid.bound$tstart.wid] <- T
	xprospn$is.tend[xprospn$wid %in% tid.bound$tend.wid] <- T

	rsturns <- xprospn[,list(num.rswords=sum(rstype!="N")),by=tid] 
	setkey(rsturns, tid) 
	setkey(xprospn, tid)
	xprospn <- rsturns[xprospn]
	setkey(xprospn, wid)

	save(xprospn, file="xprospn.dt")
	return(xprospn)

}

repverbs0 <- c("SAID","SAYS","SAYING","SAY",
		"THINK", "THINKS", "THOUGHT", "THINKING", 
		"KNOW", "KNOWS", "KNEW", "KNOWING",
		"GO", "GOES", "WENT", "GOING",
		"WANT","WANTS", "WANTED", "WANTING",
		"TELL", "TELLS", "TOLD", "TELLING", 
		"MEAN", "MEANS", "MEANT", "MEANING",
		"LIKE" 
		)

repverbs1 <- c( "SAID", "THOUGHT", "SAY", "SAYING", "KNOW", "SAYS", "WENT",
	"WANT", "LIKE", "THINKING", "THINK", "WANTED", "KNOWS", "GOES", "TOLD",
	"GO")


get.compare.groups <- function(xprospn) {

	rs.sample <- xprospn[is.rsstart == T]
	re.sample <- xprospn[is.rsend ==T]

	## How do RS segment starts compare to word boundaries with similar
	## Lexical properties?
	## Get top words occuring before an RS segmenta    
	rs20 <- xprospn[is.rsstart==T,length(wid) ,p.word][order(V1, decreasing=T)][1:30,unlevel(p.word)][c(1:21)]

	## get words that follow these words that aren't turn ends 
	## and aren't in turns containing reported speech 
	xverbs <- xprospn[word %in% rs20][is.tend==F]$tid
	xtid <- xprospn[tid %in% xverbs, list(sum(rid != "")),by=tid][V1==0]$tid
	x.nors <- xprospn[tid %in% xtid][p.word %in% rs20]

	## sample  to match the number of rs segments
	no.rs.sample <- x.nors 
	x.rs.sample.pword <- rbindlist(list(rs.sample, no.rs.sample))


	## How different are RS boundaries from other turn starts?
	x.no.rs.tstart <- xprospn[rstype == "N"][n.rstype=="N"][p.rstype=="N"][is.tend==F][is.tstart==T]
	no.rs.sample.tstart <- x.no.rs.tstart
	x.rs.sample.tstart <- rbindlist(list(rs.sample, no.rs.sample.tstart))

	## How different are RS ends from other turn ends? 
	## Get turn ends that aren't also RS ends and are not 
	## followed by an RS segment.  
	x.no.re <- xprospn[rstype == "N"][n.rstype=="N"][is.tend==T][is.tstart==F]
	no.re.sample <- x.no.re
	x.re.sample.tend <- rbindlist(list(re.sample, no.re.sample))


	## How different are RS boundaries from other turn medial boundaries?
	x.no.re.med <- xprospn[rstype == "N"][n.rstype=="N"][p.rstype=="N"][is.tend==F][is.tstart==F]
	no.re.sample.med <- x.no.re.med

	x.rs.sample.med <- rbindlist(list(rs.sample, no.re.sample.med))
	x.re.sample.med <- rbindlist(list(re.sample, no.re.sample.med))


	return(list(x.rs.pword=x.rs.sample.pword,
		x.rs.tstart=x.rs.sample.tstart,
		x.re.tend=x.re.sample.tend,
		x.rs.med=x.rs.sample.med,
		x.re.med=x.re.sample.med
		))

}

get.samples <- function(xprospn) {

	rs.sample <- xprospn[is.rsstart == T]
	re.sample <- xprospn[is.rsend ==T]

	## How do RS segment starts compare to word boundaries with similar
	## Lexical properties?
	## Get top words occuring before an RS segmenta    
	rs20 <- xprospn[is.rsstart==T,length(wid) ,p.word][order(V1, decreasing=T)][1:30,unlevel(p.word)][c(1:21)]

	## get words that follow these words that aren't turn ends 
	## and aren't in turns containing reported speech 
	xverbs <- xprospn[word %in% rs20][is.tend==F]$tid
	xtid <- xprospn[tid %in% xverbs, list(sum(rid != "")),by=tid][V1==0]$tid
	x.nors <- xprospn[tid %in% xtid][p.word %in% rs20]

	## sample  to match the number of rs segments
	no.rs.sample <- x.nors[sample(1:nrow(x.nors), size=nrow(rs.sample))]
	x.rs.sample.pword <- rbindlist(list(rs.sample, no.rs.sample))


	## How different are RS boundaries from other turn starts?
	x.no.rs.tstart <- xprospn[rstype == "N"][n.rstype=="N"][p.rstype=="N"][is.tend==F][is.tstart==F]
	no.rs.sample.tstart <- x.no.rs.tstart[sample(1:nrow(x.no.rs.tstart), size=nrow(rs.sample))]
	x.rs.sample.tstart <- rbindlist(list(rs.sample, no.rs.sample.tstart))

	## How different are RS ends from other turn ends? 
	## Get turn ends that aren't also RS ends and are not 
	## followed by an RS segment.  
	x.no.re <- xprospn[rstype == "N"][n.rstype=="N"][is.tend==T][is.tstart==F]
	no.re.sample <- x.no.re[sample(1:nrow(x.no.re), size=nrow(re.sample))]
	x.re.sample.tend <- rbindlist(list(re.sample, no.re.sample))


	## How different are RS boundaries from other turn medial boundaries?
	x.no.re.med <- xprospn[rstype == "N"][n.rstype=="N"][p.rstype=="N"][is.tend==F][is.tstart==F]
	no.re.sample.med <- x.no.re.med[sample(1:nrow(x.no.re.med), size=nrow(re.sample))]

	x.rs.sample.med <- rbindlist(list(rs.sample, no.re.sample.med))
	x.re.sample.med <- rbindlist(list(re.sample, no.re.sample.med))


	return(list(x.rs.sample.pword=x.rs.sample.pword,
		x.rs.sample.tstart=x.rs.sample.tstart,
		x.re.sample.tend=x.re.sample.tend,
		x.rs.sample.med=x.rs.sample.med,
		x.re.sample.med=x.re.sample.med
		))

}

models.turn <- function(xturn) {
	xturn <- data.table(xturn, xturn[,list( 
			pdur=p.wend-p.wstart, 
			ndur=n.wend-n.wstart, 
			pgap=p.wend- wstart,
			ngap=n.wstart- wend,
                        dn.mean.F0=-(mean.normF0 - n.mean.normF0),
                        dn.range.F0=-(range.normF0 - n.range.normF0),
                        dn.mean.I0=-(mean.normI0 - n.mean.normI0),
                        dn.range.I0=-(range.normI0 - n.range.normI0),
                        dp.mean.F0=mean.normF0 - p.mean.normF0,
                        dp.range.F0=range.normF0 - p.range.normF0,
                        dp.mean.I0=mean.normI0 - p.mean.normI0,
                        dp.range.I0=range.normI0 - p.range.normI0) ])

	m.turn <- glmer(is.rs ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(slope.normF0) + to.zscore(slope.normI0) 
			#+ to.zscore(dp.mean.F0) + to.zscore(dp.mean.I0) 
			#+ to.zscore(dp.range.F0) + to.zscore(dp.range.I0) 
			#+ to.zscore(dn.mean.F0) + to.zscore(dn.mean.I0) 
			#+ to.zscore(dn.range.F0) + to.zscore(dn.range.I0) 
			#+ log(pdur) + log(ndur) + pgap + ngap
			+ to.zscore(intern.pause)
			+ to.zscore(abs(dur))
			, data=xturn,  family = binomial(link = "logit"))

	xturn.all <- turn.pros[,list(rs=(nrswords>0), id=tid, spk=spk, dur=tend-tstart, 
		mean.normF0, sd.normF0, slope.normF0, 
		range.normF0=max.normF0-min.normF0, 
		range2.normF0=q99.normF0-q1.normF0, 
		mean.normI0, sd.normI0, slope.normI0, 
		range.normI0=max.normI0-min.normI0, 
		range2.normI0=q99.normI0-q1.normI0 
		)] 		

	m.turn.all <- glmer(rs ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(slope.normF0) + to.zscore(slope.normI0) 
			+ to.zscore(log(abs(dur)))
			, data=xturn.all,  family = binomial(link = "logit"))

	rs.pros <- data.table(rs.pros, rstype=rs.pros[,substr(label,4,4)])
	rs.pros[rstype=="_"]$rstype <- "D"

	xrs.id <- rs.pros[rstype %in% c("D","I"),list(rs=(rstype=="D"), id=rid, spk=spk, dur=rstart-rend, nwords,
		mean.normF0, sd.normF0, slope.normF0, 
		range.normF0=max.normF0-min.normF0, 
		range2.normF0=q99.normF0-q1.normF0, 
		mean.normI0, sd.normI0, slope.normI0, 
		range.normI0=max.normI0-min.normI0, 
		range2.normI0=q99.normI0-q1.normI0 
		)] 		

	xid <- refactor(xturn[rstype %in% c("D","I")])
	xid$rstype <- factor(xid$rstype)
	m.turn.id <- glmer(rstype ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(slope.normF0) + to.zscore(slope.normI0) 
			+ to.zscore(dp.mean.F0) + to.zscore(dp.mean.I0) 
			+ to.zscore(dp.range.F0) + to.zscore(dp.range.I0) 
			+ to.zscore(dn.mean.F0) + to.zscore(dn.mean.I0) 
			+ to.zscore(dn.range.F0) + to.zscore(dn.range.I0) 
			+ to.zscore(log(abs(dur)))
			, data=xid,  family = binomial(link = "logit"))

}

models.id <- function(xprospn) {
	xid <- refactor(xprospn[rstype %in% c("D","I")]) 
	xid <- data.table(xid, is.direct=(xid$rstype =="D"))
	m.id.start <- glmer(is.direct ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=xid[is.rsstart==T],  family = binomial(link = "logit"))

	m.id.end <- glmer(is.direct ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(range.normF0 - n.range.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(range.normI0 - n.range.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=xid[is.rsend==T],  family = binomial(link = "logit"))

}

models.compare <- function(xprospn) {

	xs.list <- get.compare.groups(xprospn) 
	save(xs.list, file="x.compare.list")

	x.rs.pword <- xs.list[["x.rs.pword"]] 
	x.rs.tstart<- xs.list[["x.rs.tstart"]] 
	x.rs.med <- xs.list[["x.rs.med"]]

	m.start.pword <- glmer(is.rsstart ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=x.rs.pword,  family = binomial(link = "logit"))
	display(m.start.pword)

	m.start.tstart <- glmer(is.rsstart ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=x.rs.tstart,  family = binomial(link = "logit"))
	display(m.start.tstart)

	m.start.med <- glmer(is.rsstart ~ (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=x.rs.med,  family = binomial(link = "logit"))
	display(m.start.med)
	
	############################################################################
	x.re.med=xs.list[["x.re.med"]]
	x.re.tend=xs.list[["x.re.tend"]]

	m.end.tend <- glmer(is.rsend ~ (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(range.normF0 - n.range.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(range.normI0 - n.range.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=x.re.tend,  family = binomial(link = "logit"))

	display(m.end.tend)

	m.end.med <- glmer(is.rsend ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(range.normF0 - n.range.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(range.normI0 - n.range.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=x.re.med,  family = binomial(link = "logit"))
	display(m.end.med)

}

models.sample <- function(xprospn) {

	x.samples.list <- get.samples(xprospn) 
	save(x.samples.list, file="x.samples.list")

	x.rs.sample.pword <- x.samples.list[["x.rs.sample.pword"]] 
	x.rs.sample.tstart<- x.samples.list[["x.rs.sample.tstart"]] 
	x.rs.sample.med <- x.samples.list[["x.rs.sample.med"]]

	m.start.pword <- glmer(is.rsstart ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=x.rs.sample.pword,  family = binomial(link = "logit"))

	m.start.tstart <- glmer(is.rsstart ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=x.rs.sample.tstart,  family = binomial(link = "logit"))

	m.start.med <- glmer(is.rsstart ~ (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=x.rs.sample.med,  family = binomial(link = "logit"))
	display(m.start.med)
	#---------------------------------------------------------------------------#

	m.start.pword.pw <- glmer(is.rsstart ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(sd.normF0) + to.zscore(sd.normI0) 
			+ to.zscore(slope.normF0) + to.zscore(slope.normI0) 
			+ to.zscore(p.mean.normF0) + to.zscore(p.mean.normI0) 
			+ to.zscore(p.sd.normF0) + to.zscore(p.sd.normI0) 
			+ to.zscore(p.slope.normF0) + to.zscore(p.slope.normI0) 
			, data=x.rs.sample.pword,  family = binomial(link = "logit"))
	
	############################################################################
	x.re.sample.med=x.samples.list[["x.re.sample.med"]]
	x.re.sample.tend=x.samples.list[["x.re.sample.tend"]]

	m.end.tend <- glmer(is.rsend ~ (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(range.normF0 - n.range.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(range.normI0 - n.range.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=x.re.sample.tend,  family = binomial(link = "logit"))

	display(m.end.tend)

	m.end.med <- glmer(is.rsend ~  (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(range.normF0 - n.range.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(range.normI0 - n.range.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=x.re.sample.med,  family = binomial(link = "logit"))
	display(m.end.med)

	#---------------------------------------------------------------------------#

	m.end.tend.sd <- glmer(is.rsend ~ (1 | conv) + (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(sd.normF0) + to.zscore(sd.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(sd.normF0 - n.sd.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(sd.normI0 - n.sd.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=x.re.sample.tend,  family = binomial(link = "logit"))

	m.end.med.sd <- glmer(is.rsend ~ (1 | conv) + (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(sd.normF0) + to.zscore(sd.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(sd.normF0 - n.sd.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(sd.normI0 - n.sd.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=x.re.sample.med,  family = binomial(link = "logit"))
}



models.all <- function(xprospn) {

	m.start.prev <- glmer(is.rsstart ~ (1 | conv) + (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(sd.normF0) + to.zscore(sd.normI0) 
			+ to.zscore(slope.normF0) + to.zscore(slope.normI0) 
			+ to.zscore(p.mean.normF0) + to.zscore(p.mean.normI0) 
			+ to.zscore(p.sd.normF0) + to.zscore(p.sd.normI0) 
			+ to.zscore(p.slope.normF0) + to.zscore(p.slope.normI0) 
			, data=xprospn,  family = binomial(link = "logit"))

	m.start.sd <- glmer(is.rsstart ~ (1 | conv) + (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(sd.normF0) + to.zscore(sd.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(sd.normF0 - p.sd.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(sd.normI0 - p.sd.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=xprospn,  family = binomial(link = "logit"))

	m.start.nodr <- glmer(is.rsstart ~ (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			#+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			#+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=xprospn,  family = binomial(link = "logit"))

	m.start <- glmer(is.rsstart ~ (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - p.mean.normF0) 
			+ to.zscore(range.normF0 - p.range.normF0) 
			+ to.zscore(mean.normI0 - p.mean.normI0) 
			+ to.zscore(range.normI0 - p.range.normI0) 
			+ to.zscore(wstart - p.wend) 
			, data=xprospn,  family = binomial(link = "logit"))


	m.end0 <- glmer(is.rsend ~ (1 | conv) + (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(sd.normF0) + to.zscore(sd.normI0) 
			, data=xprospn,  family = binomial(link = "logit"))

	m.end.pw <- glmer(is.rsend ~ (1 | conv) + (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(sd.normF0) + to.zscore(sd.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(sd.normF0 - n.sd.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(sd.normI0 - n.sd.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=xprospn,  family = binomial(link = "logit"))

	m.end <- glmer(is.rsend ~ (1 | conv) + (1 | spk) 
			+ to.zscore(mean.normF0) + to.zscore(mean.normI0) 
			+ to.zscore(range.normF0) + to.zscore(range.normI0) 
			+ to.zscore(mean.normF0 - n.mean.normF0) 
			+ to.zscore(range.normF0 - n.range.normF0) 
			+ to.zscore(mean.normI0 - n.mean.normI0) 
			+ to.zscore(range.normI0 - n.range.normI0) 
			+ to.zscore(wstart - n.wend) 
			, data=xprospn,  family = binomial(link = "logit"))




}

get.auroc <- function(y) {
        y <- data.table(y)
        #if(nrow(y[pred==1]) == 0) {
        #        return(0)
        #}
        rocpred <- prediction(y$prob, y$labels)
        xauc <- attributes(performance(rocpred, "auc"))$y.values[[1]]
        return(xauc)

}



get.ir.stats <- function(x, pred.var="test.pred", orig.var="test.labels") {
        if (!is.data.table(x)) {
                x <- data.table(x)
        }
        setnames(x, c(orig.var), c("labels"))
        setnames(x, c(pred.var), c("pred"))
        x <- data.table(xid=1:nrow(x), x)
#	print(x)

        ctab.all <- x[,length(xid),by=list(labels, pred)]

        irstats <- NULL

        for (label in unique(ctab.all$labels)) {
                #print(label)
                ctab <- ctab.all[,sum(V1),by=list(labels==label, pred==label)]
                nlabel <- ctab[,sum(V1)]
                prop <- ctab[labels==T, sum(V1)]/nlabel
                N <- ctab[labels==T, sum(V1)]
                baseline <- max((ctab[,sum(V1),by=labels]$V1)/nlabel, na.rm=T)
                accuracy <- ctab[,sum(V1[labels == pred])/(sum(V1))]
                precision <- ctab[,V1[labels & pred]/(sum(V1[pred]))]
                if (length(precision)==0) {precision <- 0}
                recall <- ctab[,V1[labels & pred]/(sum(V1[labels]))]
                if (length(recall)==0) {recall <- 0}
                F1 <- 2* ((precision*recall)/(precision+recall))
                irstats <- rbindlist(list(irstats, data.table(label, prop, N, accuracy, precision=precision, recall=recall, F1=F1)))

        }
        irstats[is.na(F1)]$F1 <- 0
	#print(names(x))
        setnames(x, c("labels"), c(orig.var))
        setnames(x, c("pred"), c(pred.var))

	#irstats <- data.table(irstats, auroc=get.auroc(x))
	#print(names(x))
        return(irstats)
}


get.exp.feats <- function(x, re20, rs20) {
	parc <- toupper(c("say","add","note","think","believe","tell","argue","expect","report","estimate")) 
	x.data <- x[,list( 
			pgap=(wstart-p.wend),
			ngap=(n.wstart-wend),
			p.parc= 1.0 * (p.word %in% parc), 	
			p.said= 1.0 * (p.word == "SAID"), 	
			p.thought= 1.0 * (p.word == "THOUGHT"), 	
			p.rs20= 1.0 * (p.word %in% rs20),
			n.re20= 1.0 * (n.word %in% re20),
			n.and= 1.0 * (n.word == "AND"), 	
			n.but= 1.0 * (n.word == "BUT"), 	
			sd.normF0,  sd.normI0, 
			mean.normF0,  mean.normI0, 
			range.normF0, range.normI0,
			slope.normF0, slope.normI0,
                        dn.mean.F0=-(mean.normF0 - n.mean.normF0),
                        dn.range.F0=-(range.normF0 - n.range.normF0),
                        dn.mean.I0=-(mean.normI0 - n.mean.normI0),
                        dn.range.I0=-(range.normI0 - n.range.normI0),
                        dp.mean.F0=mean.normF0 - p.mean.normF0,
                        dp.range.F0=range.normF0 - p.range.normF0,
                        dp.mean.I0=mean.normI0 - p.mean.normI0,
                        dp.range.I0=range.normI0 - p.range.normI0 
			)]
	return(x.data)
}

## Partitions for experiments detecting end points of reported speech
train.test.re.all <- function(xprospn, k=10, rsturn.only=T) {
	rs20 <- unlevel(xprospn[is.rsstart==T,length(wid),by=p.word][order(V1, decreasing=T)][1:20]$p.word)
	re20 <- unlevel(xprospn[is.rsend==T,length(wid),by=n.word][order(V1, decreasing=T)][1:20]$n.word)

	if (rsturn.only==T) {
		x0 <- xprospn[is.rsturn==T]
	} else {
		x0 <- copy(xprospn)
	}

	x0.data <- get.exp.feats(x0[is.rsend==F], re20=re20, rs20=rs20)
	x1.data <- get.exp.feats(x0[is.rsend==T], re20=re20, rs20=rs20)

	x0.target <- x0$is.rsend[x0$is.rsend==F] 
	x1.target <- x0$is.rsend[x0$is.rsend==T] 
	
	x0.wid <- unlevel(x0$wid[x0$is.rsend==F])
	x1.wid <- unlevel(x0$wid[x0$is.rsend==T])

	x0.data.shuffle <- sample(1:nrow(x0.data))
	x1.data.shuffle <- sample(1:nrow(x1.data))

	p0.size <- floor(nrow(x0.data)/k)
	p0.end <- c(1:k) * p0.size 	
	p0.start <- c(1, head(p0.end, -1)+1)

	p1.size <- floor(nrow(x1.data)/k)
	p1.end <- c(1:k) * p1.size 	
	p1.start <- c(1, head(p1.end, -1)+1)

	x.all <- rbindlist(list(x0.data, x1.data))
	y.all <- as.factor(c(x0.target, x1.target))	
	xy.all <- data.table(target=factor(y.all), x.all)

	return(list(x0.data=x0.data, x1.data=x1.data, 
		x0.target=x0.target, x1.target=x1.target,
		x0.wid=x0.wid, x1.wid=x1.wid,
		x0.data.shuffle=x0.data.shuffle, x1.data.shuffle=x1.data.shuffle,
		p0.size=p0.size, p0.end=p0.end, p0.start=p0.start,
		p1.size=p1.size, p1.end=p1.end, p1.start=p1.start,
		x.all=x.all, y.all=y.all, xy.all=xy.all
		))


}


## Partitions for experiments detecting start points of reported speech
train.test.rs.all <- function(xprospn, k=10, rsturn.only=T) {
	rs20 <- unlevel(xprospn[is.rsstart==T,length(wid),by=p.word][order(V1, decreasing=T)][1:20]$p.word)
	re20 <- unlevel(xprospn[is.rsend==T,length(wid),by=n.word][order(V1, decreasing=T)][1:20]$n.word)

	if (rsturn.only==T) {
		x0 <- xprospn[is.rsturn==T]
	} else {
		x0 <- copy(xprospn)
	}

	x0.data <- get.exp.feats(x0[is.rsstart==F], re20=re20, rs20=rs20)
	x1.data <- get.exp.feats(x0[is.rsstart==T], re20=re20, rs20=rs20)

	x0.target <- x0$is.rsstart[x0$is.rsstart==F] 
	x1.target <- x0$is.rsstart[x0$is.rsstart==T] 
	
	x0.wid <- unlevel(x0$wid[x0$is.rsstart==F])
	x1.wid <- unlevel(x0$wid[x0$is.rsstart==T])

	#co <- 1.7

	x0.data.shuffle <- sample(1:nrow(x0.data))
	x1.data.shuffle <- sample(1:nrow(x1.data))

	p0.size <- floor(nrow(x0.data)/k)
	p0.end <- c(1:k) * p0.size 	
	p0.start <- c(1, head(p0.end, -1)+1)

	p1.size <- floor(nrow(x1.data)/k)
	p1.end <- c(1:k) * p1.size 	
	p1.start <- c(1, head(p1.end, -1)+1)

	x.all <- rbindlist(list(x0.data, x1.data))
	y.all <- as.factor(c(x0.target, x1.target))	
	xy.all <- data.table(target=factor(y.all), x.all)


	return(list(x0.data=x0.data, x1.data=x1.data, 
		x0.target=x0.target, x1.target=x1.target,
		x0.wid=x0.wid, x1.wid=x1.wid,
		x0.data.shuffle=x0.data.shuffle, x1.data.shuffle=x1.data.shuffle,
		p0.size=p0.size, p0.end=p0.end, p0.start=p0.start,
		p1.size=p1.size, p1.end=p1.end, p1.start=p1.start,
		x.all=x.all, y.all=y.all, xy.all=xy.all
		))


}

get.turn.feats <- function(x) {
	x.data <- x[,list( 
		        pdur=p.wend-p.wstart,
                        ndur=n.wend-n.wstart,
                        pgap=starttime - pw.wend,
                        ngap=nw.wstart- endtime,
			intern.pause, 
			mean.normF0,  mean.normI0, 
			sd.normF0,  sd.normI0, 
			range.normF0, range.normI0,
			slope.normF0, slope.normI0,
                        dn.mean.F0=-(mean.normF0 - n.mean.normF0),
                        dn.range.F0=-(range.normF0 - n.range.normF0),
                        dn.mean.I0=-(mean.normI0 - n.mean.normI0),
                        dn.range.I0=-(range.normI0 - n.range.normI0),
                        dp.mean.F0=mean.normF0 - p.mean.normF0,
                        dp.range.F0=range.normF0 - p.range.normF0,
                        dp.mean.I0=mean.normI0 - p.mean.normI0,
                        dp.range.I0=range.normI0 - p.range.normI0,
			dur=wend-wstart,
			nwords=nwords
			)]
	return(x.data)
}


## Get elements needed for k-fold cross-validation experiments 
## with liblinear, libsvm.
## i.e. folds are determined here.

train.test.turn <- function(x0, k=10) {
	x0.data <- get.turn.feats(x0[is.rs==F])
	x1.data <- get.turn.feats(x0[is.rs==T])

	x0.target <- x0$is.rs[x0$is.rs==F] 
	x1.target <- x0$is.rs[x0$is.rs==T] 
	
	x0.wid <- unlevel(x0$id[x0$is.rs==F])
	x1.wid <- unlevel(x0$id[x0$is.rs==T])

	x0.data.shuffle <- sample(1:nrow(x0.data))
	x1.data.shuffle <- sample(1:nrow(x1.data))

	## Folds
	p0.size <- floor(nrow(x0.data)/k)
	p0.end <- c(1:k) * p0.size 	
	p0.start <- c(1, head(p0.end, -1)+1)

	p1.size <- floor(nrow(x1.data)/k)
	p1.end <- c(1:k) * p1.size 	
	p1.start <- c(1, head(p1.end, -1)+1)

	## All together
	x.all <- rbindlist(list(x0.data, x1.data))
	y.all <- as.factor(c(x0.target, x1.target))	
	xy.all <- data.table(target=factor(y.all), x.all)


	## Lots of extraneous info here I think
	return(list(x0.data=x0.data, x1.data=x1.data, 
		x0.target=x0.target, x1.target=x1.target,
		x0.wid=x0.wid, x1.wid=x1.wid,
		x0.data.shuffle=x0.data.shuffle, x1.data.shuffle=x1.data.shuffle,
		p0.size=p0.size, p0.end=p0.end, p0.start=p0.start,
		p1.size=p1.size, p1.end=p1.end, p1.start=p1.start,
		x.all=x.all, y.all=y.all, xy.all=xy.all
		))


}


train.test.rs.even <- function(x.samples.list, k=10) {
	x0 <- x.samples.list[["x.rs.sample.med"]]

	x0.data <- get.exp.feats(x0[is.rsstart==F], re20=re20, rs20=rs20)
	x1.data <- get.exp.feats(x0[is.rsstart==T], re20=re20, rs20=rs20)

	x0.target <- x0$is.rsstart[x0$is.rsstart==F] 
	x1.target <- x0$is.rsstart[x0$is.rsstart==T] 
	
	x0.wid <- unlevel(x0$wid[x0$is.rsstart==F])
	x1.wid <- unlevel(x0$wid[x0$is.rsstart==T])

	x0.data.shuffle <- sample(1:nrow(x0.data))
	x1.data.shuffle <- sample(1:nrow(x1.data))
	p0.size <- floor(nrow(x0.data)/k)
	p0.end <- c(1:k) * p0.size 	
	p0.start <- c(1, head(p0.end, -1)+1)
	p1.size <- floor(nrow(x1.data)/k)
	p1.end <- c(1:k) * p1.size 	
	p1.start <- c(1, head(p1.end, -1)+1)

	x.all <- rbindlist(list(x0.data, x1.data))
	y.all <- as.factor(c(x0.target, x1.target))	
	xy.all <- data.table(target=factor(y.all), x.all)

	return(list(x0.data=x0.data, x1.data=x1.data, 
		x0.target=x0.target, x1.target=x1.target,
		x0.wid=x0.wid, x1.wid=x1.wid,
		x0.data.shuffle=x0.data.shuffle, x1.data.shuffle=x1.data.shuffle,
		p0.size=p0.size, p0.end=p0.end, p0.start=p0.start,
		p1.size=p1.size, p1.end=p1.end, p1.start=p1.start, 
		x.all=x.all, y.all=y.all, xy.all=xy.all
		))


}


rs.cv <- function(x.params,features, k=10,  co=1.7, use.svm=T, gamma=0.5, downsample=T, probabilities=F, kernel="radial") {
	xfeatures <- features
	#print("===++++++++++++++++++++")
	#print(c(co, gamma))
	#print(xfeatures)
	#print("===---------------------")	

	x0.data <- x.params[["x0.data"]]	
	x0.target <- x.params[["x0.target"]]	
	x0.wid <- x.params[["x0.wid"]]	
	x0.data.shuffle <- x.params[["x0.data.shuffle"]]	
	x1.data <- x.params[["x1.data"]]	
	x1.target <- x.params[["x1.target"]]	
	x1.wid <- x.params[["x1.wid"]]	
	x1.data.shuffle <- x.params[["x1.data.shuffle"]]	

	p0.size <- x.params[["p0.size"]]	
	p0.end <- x.params[["p0.end"]]	
	p0.start <- x.params[["p0.start"]]	

	p1.size <- x.params[["p1.size"]]	
	p1.end <- x.params[["p1.end"]]	
	p1.start <- x.params[["p1.start"]]	

	x0.shuffle <- x0.data[x0.data.shuffle][,xfeatures, with=F]
	y0.shuffle <- x0.target[x0.data.shuffle] * 1

	x1.shuffle <- x1.data[x1.data.shuffle][,xfeatures, with=F]
	y1.shuffle <- x1.target[x1.data.shuffle] * 1 

	all.pred <- NULL

	for (i in 1:k) {
		#print(paste("*****", i, "*********", collapse=" "))
		xi0 <- 1:nrow(x0.shuffle)
		xi1 <- 1:nrow(x1.shuffle)	
		#print(c(length(xi0), length(xi1)))

		if (downsample == T) {
			test.ind0.all <- p0.start[i]:p0.end[i]
			test.ind1.all <- p1.start[i]:p1.end[i]
			train.ind0.all <- xi0[-c(p0.start[i]:p0.end[i] )]
			train.ind1.all <- xi1[-c(p1.start[i]:p1.end[i] )]

			samp.size <- min(length(train.ind0.all), length(train.ind1.all))
			train.ind0 <- sample(train.ind0.all, size=samp.size)
			train.ind1 <- sample(train.ind1.all, size=samp.size)

		} else {
			test.ind0.all <- p0.start[i]:p0.end[i]
			test.ind1.all <- p1.start[i]:p1.end[i]
			test.ind0 <- p0.start[i]:p0.end[i]
			test.ind1 <- p1.start[i]:p1.end[i]

			train.ind0 <- xi0[-c(p0.start[i]:p0.end[i] )]
			train.ind1 <- xi0[-c(p1.start[i]:p1.end[i] )]
		}

		xtrain <- rbindlist(list(x0.shuffle[train.ind0],
			x1.shuffle[train.ind1] ))

		ytrain <-as.factor(c(y0.shuffle[train.ind0], y1.shuffle[train.ind1]))


		xtest <- rbindlist(list( x0.shuffle[test.ind0.all],
			x1.shuffle[test.ind1.all] ))
		

		ytest <- as.factor(c(y0.shuffle[test.ind0.all], y1.shuffle[test.ind1.all]))

		s <- scale(xtrain,center=TRUE,scale=TRUE)
		s <- fix.na(s)

		stest <- scale(xtest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
		stest <- fix.na(stest)

		if (use.svm == T) {
			if (probabilities==T) {
				#print(kernel)
				pred <- NULL
				#print(colnames(s))	
				#m <- svm(x=s,y=ytrain,scale=F, kernel="radial",gamma = gamma, cost=co, probability=T)
				m <- svm(x=s,y=ytrain,scale=F, kernel=kernel,gamma = gamma, cost=co, probability=T)
				tryCatch({	
					p <- predict(m,stest,probability=T) 
					probs <- attr(p, "probabilities")
					pred <- data.table(id=c(x0.wid[test.ind0.all],x1.wid[test.ind1.all]), 
						pred=p, labels=ytest, probs)	
				}, error = function(e) {
    					print("*** prediction problem ***")	
					print(e)
					print(xfeatures)
			#		print(colnames(s))	
				#	print(colnames(stest))	
			#		print(c(co, gamma))
					pred <- NULL
					print("**************************")

				})

				if (is.null(pred)) {
					pred <- data.table(id=c(x0.wid[test.ind0.all],x1.wid[test.ind1.all]), 
						pred=0, labels=ytest, "0"=1, "1"=0)	
					#print(pred)
				}	
				#print(p)
			} else {

				m <- svm(x=s,y=ytrain,scale=F, kernel="radial",gamma = gamma, cost=co)
				p <- predict(m,stest) 
				#print(p)
				pred <- data.table(id=c(x0.wid[test.ind0.all],x1.wid[test.ind1.all]), 
					pred=p, labels=ytest)	
			}

		} else {
			#print("logistic")
       			m <- LiblineaR(data=s,target=ytrain, type=6,cost=co,bias=TRUE,verbose=F)
			p <- predict(m,stest,proba=T)	
			#print(p)
			pred <- data.table(id=c(x0.wid[test.ind0.all],x1.wid[test.ind1.all]), 
				pred=p$predictions, labels=ytest, p$probabilities)
		}

		if (is.null(pred)) {
			print(c("///+++++", xfeatures)) 
		}
		all.pred <- rbindlist(list(all.pred, pred)) 

	}
	return(all.pred)
}


fix.na <- function(s) {
	#print(summary(s))

	for (cname in colnames(s)) {
		s[is.na(s[,cname]),cname] <- 0
	}
	
	#print(summary(s))
	return(s)
}

rs.cv.tune <- function(x.params, feats=feats, probabilities=T, use.svm=T, downsample=T, kernel="radial") {
	print(feats)
	print(c("DOWNSAMPLE", downsample))
	irstats <- NULL	
	for (co in 2^(-2:0)) {
		for (gamma in 2^(-2:0)) {
			#print(paste("***-----TUNE:", co, gamma, collapse=" "))
			#print(c(co, gamma))
			#print(feats)
			all.pred <- rs.cv(x.params, use.svm=use.svm, co=co, gamma=gamma, downsample=downsample, features=feats, probabilities=probabilities, kernel=kernel)
			#print(paste("***-----END TUNE:", co, gamma, collapse=" "))
			if (probabilities == T) {
				setnames(all.pred, "1", "prob")
				curr.auroc <- get.auroc(all.pred)
				curr.irstats <- data.table(cost=co, gamma=gamma, auroc=curr.auroc, get.ir.stats(all.pred, "pred", "labels"))
			} else {
				curr.irstats <- data.table(cost=co, gamma=gamma,  get.ir.stats(all.pred, "pred", "labels"))
			}
			irstats <- rbindlist(list(irstats, curr.irstats))
		}
	}

	#print("END rs.cv.tune")
	return(irstats)
	
}



fselect <- function(xs.params.all, feat.names, downsample=T) {
	print(feat.names)
	best.feats <- NULL
	best.auroc <- 0
	res.accum <- NULL

	while (length(feat.names) > 0) {
		all.rsstats.list <- mclapply(feat.names, function(x) {
					print(x)
					feats <- c(best.feats, x)
					tune.stats <- rs.cv.tune(xs.params.all, feats=feats, probabilities=T, downsample=downsample)
					if (is.null(tune.stats)) {
						print(c("+++++++", feats, "++++++++"))	
					}
					irstats <- data.table(feat=x, tune.stats)	
				
					return(irstats)
				}, mc.cores=8) 

		print("HERE")
		#print(all.rsstats.list[[1]])
		lapply(all.rsstats.list, function(x) {if(!("label" %in% names(x))){print(x)}})
		curr.stats <- data.table(ldply(all.rsstats.list, function(x) {
				if (!("label" %in% names(x))) {
					return(NULL)
				} 
				return(x[label==1][order(auroc,decreasing=T)][1])
				}))	
		curr.auroc <- curr.stats[, max(auroc)]

		print("!!")

		if (curr.auroc > best.auroc) {
			curr.best.feat <- curr.stats[, feat[which.max(auroc)]]
			best.feats <- c(best.feats, curr.best.feat)
			feat.names <- feat.names[!(feat.names %in% best.feats)]
			best.auroc <- curr.auroc
			print(feat.names)
			print(best.feats)
			print(best.auroc)
			res.accum <- rbindlist(list(res.accum, curr.stats[which.max(auroc)]))
		} else {
			feat.names <- c()
		}
	}

	return(list(best.feats, curr.auroc, curr.stats, res.accum))

}

get.rspros.data <- function(rs.pros, xprospn) {
	if ("rid" %in% names(rs.pros)) {
		setnames(rs.pros, "rid", "id")
	}

        xturn <- data.table(is.rs=(rs.pros$rstype=="D"), rs.pros)

	xturn <- data.table(xturn, xturn[,list(
			range.normF0=max.normF0-min.normF0, range.normI0=max.normI0-min.normI0,
			p.range.normF0=p.max.normF0-p.min.normF0, p.range.normI0=p.max.normI0-p.min.normI0,
			n.range.normF0=n.max.normF0-n.min.normF0, n.range.normI0=n.max.normI0-n.min.normI0,
			dur=endtime-starttime
			)])

	pwtime <- xprospn[,list(wid, p.wid, pw.wstart=p.wstart, pw.wend=p.wend)]
	nwtime <- xprospn[,list(wid, n.wid, nw.wstart=n.wstart, nw.wend=n.wend)]

	setkey(pwtime, wid)
	setkey(xturn, swid)
	xturn <- pwtime[xturn]
	setnames(xturn, "wid", "swid")	

	setkey(nwtime, wid)
	setkey(xturn, ewid)
	xturn <- nwtime[xturn]
	setnames(xturn, "wid", "ewid")	

	return(xturn)

}

get.turn.data.nors <- function(turn.pros, xprospn) {
	if ("tid" %in% names(turn.pros)) {
		setnames(turn.pros, "tid", "id")
	}

        xturn <- data.table(is.rs=(turn.pros$nrswords>0), turn.pros)

	xturn <- data.table(xturn, xturn[,list(
			range.normF0=max.normF0-min.normF0, range.normI0=max.normI0-min.normI0,
			p.range.normF0=p.max.normF0-p.min.normF0, p.range.normI0=p.max.normI0-p.min.normI0,
			n.range.normF0=n.max.normF0-n.min.normF0, n.range.normI0=n.max.normI0-n.min.normI0,
			dur=endtime-starttime
			)])

	pwtime <- xprospn[,list(wid, p.wid, pw.wstart=p.wstart, pw.wend=p.wend)]
	nwtime <- xprospn[,list(wid, n.wid, nw.wstart=n.wstart, nw.wend=n.wend)]

	setkey(pwtime, wid)
	setkey(xturn, swid)
	xturn <- pwtime[xturn]
	setnames(xturn, "wid", "swid")	

	setkey(nwtime, wid)
	setkey(xturn, ewid)
	xturn <- nwtime[xturn]
	setnames(xturn, "wid", "ewid")	

	return(xturn)

}


get.turn.data <- function(turn.pros, rs.pros, xprospn) {
	setnames(turn.pros, "tid", "id")
	setnames(rs.pros, "rid", "id")

	feat.names <- c("id", intersect(names(rs.pros), names(turn.pros)))	

	## Turns that contain reported speech
        xturn <- data.table(is.rs=F, turn.pros[nrswords==0, feat.names, with=F])
	## Turns that don't contain reported speech
        xrs <- data.table(is.rs=T, rs.pros[,feat.names, with=F])

	## All turn info
        xturn <- rbindlist(list(xturn, xrs))

	### Features we'll use for classification etc.
	xturn <- data.table(xturn, xturn[,list(
			range.normF0=max.normF0-min.normF0, range.normI0=max.normI0-min.normI0,
			p.range.normF0=p.max.normF0-p.min.normF0, p.range.normI0=p.max.normI0-p.min.normI0,
			n.range.normF0=n.max.normF0-n.min.normF0, n.range.normI0=n.max.normI0-n.min.normI0,
			dur=endtime-starttime
			)])

	## Add in Previous and next word start and end times
	pwtime <- xprospn[,list(wid, p.wid, pw.wstart=p.wstart, pw.wend=p.wend)]
	nwtime <- xprospn[,list(wid, n.wid, nw.wstart=n.wstart, nw.wend=n.wend)]

	setkey(pwtime, wid)
	setkey(xturn, swid)
	xturn <- pwtime[xturn]
	setnames(xturn, "wid", "swid")	

	setkey(nwtime, wid)
	setkey(xturn, ewid)
	xturn <- nwtime[xturn]
	setnames(xturn, "wid", "ewid")	

	return(xturn)

}


plot.roc <-  function(x) {
	pred <- prediction(x$prob, x$labels)
	perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
	plot(perf)
	perf2 <- performance(pred, measure = "prbe")
	thr <- attr(perf2, "x.value")
	print(thr)

	perf3 <- performance(pred, measure = "f")
	thr <- data.table(V1=attr(perf3, "x.value")[[1]], V2=attr(perf3, "y.value")[[1]])[order(V2,decreasing=T)][1,V1]
	print(thr)
	u <- get.ir.stats(x[,list(obs=(labels==1), pred=prob >= thr, id)], "pred", "obs")
	#print(x)	
	u0 <- get.ir.stats(x, "pred", "labels")

	print(paste(c(round(get.auroc(x),3), "&", round(thr,3), "&", round(u[,mean(F1)],3)), collapse=" "))

	res <- data.table(res0=u0, res.thr=u, thr=thr, auroc=get.auroc(x))  
	return(res)	
}

pred.tune <- function(x.params, feats, outfile="pred.x", downsample=T, use.svm=T, kernel="radial") {
	print(downsample)
	gamma <- 1
	pred.x.tune <- rs.cv.tune(x.params, feats=feats, probabilities=T, downsample=downsample, use.svm=use.svm, kernel=kernel)
	save(pred.x.tune, file=paste(outfile,".tune",sep="") )
	gamma <- pred.x.tune[order(auroc, decreasing=T)][1]$gamma
	co <- pred.x.tune[order(auroc, decreasing=T)][1]$cost
	print(c(co, gamma))
	pred.x <- rs.cv(x.params, use.svm=use.svm, co=co, gamma=gamma, downsample=downsample, features=feats, probabilities=T)
	setnames(pred.x, "1", "prob")
	save(pred.x, file=outfile)
	plot.roc(pred.x)
	return(pred.x)

}


get.fs.pred <- function(x.params, feats, outfile="fs.x", downsample=T)  {
	fs.x <- fselect(x.params, feat.names=feats, downsample=downsample)
	save(fs.x, file=paste(outfile, ".fs", sep=""))
	best.feats.auroc <- fs.x[[1]] 
	gamma <- fs.x[[4]][order(auroc,decreasing=T)][1]$gamma
	co <- fs.x[[4]][order(auroc,decreasing=T)][1]$cost
	print(fs.x[[4]])
	fs.pred <- rs.cv(x.params,use.svm=T,co=co,gamma=gamma,downsample=downsample,features=best.feats.auroc, probabilities=T)
	setnames(fs.pred, "1", "prob")
	plot.roc(fs.pred)
	save(fs.pred, file=outfile)

	return(list(fs.pred=fs.pred, best.feats=best.feats.auroc))
}

main <- function() {

	#### WORDS #############################################################
	word.pros <- get.word.pros()
	save(word.pros, file="word.pros.dt")

	context.words <- get.word.context(word.pros)
	xprospn <- get.pros.feat(word.pros, context.words) 
	xprospn <- data.table(xprospn, is.rsturn=(xprospn$num.rswords > 0)) 
	save(xprospn, file="xprospn.dt")
	
	load("word.pros.dt")
	load("xprospn.dt")

	#### TURN EXPERIMENTS  #############################################################
	## 
	## See 
	## turn-results.r:  no RS turn segments vs RS segments
	## turn.nors-results.r:  Whether a turn contains any RS or not
	## turn.i-results.r: I vs no-RS 
	## rpros-results.r: D v I prosody

	#### BOUNDARY EXPERIMENTS #############################################################
	## See:
	## re-results.r: RS end detection, only look at turns known to contain RS
	## re-all-results.r: RS end detection, look at all turns 
	## rs-results.r: RS start detection, only look at turns known to contain RS
	## rs-all-results.r: RS start detection, look at all turns 




}
