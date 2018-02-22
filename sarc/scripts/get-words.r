library(data.table)
source("../scripts/basics.r")

args=(commandArgs(TRUE))
print(args)
if(length(args)==0){ stop("No arguments supplied. Exiting.") }


filename <- args[1]
print(filename)

x <- data.table(read.delim(filename, header=F, sep="\t"))
setnames(x, c("tier", "label", "starttime", "endtime"))
x <- x[label != "_NA_"]
x <- x[order(starttime)]


conv <- strsplit(basename(filename), split="_")[[1]][1]
spk <- tail(strsplit(basename(filename), split="\\.")[[1]],2)[1]
spk <- gsub("[^0-9]", "", spk)

words <- data.table(conv=conv, xid=paste(conv,spk, sep="-"), 
		niteid=paste(conv, ".", spk, ".words", 1:nrow(x), sep=""),
		spk=spk, wstarts=x$starttime, wends=x$endtime, word=x$label)

outdir <- dirname(filename)
write.table(words, file=paste(outdir, "/../words/", conv, "0", spk, ".words.txt", sep=""))
