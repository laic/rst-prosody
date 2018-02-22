library(data.table)

args=(commandArgs(TRUE))
if(length(args)==0){
        stop("No arguments supplied. Exiting")
}

#----------------------------------------------------------
print(args)
conv <- args[1]
datadir <- args[2]


filenames <- grep(conv, list.files(datadir, pattern="*.words.txt", full.names=T), value=T)
print(filenames)

x.list <- lapply(filenames, function(filename) {
		x <- data.table(read.table(filename))
		return(x)
	})

x.names <- sapply(x.list, function(x) {unique(x$xid)})
names(x.list) <- x.names


#conv <- x.list[[1]]$conv[1]
outdir <- dirname(datadir)
save(x.list, file=paste(datadir, "/", conv, ".conv.words", sep=""))
#write.table(x.list, file=paste(conv, ".conv.words.txt"))

