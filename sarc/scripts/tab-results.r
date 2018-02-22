### This is just some copying and pasting
### to help keep track of results tables 

filenames <- list.files("../results/", pattern="fs.*.[^s]$")

fsx <- NULL 
for (filename in filenames) {
	print(filename)
	infile <- paste("../results/", filename, sep="")
	x <- load(infile, verbose=T)
	fs.pred <- get(x)
	
	u <- data.table(filename, plot.roc(fs.pred))		
	fsx <- rbindlist(list(fsx, u))
} 

xtable(rbindlist(list(fsx[res0.label==1][,list(filename, auroc, res0.precision, res0.recall, res0.F1, res.thr.F1)][grep("\\.turn\\.", filename)],
fsx[res0.label==1][,list(filename, auroc, res0.precision, res0.recall, res0.F1, res.thr.F1)][grep("\\.rs\\.", filename)],
fsx[res0.label==1][,list(filename, auroc, res0.precision, res0.recall, res0.F1, res.thr.F1)][grep("\\.re\\.", filename)])))


xtable(fsx[res0.label==1][,list(filename, auroc, res0.F1, res.thr.F1)][grep("fs.re\\.", filename)])
xtable(fsx[res0.label==1][,list(filename, auroc, res0.F1, res.thr.F1)][grep("fs.rs\\.", filename)])
xtable(fsx[res0.label==1][,list(filename, auroc, res0.F1, res.thr.F1)][grep("fs.turn\\.", filename)])

filenames <- list.files("../results/", pattern="pred.*.[^e]$")
predx <- NULL 
for (filename in filenames) {
	print(filename)
	infile <- paste("../results/", filename, sep="")
	x <- load(infile, verbose=T)
	fs.pred <- get(x)
	
	u <- data.table(filename, plot.roc(fs.pred))		
	predx <- rbindlist(list(predx, u))
} 

predx[res0.label==1][,list(filename, auroc, res0.F1, res.thr.F1)][grep("\\.re\\.", filename)]
predx[res0.label==1][,list(filename, auroc, res0.F1, res.thr.F1)][grep("\\.rs\\.", filename)]
predx[res0.label==1][,list(filename, auroc, res0.F1, res.thr.F1)][grep("\\.turn\\.", filename)]

xtable(rbindlist(list(predx[res0.label==1][,list(filename, auroc, res0.precision, res0.recall, res0.F1, res.thr.F1)][grep("\\.turn\\.", filename)],
predx[res0.label==1][,list(filename, auroc, res0.precision, res0.recall, res0.F1, res.thr.F1)][grep("\\.rs\\.", filename)],
predx[res0.label==1][,list(filename, auroc, res0.precision, res0.recall, res0.F1, res.thr.F1)][grep("\\.re\\.", filename)])))

