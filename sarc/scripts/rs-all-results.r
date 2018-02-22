source("../scripts/collate-word-feats.r")

load("xprospn.dt")
xsa.params.all <- train.test.rs.all(xprospn, rsturn.only=F)
save(xsa.params.all, file="xsa.params.all")

load("xsa.params.all")
rsa.feats <- names(xsa.params.all[["x0.data"]])

rsa.feats <- rsa.feats[!(rsa.feats %in% c("n.but","n.and","n.re20"))]

#rsa.pred.all <- pred.tune(xsa.params.all, feats=rsa.feats, outfile="pred.rsa.all.nods", downsample=F)
#fs.rsa.all <- get.fs.pred(xsa.params.all, feats=rsa.feats, outfile="fs.rsa.all.nods", downsample=F)
#
#rsa.pred.all <- pred.tune(xsa.params.all, feats=rsa.feats, outfile="pred.rsa.all.lr", downsample=T, use.svm=F)

#rsa.pred.all <- pred.tune(xsa.params.all, feats=rsa.feats, outfile="pred.rsa.all.lin", downsample=T, use.svm=T, kernel="linear")
rsa.pred.all <- pred.tune(xsa.params.all, feats=rsa.feats, outfile="pred.rsa.all", downsample=T)
fs.rsa.all <- get.fs.pred(xsa.params.all, feats=rsa.feats, outfile="fs.rsa.all", downsample=T)

## only F0 and Intensity features
pcontext <- rsa.feats[grep("(F0|I0)",rsa.feats)]
rsa.pred.pcontext <- pred.tune(xsa.params.all, feats=pcontext, outfile="pred.rsa.pcontext")
fs.rsa.pcontext <- get.fs.pred(xsa.params.all, feats=pcontext, outfile="fs.rsa.pcontext")

## F0 and Int, no context
pros <- pcontext[grep("^d", pcontext, invert=T)]
rsa.pred.pros <- pred.tune(xsa.params.all, feats=pros, outfile="pred.rsa.pros")
fs.rsa.pros <- get.fs.pred(xsa.params.all, feats=pros, outfile="fs.rsa.pros")

lex <- grep("^p\\.", rsa.feats, value=T)
rsa.pred.lex <- pred.tune(xsa.params.all, feats=lex, outfile="pred.rsa.lex")
rsa.pred.lex <- pred.tune(xsa.params.all, feats=lex, outfile="pred.rsa.lex.lr", use.svm=F)
fs.rsa.lex <- get.fs.pred(xsa.params.all, feats=lex, outfile="fs.rsa.lex")

nonlex <- grep("^[p]\\.", rsa.feats, value=T, invert=T)
rsa.pred.nonlex <- pred.tune(xsa.params.all, feats=nonlex, outfile="pred.rsa.nonlex")
rsa.pred.nonlex <- pred.tune(xsa.params.all, feats=nonlex, outfile="pred.rsa.nonlex.lr", use.svm=F)
fs.rsa.nonlex <- get.fs.pred(xsa.params.all, feats=nonlex, outfile="fs.rsa.nonlex")

sigpros <- c("mean.normF0", "mean.normI0", "range.normI0", "dp.range.F0", "dp.mean.I0")
rsa.pred.sigpros <- pred.tune(xsa.params.all, feats=sigpros, outfile="pred.rsa.sigpros", downsample=T)
rsa.pred.sigpros <- pred.tune(xsa.params.all, feats=sigpros, outfile="pred.rsa.sigpros.nods", downsample=F)
rsa.pred.sigpros <- pred.tune(xsa.params.all, feats=sigpros, outfile="pred.rsa.sigpros.lr", use.svm=F)
fs.rsa.sigpros <- get.fs.pred(xsa.params.all, feats=sigpros, outfile="fs.rsa.sigpros")



###########################################################################

rs20 <- unlevel(xprospn[is.rsstart==T,length(wid),by=p.word][order(V1, decreasing=T)][1:20]$p.word)
parc <- toupper(c("say","add","note","think","believe","tell","argue","expect","report","estimate"))

xpros <- data.table(xprospn, xprospn[,list( 
	pgap=(wstart-p.wend),
	ngap=(n.wstart-wend),
	p.parc= factor(p.word %in% parc),
	p.said= factor(p.word == "SAID"),
	p.thought= factor(p.word == "THOUGHT"),
	p.rs20= factor(p.word %in% rs20))])

xpros[is.na(p.thought)]$p.thought <- "FALSE"
xpros[is.na(p.said)]$p.said <- "FALSE"
	
m.start <- glmer(is.rsstart ~ (1 | spk)
		+ to.zscore(mean.normF0) + to.zscore(mean.normI0)
		+ to.zscore(range.normF0) + to.zscore(range.normI0)
		+ to.zscore(mean.normF0 - p.mean.normF0)
		+ to.zscore(range.normF0 - p.range.normF0)
		+ to.zscore(mean.normI0 - p.mean.normI0)
		+ to.zscore(range.normI0 - p.range.normI0)
		+ to.zscore(n.mean.normF0 - mean.normF0)
		+ to.zscore(n.range.normF0 - range.normF0)
		+ to.zscore(n.mean.normI0 - mean.normI0)
		+ to.zscore(n.range.normI0 - range.normI0)
		#+ to.zscore(pgap)
		#+ to.zscore(ngap)
		#+ p.parc
		#+ p.said
		#+ p.thought
		#+ p.rs20 
		, data=xpros,  family = binomial(link = "logit"))
display(m.start)



###########################################################################
### only Intensity features
#pi0 <- rs.feats[grep("I0",rs.feats)]
#rs.pred.pi0 <- pred.tune(xsa.params.all, feats=pi0, outfile="pred.rs.pi0")
#
### only F0
#pf0 <- rs.feats[grep("F0",rs.feats)]
#rs.pred.pf0 <- pred.tune(xsa.params.all, feats=pf0, outfile="pred.rs.pf0")
#fs.rs.pf0 <- get.fs.pred(xsa.params.all, feats=pf0, outfile="fs.rs.pf0")
#
### only Intensity features
#ti0 <- pros[grep("I0",pros)]
#rs.pred.ti0 <- pred.tune(xsa.params.all, feats=ti0, outfile="pred.rs.ti0")
#
### only F0
#tf0 <- pros[grep("F0",pros)]
#rs.pred.tf0 <- pred.tune(xsa.params.all, feats=tf0, outfile="pred.rs.tf0")
#fs.rs.tf0 <- get.fs.pred(xsa.params.all, feats=tf0, outfile="fs.rs.tf0")
#

