source("../scripts/collate-word-feats.r")

#load("xprospn.dt")
#xs.params.all <- train.test.rs.all(xprospn)
#save(xs.params.all, file="xs.params.all")

load("xs.params.all")
rs.feats <- names(xs.params.all[["x0.data"]])

rs.feats <- rs.feats[!(rs.feats %in% c("n.but","n.and","n.re20"))]

#rs.pred.all <- pred.tune(xs.params.all, feats=rs.feats, outfile="pred.rs.all.nods", downsample=F)
#fs.rs.all <- get.fs.pred(xs.params.all, feats=rs.feats, outfile="fs.rs.all.nods", downsample=F)
#
#rs.pred.all <- pred.tune(xs.params.all, feats=rs.feats, outfile="pred.rs.all.lr", downsample=T, use.svm=F)

rs.pred.all <- pred.tune(xs.params.all, feats=rs.feats, outfile="pred.rs.all", downsample=T)

rs.pred.all <- pred.tune(xs.params.all, feats=rs.feats, outfile="pred.rs.all.lin", downsample=T, use.svm=T, kernel="linear")

fs.rs.all <- get.fs.pred(xs.params.all, feats=rs.feats, outfile="fs.rs.all", downsample=T)

## only F0 and Intensity features
pcontext <- rs.feats[grep("(F0|I0)",rs.feats)]
rs.pred.pcontext <- pred.tune(xs.params.all, feats=pcontext, outfile="pred.rs.pcontext")
fs.rs.pcontext <- get.fs.pred(xs.params.all, feats=pcontext, outfile="fs.rs.pcontext")

## F0 and Int, no context
pros <- pcontext[grep("^d", pcontext, invert=T)]
rs.pred.pros <- pred.tune(xs.params.all, feats=pros, outfile="pred.rs.pros")
fs.rs.pros <- get.fs.pred(xs.params.all, feats=pros, outfile="fs.rs.pros")

lex <- grep("^p\\.", rs.feats, value=T)
rs.pred.lex <- pred.tune(xs.params.all, feats=lex, outfile="pred.rs.lex")
rs.pred.lex <- pred.tune(xs.params.all, feats=lex, outfile="pred.rs.lex.lr", use.svm=F)
fs.rs.lex <- get.fs.pred(xs.params.all, feats=lex, outfile="fs.rs.lex")

nonlex <- grep("^[p]\\.", rs.feats, value=T, invert=T)
rs.pred.nonlex <- pred.tune(xs.params.all, feats=nonlex, outfile="pred.rs.nonlex")
rs.pred.nonlex <- pred.tune(xs.params.all, feats=nonlex, outfile="pred.rs.nonlex.lr", use.svm=F)
fs.rs.nonlex <- get.fs.pred(xs.params.all, feats=nonlex, outfile="fs.rs.nonlex")

sigpros <- c("mean.normF0", "mean.normI0", "range.normI0", "dp.range.F0", "dp.mean.I0")
rs.pred.sigpros <- pred.tune(xs.params.all, feats=sigpros, outfile="pred.rs.sigpros", downsample=T)
rs.pred.sigpros <- pred.tune(xs.params.all, feats=sigpros, outfile="pred.rs.sigpros.nods", downsample=F)
rs.pred.sigpros <- pred.tune(xs.params.all, feats=sigpros, outfile="pred.rs.sigpros.lr", use.svm=F)
fs.rs.sigpros <- get.fs.pred(xs.params.all, feats=sigpros, outfile="fs.rs.sigpros")

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
		+ to.zscore(-(n.mean.normF0 - mean.normF0))
		+ to.zscore(-(n.range.normF0 - range.normF0))
		+ to.zscore(-(n.mean.normI0 - mean.normI0))
		+ to.zscore(-(n.range.normI0 - range.normI0))
		#+ to.zscore(pgap)
		#+ to.zscore(ngap)
		#+ p.parc
		#+ p.said
		#+ p.thought
		#+ p.rs20 
		, data=xprospn[is.rsturn==T],  family = binomial(link = "logit"))
display(m.start)
u <- get.fixef(m.start)
u[ymin > 0]
u[ymax < 0]


###########################################################################
### only Intensity features
#pi0 <- rs.feats[grep("I0",rs.feats)]
#rs.pred.pi0 <- pred.tune(xs.params.all, feats=pi0, outfile="pred.rs.pi0")
#
### only F0
#pf0 <- rs.feats[grep("F0",rs.feats)]
#rs.pred.pf0 <- pred.tune(xs.params.all, feats=pf0, outfile="pred.rs.pf0")
#fs.rs.pf0 <- get.fs.pred(xs.params.all, feats=pf0, outfile="fs.rs.pf0")
#
### only Intensity features
#ti0 <- pros[grep("I0",pros)]
#rs.pred.ti0 <- pred.tune(xs.params.all, feats=ti0, outfile="pred.rs.ti0")
#
### only F0
#tf0 <- pros[grep("F0",pros)]
#rs.pred.tf0 <- pred.tune(xs.params.all, feats=tf0, outfile="pred.rs.tf0")
#fs.rs.tf0 <- get.fs.pred(xs.params.all, feats=tf0, outfile="fs.rs.tf0")
#

