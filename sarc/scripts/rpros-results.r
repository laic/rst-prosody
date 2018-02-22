## Experiments using turn level features

source("../scripts/collate-word-feats.r")
load("word.pros.dt")
load("../results/xprospn.dt")

turn.pros <- get.turn.pros(word.pros)
rs.pros <- get.rs.pros(word.pros, turn.pros)

## D v I experiments?
xrs <- get.rspros.data(rs.pros[rstype %in% c("D","I")], xprospn)
save(xrs, file="xrs")
xrs.params.all <- train.test.turn(xrs)
save(xrs.params.all, file="xrs.params.all")

rsp.feats <- names(xrs.params.all[["x0.data"]])
print(rsp.feats)

rsp.feats <- rsp.feats[!(rsp.feats %in% c("pdur","ndur","pgap","ngap"))]
rsp.feats <- grep("^d[pn]", rsp.feats, invert=T, value=T)

rsp.pred.all <- pred.tune(xrs.params.all, feats=rsp.feats, outfile="pred.rsp.all", downsample=F)
fs.rsp.all <- get.fs.pred(xrs.params.all, feats=rsp.feats, outfile="fs.rsp.all", downsample=F)


## only F0 and Intensity features
#pcontext <- rsp.feats[grep("(F0|I0)",rsp.feats)]
#rsp.pred.pcontext <- pred.tune(xrs.params.all, feats=pcontext, outfile="pred.rsp.pcontext")
#fs.rsp.pcontext <- get.fs.pred(xrs.params.all, feats=pcontext, outfile="fs.rsp.pcontext")

## F0 and Int, no context
pros <- rsp.feats[grep("(F0|I0)",rsp.feats)]
rsp.pred.pros <- pred.tune(xrs.params.all, feats=pros, outfile="pred.rsp.pros", downsample=F)
fs.rsp.pros <- get.fs.pred(xrs.params.all, feats=pros, outfile="fs.rsp.pros", downsample=F)

sigpros <- c("range.normF0")
rsp.pred.sigpros <- pred.tune(xrs.params.all, feats=sigpros, outfile="pred.rsp.sigpros", downsample=F)

#lex <- grep("^n\\.", rsp.feats, value=T)
#rsp.pred.lex <- pred.tune(xrs.params.all, feats=lex, outfile="pred.rsp.lex")
#fs.rsp.lex <- get.fs.pred(xrs.params.all, feats=lex, outfile="fs.rsp.lex")
#
#nonlex <- grep("^n\\.", rsp.feats, value=T, invert=T)
#rsp.pred.nonlex <- pred.tune(xrs.params.all, feats=nonlex, outfile="pred.rsp.nonlex")
#fs.rsp.nonlex <- get.fs.pred(xrs.params.all, feats=nonlex, outfile="fs.rsp.nonlex")


############################################################################
### only Intensity features
#pi0 <- rsp.feats[grep("I0",rsp.feats)]
#rsp.pred.pi0 <- pred.tune(xrs.params.all, feats=pi0, outfile="pred.rsp.pi0")
#
### only F0
#pf0 <- rsp.feats[grep("F0",rsp.feats)]
#rsp.pred.pf0 <- pred.tune(xrs.params.all, feats=pf0, outfile="pred.rsp.pf0")
#fs.rsp.pf0 <- get.fs.pred(xrs.params.all, feats=pf0, outfile="fs.rsp.pf0")
#
### only Intensity features
#ti0 <- pros[grep("I0",pros)]
#rsp.pred.ti0 <- pred.tune(xrs.params.all, feats=ti0, outfile="pred.rsp.ti0")
#
### only F0
#tf0 <- pros[grep("F0",pros)]
#rsp.pred.tf0 <- pred.tune(xrs.params.all, feats=tf0, outfile="pred.rsp.tf0")
#fs.rsp.tf0 <- get.fs.pred(xrs.params.all, feats=tf0, outfile="fs.rsp.tf0")
#
###########################################################################
# Models
###########################################################################

m.rsp <- glmer(is.rs ~  (1 | spk)
		+ to.zscore(mean.normF0) + to.zscore(mean.normI0)
		+ to.zscore(range.normF0) + to.zscore(range.normI0)
		+ to.zscore(slope.normF0) + to.zscore(slope.normI0)
		+ to.zscore(intern.pause)
		+ to.zscore(abs(dur))
		, data=xrs,  family = binomial(link = "logit"))

xtable(get.fixef(m.rsp, roundval=2))

