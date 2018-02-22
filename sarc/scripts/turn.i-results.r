## Turn experiments: indirect vs no reported speech 

source("../scripts/collate-word-feats.r")

load("word.pros.dt")
load("xprospn.dt")

turn.pros <- get.turn.pros(word.pros)
rs.pros <- get.rs.pros(word.pros, turn.pros)
xturn <- get.turn.data(turn.pros, rs.pros, xprospn)
xturn.ind <- xturn[rstype %in% c("I", "N")] 

save(xturn.ind, file="xturn.ind")
xturn.ind.params.all <- train.test.turn(xturn.ind)
save(xturn.ind.params.all, file="xturn.ind.params.all")

turn.ind.feats <- names(xturn.ind.params.all[["x0.data"]])
print(turn.ind.feats)

turn.ind.feats <- turn.ind.feats[!(turn.ind.feats %in% c("pdur","ndur","pgap","ngap"))]
turn.ind.feats <- grep("^d[pn]", turn.ind.feats, invert=T, value=T)

turn.ind.pred.all <- pred.tune(xturn.ind.params.all, feats=turn.ind.feats, outfile="pred.turn.ind.all")
fs.turn.ind.all <- get.fs.pred(xturn.ind.params.all, feats=turn.ind.feats, outfile="fs.turn.ind.all")

## only F0 and Intensity features
#pcontext <- turn.ind.feats[grep("(F0|I0)",turn.ind.feats)]
#turn.ind.pred.pcontext <- pred.tune(xturn.ind.params.all, feats=pcontext, outfile="pred.turn.ind.pcontext")
#fs.turn.ind.pcontext <- get.fs.pred(xturn.ind.params.all, feats=pcontext, outfile="fs.turn.ind.pcontext")

## F0 and Int, no context
pros <- turn.ind.feats[grep("(F0|I0)",turn.ind.feats)]
turn.ind.pred.pros <- pred.tune(xturn.ind.params.all, feats=pros, outfile="pred.turn.ind.pros")
fs.turn.ind.pros <- get.fs.pred(xturn.ind.params.all, feats=pros, outfile="fs.turn.ind.pros")

#lex <- grep("^n\\.", turn.ind.feats, value=T)
#turn.ind.pred.lex <- pred.tune(xturn.ind.params.all, feats=lex, outfile="pred.turn.ind.lex")
#fs.turn.ind.lex <- get.fs.pred(xturn.ind.params.all, feats=lex, outfile="fs.turn.ind.lex")
#
#nonlex <- grep("^n\\.", turn.ind.feats, value=T, invert=T)
#turn.ind.pred.nonlex <- pred.tune(xturn.ind.params.all, feats=nonlex, outfile="pred.turn.ind.nonlex")
#fs.turn.ind.nonlex <- get.fs.pred(xturn.ind.params.all, feats=nonlex, outfile="fs.turn.ind.nonlex")


############################################################################
### only Intensity features
#pi0 <- turn.ind.feats[grep("I0",turn.ind.feats)]
#turn.ind.pred.pi0 <- pred.tune(xturn.ind.params.all, feats=pi0, outfile="pred.turn.ind.pi0")
#
### only F0
#pf0 <- turn.ind.feats[grep("F0",turn.ind.feats)]
#turn.ind.pred.pf0 <- pred.tune(xturn.ind.params.all, feats=pf0, outfile="pred.turn.ind.pf0")
#fs.turn.ind.pf0 <- get.fs.pred(xturn.ind.params.all, feats=pf0, outfile="fs.turn.ind.pf0")
#
### only Intensity features
#ti0 <- pros[grep("I0",pros)]
#turn.ind.pred.ti0 <- pred.tune(xturn.ind.params.all, feats=ti0, outfile="pred.turn.ind.ti0")
#
### only F0
#tf0 <- pros[grep("F0",pros)]
#turn.ind.pred.tf0 <- pred.tune(xturn.ind.params.all, feats=tf0, outfile="pred.turn.ind.tf0")
#fs.turn.ind.tf0 <- get.fs.pred(xturn.ind.params.all, feats=tf0, outfile="fs.turn.ind.tf0")
#
###########################################################################
# Models
###########################################################################

m.turn.ind <- glmer(is.rs ~  (1 | spk)
		+ to.zscore(mean.normF0) + to.zscore(mean.normI0)
		+ to.zscore(range.normF0) + to.zscore(range.normI0)
		+ to.zscore(slope.normF0) + to.zscore(slope.normI0)
		+ to.zscore(intern.pause)
		+ to.zscore(abs(dur))
		, data=xturn.ind,  family = binomial(link = "logit"))

xtable(get.fixef(m.turn.ind, roundval=2))

sigpros <- c("range.normF0","slope.normF0","range.normI0","slope.normI0","intern.pause")
turn.ind.pred.sigpros <- pred.tune(xturn.ind.params.all, feats=sigpros, outfile="pred.turn.ind.sigpros")
