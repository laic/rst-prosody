## Turn prosody experiments RS vs non-RS turns
source("../scripts/collate-word-feats.r")

load("word.pros.dt")
load("xprospn.dt")

## turn features
turn.pros <- get.turn.pros(word.pros)
rs.pros <- get.rs.pros(word.pros, turn.pros)
xturn <- get.turn.data(turn.pros, rs.pros, xprospn)
save(xturn, file="xturn")

## partitions
xturn.params.all <- train.test.turn(xturn)
save(xturn.params.all, file="xturn.params.all")

turn.feats <- names(xturn.params.all[["x0.data"]])
print(turn.feats)

## turn intrinsic features only
turn.feats <- turn.feats[!(turn.feats %in% c("pdur","ndur","pgap","ngap"))]
turn.feats <- grep("^d[pn]", turn.feats, invert=T, value=T)

turn.pred.all <- pred.tune(xturn.params.all, feats=turn.feats, outfile="pred.turn.all")
fs.turn.all <- get.fs.pred(xturn.params.all, feats=turn.feats, outfile="fs.turn.all")

## only F0 and Intensity features
#pcontext <- turn.feats[grep("(F0|I0)",turn.feats)]
#turn.pred.pcontext <- pred.tune(xturn.params.all, feats=pcontext, outfile="pred.turn.pcontext")
#fs.turn.pcontext <- get.fs.pred(xturn.params.all, feats=pcontext, outfile="fs.turn.pcontext")

## F0 and Int, no context
pros <- turn.feats[grep("(F0|I0)",turn.feats)]
turn.pred.pros <- pred.tune(xturn.params.all, feats=pros, outfile="pred.turn.pros")
fs.turn.pros <- get.fs.pred(xturn.params.all, feats=pros, outfile="fs.turn.pros")

#lex <- grep("^n\\.", turn.feats, value=T)
#turn.pred.lex <- pred.tune(xturn.params.all, feats=lex, outfile="pred.turn.lex")
#fs.turn.lex <- get.fs.pred(xturn.params.all, feats=lex, outfile="fs.turn.lex")
#
#nonlex <- grep("^n\\.", turn.feats, value=T, invert=T)
#turn.pred.nonlex <- pred.tune(xturn.params.all, feats=nonlex, outfile="pred.turn.nonlex")
#fs.turn.nonlex <- get.fs.pred(xturn.params.all, feats=nonlex, outfile="fs.turn.nonlex")


############################################################################
### only Intensity features
#pi0 <- turn.feats[grep("I0",turn.feats)]
#turn.pred.pi0 <- pred.tune(xturn.params.all, feats=pi0, outfile="pred.turn.pi0")
#
### only F0
#pf0 <- turn.feats[grep("F0",turn.feats)]
#turn.pred.pf0 <- pred.tune(xturn.params.all, feats=pf0, outfile="pred.turn.pf0")
#fs.turn.pf0 <- get.fs.pred(xturn.params.all, feats=pf0, outfile="fs.turn.pf0")
#
### only Intensity features
#ti0 <- pros[grep("I0",pros)]
#turn.pred.ti0 <- pred.tune(xturn.params.all, feats=ti0, outfile="pred.turn.ti0")
#
### only F0
#tf0 <- pros[grep("F0",pros)]
#turn.pred.tf0 <- pred.tune(xturn.params.all, feats=tf0, outfile="pred.turn.tf0")
#fs.turn.tf0 <- get.fs.pred(xturn.params.all, feats=tf0, outfile="fs.turn.tf0")
#
###########################################################################
# Models
###########################################################################

m.turn <- glmer(is.rs ~  (1 | spk)
		+ to.zscore(mean.normF0) + to.zscore(mean.normI0)
		+ to.zscore(range.normF0) + to.zscore(range.normI0)
		+ to.zscore(slope.normF0) + to.zscore(slope.normI0)
		+ to.zscore(intern.pause)
		+ to.zscore(abs(dur))
		, data=xturn,  family = binomial(link = "logit"))

xtable(get.fixef(m.turn, roundval=2))

sigpros <- c("range.normF0","slope.normF0","range.normI0","slope.normI0","intern.pause")
turn.pred.sigpros <- pred.tune(xturn.params.all, feats=sigpros, outfile="pred.turn.sigpros")
