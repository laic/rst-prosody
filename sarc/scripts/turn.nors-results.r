## Experiments : turn contains RS or not?

library(xtable)
source("../scripts/proc-lme.r")
source("../scripts/collate-word-feats.r")

load("word.pros.dt")
load("xprospn.dt")

turn.pros <- get.turn.pros(word.pros)

xturn.nors <- get.turn.data.nors(turn.pros, xprospn)
save(xturn.nors, file="xturn.nors")
xturn.nors.params.all <- train.test.turn(xturn.nors)
save(xturn.nors.params.all, file="xturn.nors.params.all")

turn.nors.feats <- names(xturn.nors.params.all[["x0.data"]])
print(turn.nors.feats)

turn.nors.pred.all <- pred.tune(xturn.nors.params.all, feats=turn.nors.feats, outfile="pred.turn.nors.all")
fs.turn.nors.all <- get.fs.pred(xturn.nors.params.all, feats=turn.nors.feats, outfile="fs.turn.nors.all")

nocontext <- grep("^[pnd][pndg]", turn.nors.feats, value=T, invert=T)
turn.nors.pred.nocontext <- pred.tune(xturn.nors.params.all, feats=nocontext, outfile="pred.turn.nors.nocontext")
fs.turn.nors.nocontext <- get.fs.pred(xturn.nors.params.all, feats=nocontext, outfile="fs.turn.nors.nocontext")


# only F0 and Intensity features
pcontext <- turn.nors.feats[grep("(F0|I0)",turn.nors.feats)]
turn.nors.pred.pcontext <- pred.tune(xturn.nors.params.all, feats=pcontext, outfile="pred.turn.nors.pcontext")
fs.turn.nors.pcontext <- get.fs.pred(xturn.nors.params.all, feats=pcontext, outfile="fs.turn.nors.pcontext")

## F0 and Int, no context
pros <- pcontext[grep("(F0|I0)",pcontext)]
turn.nors.pred.pros <- pred.tune(xturn.nors.params.all, feats=pros, outfile="pred.turn.nors.pros")
fs.turn.nors.pros <- get.fs.pred(xturn.nors.params.all, feats=pros, outfile="fs.turn.nors.pros")

#lex <- grep("^n\\.", turn.nors.feats, value=T)
#turn.nors.pred.lex <- pred.tune(xturn.nors.params.all, feats=lex, outfile="pred.turn.nors.lex")
#fs.turn.nors.lex <- get.fs.pred(xturn.nors.params.all, feats=lex, outfile="fs.turn.nors.lex")
#
#nonlex <- grep("^n\\.", turn.nors.feats, value=T, invert=T)
#turn.nors.pred.nonlex <- pred.tune(xturn.nors.params.all, feats=nonlex, outfile="pred.turn.nors.nonlex")
#fs.turn.nors.nonlex <- get.fs.pred(xturn.nors.params.all, feats=nonlex, outfile="fs.turn.nors.nonlex")


############################################################################
### only Intensity features
#pi0 <- turn.nors.feats[grep("I0",turn.nors.feats)]
#turn.nors.pred.pi0 <- pred.tune(xturn.nors.params.all, feats=pi0, outfile="pred.turn.nors.pi0")
#
### only F0
#pf0 <- turn.nors.feats[grep("F0",turn.nors.feats)]
#turn.nors.pred.pf0 <- pred.tune(xturn.nors.params.all, feats=pf0, outfile="pred.turn.nors.pf0")
#fs.turn.nors.pf0 <- get.fs.pred(xturn.nors.params.all, feats=pf0, outfile="fs.turn.nors.pf0")
#
### only Intensity features
#ti0 <- pros[grep("I0",pros)]
#turn.nors.pred.ti0 <- pred.tune(xturn.nors.params.all, feats=ti0, outfile="pred.turn.nors.ti0")
#
### only F0
#tf0 <- pros[grep("F0",pros)]
#turn.nors.pred.tf0 <- pred.tune(xturn.nors.params.all, feats=tf0, outfile="pred.turn.nors.tf0")
#fs.turn.nors.tf0 <- get.fs.pred(xturn.nors.params.all, feats=tf0, outfile="fs.turn.nors.tf0")


############################################################################

m.turn.nors <- glmer(is.rs ~  (1 | spk)
                + to.zscore(mean.normF0) + to.zscore(mean.normI0)
                + to.zscore(range.normF0) + to.zscore(range.normI0)
                #+ to.zscore(sd.normF0) + to.zscore(.normI0)
                + to.zscore(slope.normF0) + to.zscore(slope.normI0)
                + to.zscore(intern.pause)
                + to.zscore(abs(dur))
                , data=xturn.nors,  family = binomial(link = "logit"))
display(m.turn.nors) 
xtable(get.fixef(m.turn.nors, roundval=2))

sigpros <- c("dur")
turn.nors.pred.sigpros <- pred.tune(xturn.nors.params.all, feats=sigpros, outfile="pred.turn.nors.sigpros")
