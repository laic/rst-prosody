## Experiments using all turns, not just those containing
## attributed speech

source("../scripts/collate-word-feats.r")

load("xprospn.dt")
xea.params.all <- train.test.re.all(xprospn, rsturn.only=F)
save(xea.params.all, file="xea.params.all")

rea.feats <- names(xea.params.all[["x0.data"]])
rea.feats <- rea.feats[!(rea.feats %in% c("p.thought","p.parc","p.said","p.rs20"))]

rea.pread.all <- pred.tune(xea.params.all, feats=rea.feats, outfile="pred.rea.all")
fs.rea.all <- get.fs.pred(xea.params.all, feats=rea.feats, outfile="fs.rea.all")

## only F0 and Intensity features
pcontext <- rea.feats[grep("(F0|I0)",rea.feats)]
rea.pred.pcontext <- pred.tune(xea.params.all, feats=pcontext, outfile="pred.rea.pcontext")
fs.rea.pcontext <- get.fs.pred(xea.params.all, feats=pcontext, outfile="fs.rea.pcontext")

## F0 and Int, no context
pros <- pcontext[grep("^d", pcontext, invert=T)]
rea.pred.pros <- pred.tune(xea.params.all, feats=pros, outfile="pred.rea.pros")
fs.rea.pros <- get.fs.pred(xea.params.all, feats=pros, outfile="fs.rea.pros")

## Lexical only
lex <- grep("^n\\.", rea.feats, value=T)
rea.pred.lex <- pred.tune(xea.params.all, feats=lex, outfile="pred.rea.lex")
fs.rea.lex <- get.fs.pred(xea.params.all, feats=lex, outfile="fs.rea.lex")

## Nonlexical only
nonlex <- grep("^n\\.", rea.feats, value=T, invert=T)
rea.pred.nonlex <- pred.tune(xea.params.all, feats=nonlex, outfile="pred.rea.nonlex")
fs.rea.nonlex <- get.fs.pred(xea.params.all, feats=nonlex, outfile="fs.rea.nonlex")


###########################################################################


        m.end <- glmer(is.rsend ~ (1 | spk)
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
                        , data=xprospn,  family = binomial(link = "logit"))


###########################################################################
## only Intensity features
pi0 <- rea.feats[grep("I0",rea.feats)]
rea.pred.pi0 <- pred.tune(xea.params.all, feats=pi0, outfile="pred.rea.pi0")

## only F0
pf0 <- rea.feats[grep("F0",rea.feats)]
rea.pred.pf0 <- pred.tune(xea.params.all, feats=pf0, outfile="pred.rea.pf0")
fs.rea.pf0 <- get.fs.pred(xea.params.all, feats=pf0, outfile="fs.rea.pf0")

## only Intensity features
ti0 <- pros[grep("I0",pros)]
rea.pred.ti0 <- pred.tune(xea.params.all, feats=ti0, outfile="pred.rea.ti0")

## only F0
tf0 <- pros[grep("F0",pros)]
rea.pred.tf0 <- pred.tune(xea.params.all, feats=tf0, outfile="pred.rea.tf0")
fs.rea.tf0 <- get.fs.pred(xea.params.all, feats=tf0, outfile="fs.rea.tf0")

## `Significant' features from logistic regression
sigpros <- c("range.normF0","range.normI0","dp.range.I0", "dn.mean.I0","dn.range.I0")
rea.pred.sigpros <- pred.tune(xea.params.all, feats=sigpros, outfile="pred.rea.sigpros")
