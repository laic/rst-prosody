## Word boundary experiments 
source("../scripts/collate-word-feats.r")
load("xprospn.dt")

## Get test/train partitions
xe.params.all <- train.test.re.all(xprospn)
save(xe.params.all, file="xe.params.all")

## feature names
re.feats <- names(xe.params.all[["x0.data"]])

## Excluding lexical features
re.feats <- re.feats[!(re.feats %in% c("p.thought","p.parc","p.said","p.rs20"))]
re.pred.all <- pred.tune(xe.params.all, feats=re.feats, outfile="pred.re.all")
fs.re.all <- get.fs.pred(xe.params.all, feats=re.feats, outfile="fs.re.all")

## only F0 and Intensity features
pcontext <- re.feats[grep("(F0|I0)",re.feats)]
re.pred.pcontext <- pred.tune(xe.params.all, feats=pcontext, outfile="pred.re.pcontext")
fs.re.pcontext <- get.fs.pred(xe.params.all, feats=pcontext, outfile="fs.re.pcontext")

## F0 and Int, no context
pros <- pcontext[grep("^d", pcontext, invert=T)]
re.pred.pros <- pred.tune(xe.params.all, feats=pros, outfile="pred.re.pros")
fs.re.pros <- get.fs.pred(xe.params.all, feats=pros, outfile="fs.re.pros")

lex <- grep("^n\\.", re.feats, value=T)
re.pred.lex <- pred.tune(xe.params.all, feats=lex, outfile="pred.re.lex")
fs.re.lex <- get.fs.pred(xe.params.all, feats=lex, outfile="fs.re.lex")

nonlex <- grep("^n\\.", re.feats, value=T, invert=T)
re.pred.nonlex <- pred.tune(xe.params.all, feats=nonlex, outfile="pred.re.nonlex")
fs.re.nonlex <- get.fs.pred(xe.params.all, feats=nonlex, outfile="fs.re.nonlex")


###########################################################################
## only Intensity features
pi0 <- re.feats[grep("I0",re.feats)]
re.pred.pi0 <- pred.tune(xe.params.all, feats=pi0, outfile="pred.re.pi0")

## only F0
pf0 <- re.feats[grep("F0",re.feats)]
re.pred.pf0 <- pred.tune(xe.params.all, feats=pf0, outfile="pred.re.pf0")
fs.re.pf0 <- get.fs.pred(xe.params.all, feats=pf0, outfile="fs.re.pf0")

## only Intensity features
ti0 <- pros[grep("I0",pros)]
re.pred.ti0 <- pred.tune(xe.params.all, feats=ti0, outfile="pred.re.ti0")

## only F0
tf0 <- pros[grep("F0",pros)]
re.pred.tf0 <- pred.tune(xe.params.all, feats=tf0, outfile="pred.re.tf0")
fs.re.tf0 <- get.fs.pred(xe.params.all, feats=tf0, outfile="fs.re.tf0")


