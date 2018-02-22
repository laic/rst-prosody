library(xtable)
source("../scripts/proc-lme.r")
source("../scripts/collate-word-feats.r")

load("xprospn.dt", verbose=T)

xs.list <- get.compare.groups(xprospn)
save(xs.list, file="x.compare.list")

x.rs.pword <- xs.list[["x.rs.pword"]]
x.rs.tstart<- xs.list[["x.rs.tstart"]]
x.rs.med <- xs.list[["x.rs.med"]]

m.start.tstart <- glmer(is.rsstart ~  (1 | spk)
		+ to.zscore(mean.normF0) + to.zscore(mean.normI0)
                + to.zscore(range.normF0) + to.zscore(range.normI0)
                + to.zscore(mean.normF0 - p.mean.normF0)
                + to.zscore(range.normF0 - p.range.normF0)
                + to.zscore(mean.normI0 - p.mean.normI0)
                + to.zscore(range.normI0 - p.range.normI0)
                + to.zscore(mean.normF0 - n.mean.normF0)
                + to.zscore(range.normF0 - n.range.normF0)
                + to.zscore(mean.normI0 - n.mean.normI0)
                + to.zscore(range.normI0 - n.range.normI0)
		, data=x.rs.tstart,  family = binomial(link = "logit"))
display(m.start.tstart)
u <- get.fixef(m.start.tstart)
u[ymin > 0]
u[ymax < 0]

m.start.med <- glmer(is.rsstart ~ (1 | spk)
		+ to.zscore(mean.normF0) + to.zscore(mean.normI0)
                + to.zscore(range.normF0) + to.zscore(range.normI0)
                + to.zscore(mean.normF0 - p.mean.normF0)
                + to.zscore(range.normF0 - p.range.normF0)
                + to.zscore(mean.normI0 - p.mean.normI0)
                + to.zscore(range.normI0 - p.range.normI0)
                + to.zscore(mean.normF0 - n.mean.normF0)
                + to.zscore(range.normF0 - n.range.normF0)
                + to.zscore(mean.normI0 - n.mean.normI0)
                + to.zscore(range.normI0 - n.range.normI0)
		, data=x.rs.med,  family = binomial(link = "logit"))
display(m.start.med)
u <- get.fixef(m.start.med)
u[ymin > 0]
u[ymax < 0]

x.re.med=xs.list[["x.re.med"]]
x.re.tend=xs.list[["x.re.tend"]]

m.end.tend <- glmer(is.rsend ~ (1 | spk)
		+ to.zscore(mean.normF0) + to.zscore(mean.normI0)
                + to.zscore(range.normF0) + to.zscore(range.normI0)
                + to.zscore(mean.normF0 - p.mean.normF0)
                + to.zscore(range.normF0 - p.range.normF0)
                + to.zscore(mean.normI0 - p.mean.normI0)
                + to.zscore(range.normI0 - p.range.normI0)
                + to.zscore(mean.normF0 - n.mean.normF0)
                + to.zscore(range.normF0 - n.range.normF0)
                + to.zscore(mean.normI0 - n.mean.normI0)
                + to.zscore(range.normI0 - n.range.normI0)
		, data=x.re.tend,  family = binomial(link = "logit"))

display(m.end.tend)
u <- get.fixef(m.end.tend)
u[ymin > 0]
u[ymax < 0]


m.end.med <- glmer(is.rsend ~  (1 | spk)
		+ to.zscore(mean.normF0) + to.zscore(mean.normI0)
                + to.zscore(range.normF0) + to.zscore(range.normI0)
                + to.zscore(mean.normF0 - p.mean.normF0)
                + to.zscore(range.normF0 - p.range.normF0)
                + to.zscore(mean.normI0 - p.mean.normI0)
                + to.zscore(range.normI0 - p.range.normI0)
                + to.zscore(mean.normF0 - n.mean.normF0)
                + to.zscore(range.normF0 - n.range.normF0)
                + to.zscore(mean.normI0 - n.mean.normI0)
                + to.zscore(range.normI0 - n.range.normI0)
		, data=x.re.med,  family = binomial(link = "logit"))
display(m.end.med)
u <- get.fixef(m.end.med)
u[ymin > 0]
u[ymax < 0]


