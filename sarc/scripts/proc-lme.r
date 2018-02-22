## Some helper functions for evaluating lme models

## Get group level effects in prediction
get.glevel.effect <- function(u, glevel, gname) {
#	u <- ranef(m)

	xf <- 0
	if ((glevel %in% rownames(u[[gname]]))) {
		xf <- u[[gname]][glevel,]
	} else {
		xf <- 0
	#	print("???")
	}

	return(xf)
}


## linear prediction    
get.qs.linear.pred <- function(m, newdata, corpus=F) {

        mm <- model.matrix(terms(m),data=newdata) 
        u <- mm %*% fixef(m)

	m.ranef <- ranef(m)
        role.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,role,"role")),by=list(pid)]$fx
        group.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,group,"group")),by=list(pid)]$fx
        meeting.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,meeting,"meeting")),by=list(pid)]$fx
	if (corpus==T) {
        	corpus.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,corpus,"corpus")),by=list(pid)]$fx
        	pred.fx  <- u + role.fx + group.fx + meeting.fx + corpus.fx
	} else {
        	pred.fx  <- u + role.fx
	}
        return(pred.fx)
}


## Out of date with respect to group level indicators?
get.eda.linear.pred <- function(m, newdata, corpus=F) {

        mm <- model.matrix(terms(m),data=newdata) 
        u <- mm %*% fixef(m)

	m.ranef <- ranef(m)
        annot.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,annot,"annot")),by=list(wid,annot)]$fx
        mtype.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,mtype,"mtype")),by=list(wid,annot)]$fx
	if (corpus==T) {
        	corpus.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,corpus,"corpus")),by=list(wid,annot)]$fx
        	pred.fx  <- u + annot.fx + mtype.fx + corpus.fx
	} else {
        	pred.fx  <- u + annot.fx + mtype.fx 
	}
        return(pred.fx)
}

## This needs to be generalized
get.eda.true.pred <- function(m, newdata, corpus=F, spk=F) {

	print(nrow(newdata))
	#print(terms(m))
        mm <- model.matrix(terms(m),data=newdata) 
	#print(nrow(mm))
        u <- mm %*% fixef(m)

	
	m.ranef <- ranef(m)

	if (spk) {
		print("HERE spk")	
		mtype.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,mtype,"mtype")),by=list(wid.spk,annot)]$fx

        	sum.fx  <- u + mtype.fx
		if ("annot" %in% m.ranef) {
			annot.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,annot,"annot")),by=list(wid.spk,annot)]$fx
			sum.fx <- sum.fx + annot.fx
		}
		if ("eda.annot" %in% m.ranef) {
			eda.annot.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,eda.annot,"eda.annot")),by=list(wid.spk,annot)]$fx
			sum.fx <- sum.fx + eda.annot.fx
		}
		if ("mgroup" %in% m.ranef) {
			mgroup.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,mgroup,"mgroup")),by=list(wid.spk,annot)]$fx
			sum.fx <- sum.fx + mgroup.fx
		}
		if ("corpus" %in% m.ranef) {
        		corpus.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,corpus,"corpus")),by=list(wid.spk,annot)]$fx
			sum.fx <- sum.fx + corpus.fx

		}
	} else {
		print("HERE")	
		mtype.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,mtype,"mtype")),by=list(wid,annot)]$fx
        	sum.fx  <- u + mtype.fx
		print(m.ranef)
		print(names(m.ranef))
		if ("annot" %in% m.ranef) {
			print("annot")
			annot.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,annot,"annot")),by=list(wid,annot)]$fx
			sum.fx <- sum.fx + annot.fx
		}
		if ("eda.annot" %in% m.ranef) {
			print("eda.annot")
			eda.annot.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,eda.annot,"eda.annot")),by=list(wid,annot)]$fx
			sum.fx <- sum.fx + eda.annot.fx
		}
		if ("mgroup" %in% m.ranef) {
			print("mgroup")
			mgroup.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,mgroup,"mgroup")),by=list(wid,annot)]$fx
			sum.fx <- sum.fx + mgroup.fx
		}
		if ("corpus" %in% m.ranef) {
			print("corpus")
        		corpus.fx <- newdata[,list(fx=get.glevel.effect(m.ranef,corpus,"corpus")),by=list(wid,annot)]$fx
			sum.fx <- sum.fx + corpus.fx

		}
	}

       	pred.fx  <- invlogit(sum.fx)

        return(pred.fx)
}


train.logit.model <- function(m, xdata, x.formula) {
	glmer(x.formula, data=xdata,family=binomial(link="logit"))
}

train.linear.model <- function(m, xdata, x.formula) {
	glmer(x.formula, data=xdata)
}

## Leave one out cross-validation
get.loocv.qs <- function(m, xdata, filename="curr.loocv.qs", corpus=F, linear=T) {
	print(filename)
	x <- xdata	
	gnames <- unique(xdata$group) 
	xpred <- NULL
	mlist <- list()	

	for (gname  in gnames) {
		print(gname)
		curr <- x[group==gname]
		if (linear) {
			curr.m <- train.linear.model(m, x[group != gname])
			curr.pred <- get.qs.linear.pred(curr.m, curr, corpus=corpus)
			xpred <- rbind(xpred, data.table(curr[,list(pid, role, meeting, group, A.centre)], 
						pred.val=curr.pred[,1])) 

		} else {
			print("Oh no, not yet!")	
			xpred <- curr 
		}
		mlist[[gname]] <- curr.m
	}	

	currcv <- list(xpred=xpred, mlist=mlist)
	if (!is.null(filename)) {
		print(filename)
		save(currcv, file=filename) 
	}		
	return(xpred)	
}

get.cv.qs <- function(m, xdata, nfolds=5, filename="curr.cv.qs", corpus=F, linear=T) {
	print(filename)
	ord <- sample.int(nrow(xdata))   
	nx <- ceiling(nrow(xdata)/nfolds)	
	fold.starts <- seq(1,nrow(xdata),by=nx)
	fold.ends <- c(fold.starts[2:length(fold.starts)]-1, nrow(xdata))
	folds <- data.table(fstart=fold.starts, fend=fold.ends)

	#x <- xdata
	x <- xdata[ord]	
	xpred <- NULL
	mlist <- list()	

	for (i in c(1:nrow(folds))) {
		print(folds[i])
		curr <- x[folds$fstart[i]:folds$fend[i]]
		if (linear) {
			curr.m <- train.linear.model(m, x[!(folds$fstart[i]:folds$fend[i])])
			curr.pred <- get.qs.linear.pred(curr.m, curr, corpus=corpus)
			xpred <- rbind(xpred, data.table(curr[,list(pid, role, meeting, group, A.centre)], 
						pred.val=curr.pred[,1])) 

		} else {
			print("Oh no, not yet!")	
			xpred <- curr 
		}
		mlist[[folds$fstart[i]]] <- curr.m
	}	

	currcv <- list(xpred=xpred, mlist=mlist, folds=folds, ord=ord)
	if (!is.null(filename)) {
		print(filename)
		#save(currcv, file=filename) 
	}		
	return(xpred)	
}

get.cv <- function(m, xdata, x.formula, nfolds=5, filename=NULL, corpus=F, linear=F, spk=F) {
	print(filename)
	ord <- sample.int(nrow(xdata))   
	nx <- ceiling(nrow(xdata)/nfolds)	
	fold.starts <- seq(1,nrow(xdata),by=nx)
	fold.ends <- c(fold.starts[2:length(fold.starts)]-1, nrow(xdata))
	folds <- data.table(fstart=fold.starts, fend=fold.ends)

	#x <- xdata
	x <- xdata[ord]	
	xpred <- NULL
	mlist <- list()	


	for (i in c(1:nrow(folds))) {
		print(folds[i])
		curr <- x[folds$fstart[i]:folds$fend[i]]
		print(nrow(curr))
		if (linear) {
			curr.m <- train.linear.model(m=m, xdata=x[!(folds$fstart[i]:folds$fend[i])], x.formula=x.formula)
			curr.pred <- get.eda.linear.pred(curr.m, curr, corpus=corpus)
			xpred <- rbind(xpred, data.table(curr[,list(wid,annot,corpus,eda.true,eda.time,link.eda)], 
						pred.val=curr.pred[,1])) 

		} else {
			curr.m <- train.logit.model(m=m, xdata=x[!(folds$fstart[i]:folds$fend[i])], x.formula=x.formula)
			print("Here")
			curr.pred <- get.eda.true.pred(curr.m, curr, corpus=corpus, spk=spk)
			xpred <- rbind(xpred, data.table(curr[,list(wid,annot,corpus,eda.true,eda.time,link.eda)], 
						logit.val=curr.pred[,1])) 
		}
		mlist[[folds$fstart[i]]] <- curr.m
	}	

	currcv <- list(xpred=xpred, mlist=mlist, folds=folds, ord=ord)
	if (!is.null(filename)) {
		print(filename)
		#save(currcv, file=filename) 
	}		
	return(xpred)	
}

get.cv.wrapper <- function(m, N, xdata, x.formula=formula(m), nfolds=10, corpus=F, linear=F, spk=F) {
	#get.cv(m, xdata, nfolds, filename=paste("/home/clai/kdata/ami/derived/R.outputs/cv.",N,sep=""), corpus=corpus)
	get.cv(m, xdata, x.formula, nfolds, filename=paste("cv.",N,sep=""), corpus=corpus, linear=linear, spk=spk)
}

get.cv.reps <- function(m, xdata, nfolds=5, nreps=10) {
	mclapply(c(1:nreps), get.cv.wrapper, m=m, xdata=xdata, nfolds=nfolds,  
	 	mc.cores=5)

}

get.f1 <- function(x, dep.var="eda.true") {
	if (!is.data.table(x)) {
		x <- data.table(x)
	}
	setnames(x, c(dep.var), c("eda.true"))
	if ("wid" %in% names(x) & !("niteid" %in% names(x))) {
		setnames(x, c("wid"), c("niteid"))
	}	
	ctab <- x[,length(niteid),by=list(eda.true, logit.val > 0.5)]
	baseline <- x[,length(niteid)/nrow(x),by=list(eda.true)]
	accuracy <- ctab[,sum(V1[eda.true == logit.val])/(sum(V1))]
	precision <- ctab[,V1[eda.true & logit.val]/(sum(V1[logit.val]))]
	if (length(precision)==0) {precision <- 0}
	recall <- ctab[,V1[eda.true & logit.val]/(sum(V1[eda.true]))]
	if (length(recall)==0) {recall <- 0}
	F1 <- 2* ((precision*recall)/(precision+recall))

	setnames(x, c("eda.true"), c(dep.var))

	return(list(f1=data.table(baseline, accuracy, precision=precision, recall=recall, F1=F1), ctab=ctab))
}


#---------------------------------------------------------------------
# Examine model parameters

## Get fixed effects of model m
get.fixef <- function(m, roundval=3) {
	u <- display(m)
	v0 <- round(data.table(estimate=u$coef, se=u$se, ymin=u$coef-2*u$se, ymax=u$coef+2*u$se), roundval) 
	v <- data.table(varx=names(u$coef), v0)
	return(v)
}

get.ranef.var <- function(m, varname) {

	u0 <- se.ranef(m)[[varname]]
	x0 <- ranef(m)[[varname]]

	x1 <- data.frame(varx=rownames(x0), x0)
	names(x1)[2] <- varname #"Intercept"
	x2 <- melt(x1)
	names(x2)  <- c("varx","varb", "estimate")

	u1 <- data.frame(varx=rownames(x0), u0)
	names(u1) <- names(x1)
	u2 <- melt(u1) 
	names(u2)  <- c("varx","varb", "se")

	v <- data.frame(x2, u2$se, ymin=x2$estimate-2*u2$se, ymax=x2$estimate+2*u2$se)
	return(v)
}

plot.fixef.est <- function(v, varname="Features") {
	p <- ggplot(v, aes(x=varx, y=estimate, ymin=ymin, ymax=ymax)) + geom_pointrange() + geom_hline(y=0, col="grey")
	p <- p + scale_x_discrete(varname) + theme(axis.text.x=element_text(angle=90, hjust=1)) 
	p

}
plot.ranef.var <- function(m, varname) {

        u0 <- se.ranef(m)[[varname]]
        x0 <- ranef(m)[[varname]]

        x1 <- data.frame(varx=rownames(x0), x0)
        names(x1)[2] <- varname #"Intercept"
        x2 <- melt(x1)
        names(x2)  <- c("varx","varb", "estimate")

        u1 <- data.frame(varx=rownames(x0), u0)
        names(u1) <- names(x1)
        u2 <- melt(u1)
        names(u2)  <- c("varx","varb", "se")

        v <- data.frame(x2, u2$se, ymin=x2$estimate-2*u2$se, ymax=x2$estimate+2*u2$se)


        p <- ggplot(v, aes(x=varx, y=estimate, ymin=ymin, ymax=ymax)) + geom_pointrange()
        p <- p + geom_hline(y=0, col="grey")
        p <- p + facet_wrap(~ varb)
        p <- p + scale_x_discrete(varname)
        p


}

