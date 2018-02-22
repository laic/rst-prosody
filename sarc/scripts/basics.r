# basics.r: Catherine Lai
# little helper functions


### Some transforms
sigmoid <- function(x) {
	return(1/(1 + exp(-x)))
}

refactor <- function(vrmi) {
        for (fx in names(vrmi)) {
                if (is.factor(vrmi[[fx]])) {
                        vrmi[[fx]] <- factor(unlevel(vrmi[[fx]]))
                }
        }
        return(vrmi)
}


to.zscore <- function(x, x.mean=mean(x,na.rm=T), x.sd=sd(x,na.rm=T)) {
        (x - x.mean)/x.sd
}


from.semitone <- function(F0.st, F0.ref=min(F0.hz)) {
        F0.ref * (2^(F0.st/12))
}


## Convert from Hz to semitones (use for F0) 
to.semitone <- function(x, F0.ref=mean(x, na.rm=T)) {
        if (length(names(x))==0) {
                F0.hz <- x
                y <- 12* log2(F0.hz/F0.ref)
        } else {
                F0.hz <- x$F0
                y <- data.frame(Time=x$Time, F0=12* log2(F0.hz/F0.ref))
        }
        y
}



##################################################################################
## Convert factors plain numbers and strings

unfactor <- function(x) {
        as.numeric(levels(x)[x])
}

unlevel <- function(x) {
        levels(x)[x]
}



##################################################################################
## Get various values from a data.frame (or data.table)

prange.value <- function(x0, xname="Time", yname="Value"){ 
	x <- x0[!is.na(x0[[yname]]),]	
	if(nrow(x) > 0) { max(x[[yname]],na.rm=T)-min(x[[yname]],na.rm=T) }
	else {NA}
}

mean.value <- function(x0, xname="Time",yname="Value") {
	x <- x0[!is.na(x0[[yname]]),]	
	if(nrow(x) > 0) { mean(x[[yname]], na.rm=T) }
	else {NA}
}

sd.value <- function(x0, xname="Time", yname="Value") {
	x <- x0[!is.na(x0[[yname]]),]	
	if(nrow(x) > 0) { sd(x[[yname]], na.rm=T) }
	else {NA}
}

median.value <- function(x0, xname="Time",yname="Value") {
	x <- x0[!is.na(x0[[yname]]),]	
	if(nrow(x) > 0) { median(x[[yname]], na.rm=T) }
	else {NA}
}
min.value <- function(x0, xname="Time",yname="Value") {
	x <- x0[!is.na(x0[[yname]]),]	
	if(nrow(x) > 0) { min(x[[yname]], na.rm=T) }
	else {NA}
}

max.value <- function(x0, xname="Time", yname="Value") {
	x <- x0[!is.na(x0[[yname]]),]	
	if(nrow(x) > 0) { max(x[[yname]], na.rm=T) }
	else {NA}
}

slope.value <- function(x0, xname="ActualTime", yname="Value", sampletime=F) {
	#print(c(xname, yname)) 
	x <- x0[!is.na(x0[[yname]]),]	
	zy <- x[[yname]]
	zx <- x[[xname]]

	if(nrow(x) > 1) {
		if (sampletime) {
			y <- lm(Value ~ SampleTime, data=x)$coefficients[2]
		}  else {
			y <- lm(zy ~ zx)$coefficients[2]
		}
		names(y) <- NULL
		return(y) 
	} else {return(NA)}
}  

intercept.value <- function(x0, xname="ActualTime", yname="Value", sampletime=F) {
	x <- x0[!is.na(x0[[yname]]),]	
	zy <- x[[yname]]
	zx <- x[[xname]]

	if(nrow(x) > 1) {
		if (sampletime) {
			y <- lm(Value ~ SampleTime, data=x)$coefficients[1]
		} else {
			y <- lm(zy ~ zx)$coefficients[1]
		}
		names(y) <- NULL
		return(y)
	} else {return(NA)}
}  

jitter.value <- function(x0, xname="Time", yname="Value") {
	x <- x0[!is.na(x0[[yname]]),]	
	if(nrow(x) > 0) {
		y <- x[[yname]]
        	(sum(abs(diff(y)),na.rm=T)/(length(y)-1))/(sum(abs(y),na.rm=T)/length(y))
	} else {NA}
}

npoints.value <- function(x, xname="Time", yname="Value") {
	length(x[[yname]][!is.na(x[[yname]])])
}

get.legendre.coeffs <- function(x, degree=5) {
	lp <- legendre.polynomials(degree, normalize=T)
	p <- legendre.coeff(x$ActualTime,x$Value,lp)
	p
} 


#############################################################################################
pred.point2 <- function(y1, y2, x) {
	a <- y1[2] - ((y2[2]-y1[2])/(y2[1]-y1[1])) * y1[1] 
	m = ((y2[2]-y1[2])/(y2[1]-y1[1]))
	m*x + a
}


unlist.df <- function(x) {
	y <- NULL
	if (length(x) > 0) {
		for (i in 1:length(x)) {
			y <- rbind(y, x[[i]])
		}
	}
	y
}

unlist.vec <- function(x) {
	y <- NULL
	for (i in 1:length(x)) {
		y <- c(y, x[[i]])
	}
	y
}



