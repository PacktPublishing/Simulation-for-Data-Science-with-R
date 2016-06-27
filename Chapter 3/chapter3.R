## ------------------------------------------------------------------------
trunc (2.3 * 100)

## ------------------------------------------------------------------------
round(1.49999999999999)
round(1.4999999999999999)

## ----match---------------------------------------------------------------
a <- seq(0.6, 1, by = 0.01)
match(0.88, a)
match(0.89, a)
match(0.94, a)

## ----excel---------------------------------------------------------------
excel_round <- function (x, digits) round (x * (1 + 1e-15), digits)
round(0.5, digits = 0)
excel_round(0.5, digits = 0)

## ----machine_eps---------------------------------------------------------
.Machine$double.eps
.Machine$double.digits
.Machine$double.xmax

## ----B05113_03_01, echo=FALSE, fig.align='center'------------------------
a <- c(0.65, 0.62, 0.41, 0.43, 0.61, 0.49, 0.44, 0.58, 0.47, 0.53, 0.52, 0.48, 0.51)
par(mar=c(4,4,0.2,0.2))
plot(1:length(a), a, ylim = c(0,1), xlab="n", yaxt="n", ylab="")
axis(2, at=0.5, labels = "a", las=1)
abline(h=0.5 + 0.03)
abline(h=0.5 - 0.03)
axis(2, at=0.5+0.03, labels = expression(a + epsilon), las=1)
axis(2, at=0.5-0.03, labels = expression(a - epsilon), las=1)
polygon(x=c(0,14,14,0,0), c(0.5-0.03,0.5-0.03,0.5+0.03,0.5+0.03,0.5-0.03), col="lightgrey")
points(1:length(a), a)

## ----convergence1, cache=TRUE--------------------------------------------
masch <- function(maxit=10000){
  summe <- 0
  summeNeu <- n <- 1
  ptm <- proc.time()
  while(summe != summeNeu & n < maxit){
    summe <- summeNeu
    summeNeu <- summe + 1/n
    n <- n + 1
  }
  time <- proc.time() - ptm
  list(summe=summeNeu, time=time[3])
}
masch(10000)$summe
masch(1000000)$summe
masch(10000000)$summe

## ---- echo=FALSE---------------------------------------------------------
library(knitr)

## ----result, cache=TRUE--------------------------------------------------
SEQ <- c(10,1000, seq(100000,10000000,length=10))
df <- cbind(SEQ, t(as.data.frame(sapply(SEQ, masch))))
kable(df)

## ----convergence2, cache=TRUE--------------------------------------------
x <- 1
oldsum = 0
newsum = n = term = 1
while(oldsum != newsum){
  oldsum = newsum
  term = 1/factorial(n) 
  n = n + 1
  newsum = oldsum + term
  print(paste("n = ", n, ". Diff = ", term, ". Sum = ", newsum, sep=""))
}

## ----sdev----------------------------------------------------------------
## first formula for the standard deviation:
s1 <- function(x){
  s <- sum((x - mean(x))^2)
	return(sqrt(1/(length(x)-1) * s))
}
## second formula for the standard deviation:
s2 <- function(x){
#	s <- 1/(length(x)-1) * sum(x^2) - mean(x)^2
	s <- sum(x^2) - 1/length(x) * sum(x)^2
	return(sqrt(1/(length(x)-1) * s))
}
## wrapper function:
st <- function(x, type) {
  switch(type,
         precise = s1(x),
         oldexcel = s2(x)
         )
}
## generating 1000 random numbers from standard normal distribution:
x <- rnorm(1000)
## show more digits:
options(digits=16)
## results:
st(x, "precise")
st(x, "oldexcel")

## ----sdev_results, warning=FALSE, message=FALSE, tidy=FALSE--------------
stall <- function(x){
	c(precise=st(x, "precise"), excel=st(x, "oldexcel"))
}
## generate numbers (zeros and ones)
x <- rep(0:1,100)
stall(x)
X <- matrix(nrow=length(x), ncol=10)
X[,1] <- 1
for(n in 2:ncol(X)) X[, n] <- x + 10^(2 * (n - 1))
colnames(X) <- 2 * (1:ncol(X) - 1)
dim(X)
## first four observations:
head(X,4)
options(digits=5)
apply(X, 2, stall)

## ----convergence3--------------------------------------------------------
konv <- function(q = 2){ 	
  s <-  0
	snew <- term <- i <- 1
	while(s != snew){	
	  s <- snew
		snew <-  s + q^(-i)
		i <- i + 1
	}
	list(iteration = i, total = snew, lastterm = 2^(-i))	
}
konv()

## ------------------------------------------------------------------------
konv(q = 0.5)

## ----expsum--------------------------------------------------------------
expsum <- function(x)
{	oldsum <- 0
	newsum <- n <- term <- 1
	while( oldsum != newsum )
	{	oldsum <- newsum
		term <- term * x/n
		n <- n + 1
		newsum <- oldsum + term 
	}
	list(iteration = n, summe = newsum)
}

## ----error, cache=TRUE---------------------------------------------------
x <- 1:1000
absError <- sapply (x, function (x) abs (exp (x) - expsum (x) $ sum))
relError <- absError / exp (x)

## ----error2--------------------------------------------------------------
roundingError <- sapply(x, function(x) 2^(-53)*exp(x))

## ----B05113_03_02--------------------------------------------------------
plot(x[1: 600], roundingError[1:600], log = "xy", xlab = "log (x)",
ylab = "log (rounding errors)", type = "l")

## ----B05113_03_03--------------------------------------------------------
x <- seq(0, 20, by=0.1)
iter <- sapply(x, function(x) expsum(x)$iteration)
plot(x, iter, xlab="x", ylab="No. of iterationen until convergence", type="l") 

## ----condition-----------------------------------------------------------
library("Matrix")
## reciprocal approximate condition number
rcond(Hilbert(9)) ## worse
## reciprocal condition number
x1 <- cbind(1, 1:10)
head(x1, 3)
rcond(x1) ## much better

## ------------------------------------------------------------------------
sessionInfo()

