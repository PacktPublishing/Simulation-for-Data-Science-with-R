## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)

## ---- eval=FALSE, echo=FALSE, message=FALSE,warning=FALSE----------------
## library("random")
## x <- randomNumbers(n=10000, col=2, min=0,
##           max=1e+06, check=TRUE) / 1000000
## save(x, file="~/workspace/simulation-book/book/chapter4/r12.RData")

## ---- eval=FALSE, echo=TRUE, message=FALSE,warning=FALSE-----------------
## library("random")
## x <- randomNumbers(n=10000, col=2, min=0,
##           max=1e+06, check=TRUE) / 1000000

## ---- echo=FALSE, message=FALSE,warning=FALSE----------------------------
load("~/workspace/simulation-book/book/chapter4/r12.RData")
r1 <- as.numeric(x)
require(ggplot2)
theme_set(theme_bw())

## ----B05113_04_01, fig.width=4, fig.height=4, eval=TRUE------------------
n <- length(x)
df <- data.frame(x1 = x[1:(n-1)], x2 = x[2:n])
ggplot(df, aes(x = x1, y = x2)) + geom_point(size = 0.1) +
xlab("random numbers from random.org") + ylab("lag 1")

## ------------------------------------------------------------------------
seed <- 123
randu <- function(n) {
  for (i in 1:n) {
    seed <<- (65539 * seed) %% (2^31)
	   result[i] <- seed / 2^31
  }
  return(result)
}
plot3S <- function(func, n=10000) {
  x <- func(n)
  require("rgl")
  x1 <- x[1:(n-2)]
  x2 <- x[2:(n-1)]
  x3 <- x[3:n]
  plot3d(x1, x2, x3, size=3)
  play3d(spin3d())
}

## ---- eval=FALSE---------------------------------------------------------
## plot3S(randu)
## ## to compare it with R's standard generator
## plot3S(runif) ## (Mersenne-Twister)

## ------------------------------------------------------------------------
RNGkind()

## ----B05113_04_04, fig.height=3, fig.width=6-----------------------------
ok <- RNGkind()
op <- par(mfrow=c(1,2), mar=c(3,4,2,2))
set.seed(111)
hist(rnorm(1000), main="Mersenne Twister, Inversion", freq=FALSE, xlim=c(-4,4), ylim=c(0,0.43), cex.main=0.7)
curve(dnorm(x), col = 2, lty = 1, lwd = 2, add = TRUE)
RNGkind("Super", "Box-Muller")
RNGkind()
hist(rnorm(1000), main="Super-Duper, Box-Muller", freq=FALSE, xlim=c(-4,4), ylim=c(0,0.43), cex.main=0.7) 
curve(dnorm(x), col = 2, lty = 1, lwd = 2, add = TRUE)

## ------------------------------------------------------------------------
u <- runif(100, 0, 1)
ifelse(u <= 0.2, 0, 1)

## ----B05113_04_06, echo=FALSE--------------------------------------------
plot(x=c(0,1), y=c(0,1.3), type="n", yaxt="n", xlab="U(0,1)", ylab="B(pi)")
segments(y0=-0.5, x0=0, y1=0.5, x1=0, lty=1, col="black", lwd=2)
segments(y0=0.5, y1=0.5, x0=0, x1=0.2, col="black", lwd=1, lty=2)
segments(y0=0.5, y1=1, x0=0.2, x1=0.2, lwd=2)
segments(y0=1, y1=1, x0=0.2, x1=1, col="black", lwd=1, lty=2)
segments(y0=1, y1=1.5, x0=1, x1=1, lwd=2)
points(0, 0.5, cex=2)
points(0.2, 0.5, cex=2, pch=20)
points(0.2,1, cex=2)
points(1,1, cex=2, pch=20)
arrows(x0=0.55376, x1=0.55376, y0=-0.15, y1=1, col="blue", lty=2)
arrows(x0=0.55376,x1=-0.04, y0=1, y1=1, col="blue", lty=2)
axis(2, at=c(1,0.5), labels=c(1,0), cex.axis=1.5, las=1)
text(x=0.53, y=0.3, labels="random number 0.554 \n drawn from U(0,1)", col="blue", srt=0)

## ----B05113_04_07, echo=FALSE--------------------------------------------
plot(x=c(0,1), y=c(0,3), type="n", xlab="U(0,1)", 
		ylab="exponential distributed")
s <- seq(0,1,length.out=1000)
lines(x=s, y=qexp(s), lwd=2)
arrows(x0=0.55376, x1=0.55376, y1=qexp(0.55376), y0=-015, col="blue", lty=2)
arrows(x0=0.55376, x1=-0.04, y0=qexp(0.55476), y1=qexp(0.55476), col="blue", lty=2)

## ----B05113_04_08--------------------------------------------------------
library("MASS")
invExp <- function(n, lambda = 1) {
  u <- runif(n)
  x <- -(1/lambda) * log(u)
  return(x)
}
lambda <- c(0.5, 1, 2, 5)
par(mar = c(3,3,3,0), mfrow = c(2, 2))
for (l in lambda) {
  sample <- invExp(10000, l)
  truehist(sample, nbins = 20, col = "limegreen", 
		   main = paste("lambda =", l), xlab = "")
  curve(dexp(x, l), from = 1e-10, add = TRUE)
}

## ---- echo=FALSE---------------------------------------------------------
set.seed(1234)

## ------------------------------------------------------------------------
sample(1:3, 10, replace = TRUE, prob = c(3/7,1/7,3/7))

## ----B05113_04_09, echo=FALSE--------------------------------------------
par(mfrow=c(1,2))
tab <- c(3/7, 1/7, 3/7)
#par(xaxt="n", yaxt="n")
barplot(tab, xaxt="n", yaxt="n", ylab="probabilties", xlab="categories")
axis(1, at=c(0.65, 1.85, 3.13), labels=c("x1", "x2", "x3"))
axis(2, at=c(0,1/7,3/7), labels=c(0, "1/7","3/7"))
tab <- c(7/21, 7/21, 7/21)
#par(xaxt="n", yaxt="n")
barplot(tab, xaxt="n", yaxt="n", ylab="probabilties", xlab="categories", ylim=c(0, 0.42857))
#axis(1, at=c(0.65, 1.85, 3.13), labels=c("x1", "x2", "x3"))
axis(2, at=c(0,3/21,5/21,7/21), labels=c(0, "3/21","5/21","7/21"))
segments(x0=1.3897, x1=2.3943, y0=5/21, y1=5/21)
segments(x0=2.5879, x1=3.5888, y0=3/21, y1=3/21)
text(x=c(0.63704, 1.85844, 1.84147, 3.04591, 3.04591), 
     y=c(0.16615, 0.28473, 0.13672, 0.064876, 0.237125),      
     c("x1","x1","x3","x2","x3"))

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
require("knitr")
tab <- data.frame("i"=1:3, "xi"=c("x1","x3","x2"), 
       "pi"=c("3/7","3/7","1/7"), 
       "ai"=c("x1","x1","x3"),
       "ri"=c("1","5/7","3/7"))
kable(tab)

## ---- echo=FALSE---------------------------------------------------------
library(codatables)
data("precipitation")
a <- addmargins(precipitation)
kable(a)

## ------------------------------------------------------------------------
x <- data.frame("spring" = c(275,56,52,65,37,23),
                            "summer" = c(302,20,29,17,15,5),
                            "autumn" = c(375,43,53,52,54,50),
                            "winter" = c(198,37,44,69,58,42))

## ---- eval=TRUE, warning=FALSE, message=FALSE----------------------------
xx <- expand.grid(rownames(x), colnames(x)) # all combinations
x1 <- xx[,1]
x2 <- xx[,2]
y <- as.vector(t(prop.table(x))) # cell probabilites
form <- y ~ x1:x2  # modell
mod <- glm(form, family="poisson") # estimation
pred <- (predict(mod))  # prediction
pred <- exp(pred)/(1+exp(pred))  # transf. with logistic function

## ------------------------------------------------------------------------
round(matrix(pred, ncol=4, byrow=TRUE) * sum(x)) # table

## ------------------------------------------------------------------------
x <- seq(-5, 5, length = 200)
dc <- dcauchy(x, location = 0, scale = 1)
dn <- dnorm(x, mean = 0, sd = 1)

## ----B05113_04_10--------------------------------------------------------
par(mfrow=c(1,2), mar = c(4,4,0.1,1))
plot(x, dn, type="l", ylab = "f(x) and h(x)    . In the R code: dn and dc")
lines(x, dc, col="blue", lty=2)
legend("topright", col=c("black", "blue"), lty=c(1,2), legend = c("normal, f(x)", "Cauchy, h(x)"), cex=1)
plot(x, dn/dc, type="l", ylab = "f(x) / h(x)   . In the R code: (dn / dc)")

## ------------------------------------------------------------------------
foo <- function(x) dnorm(x)/dcauchy(x)
opt <- optimize(foo, c(0, 4), maximum=TRUE)
a <- opt$objective
a
ah <- a * dc

## ----B05113_04_11--------------------------------------------------------
plot(x, dn, type="l", ylim=c(0,0.5), lwd=2, ylab = "densities")
lines(x, dc, col="blue", lty=2)
lines(x, ah, col="blue", lty=2, lwd=2)
legend("topright", col=c("black", "blue", "blue"), lty=c(1,2,2), lwd=c(1,1,2), legend = c("normal, f(x)", "Cauchy, h(x)", "g(x) = a * Cauchy"), cex=1)

## ----B05113_04_12--------------------------------------------------------
plot(x, dn, type="l", ylim=c(0,0.5), lwd=2)
polygon(x, dn, col="gray")
polygon(c(x, rev(x)), c(dn, rev(ah)), col="blue")

## ------------------------------------------------------------------------
alpha <- function(x){
	dnorm(x)/(1.520347 * dcauchy(x))
}

rejectionNorm <- function(n) {
	x <- rcauchy(10000,0,1)
	u <- runif(10000)
	return(na.omit(ifelse(u <= alpha(x), x, NA)))
}

## ----B05113_04_13--------------------------------------------------------
set.seed(123)
x <- rejectionNorm(10000)
hist(x, prob=TRUE)
curve(dnorm(x), lty = 1, lwd = 2, add = TRUE)

## ----B05113_04_14--------------------------------------------------------
curve(dbeta(x, shape1 = 2, shape2 = 2), from = 0, to = 1,
  xlab = "", ylab = "", main = "")
## a * h(x):
abline(h = 1.5, lty = 2)

## ------------------------------------------------------------------------
rsBeta <- function(n) {
  z <- runif(n)
  u <- runif(n)
  ind <- (u <= 4 * z * (1 - z))
  return(z[ind])
}
set.seed(123)
sample1 <- rsBeta(10000)
acceptS <- length(sample1) / 10000
acceptS

## ---- echo=TRUE, tidy=FALSE----------------------------------------------
library(MASS)
plot1 <- function(s, shape1=2, shape2=2){
  truehist(s, h = 0.1, xlim = c(0, 1), #ylim = c(0,2), 
  			col="white", main = "", xlab = "")
  curve(dbeta(x, shape1 = shape1, shape2 = shape2),
        from = 0, to = 1, add = TRUE)
  d <- density(s, from = 0, to = 1, adjust = 2, 
               kernel = "gaussian")
  lines(d$x, d$y, lty = 2)
  legend("topright", 
         legend = c("true density",	"density of simulated values"), 
         col = c(1, 1), lty = c(1, 2), cex = 0.6)	
}

## ----B05113_04_15--------------------------------------------------------
plot1(sample1) # produces a histgram and curve, shown below:

## ---- eval=FALSE, fig.keep='none'----------------------------------------
## rsBeta2 <- function(n, shape1=2.5, shape2=6.5){
## 	a <- optimize(f=function(x){dbeta(x,shape1,shape2)},
## 			interval=c(0,1), maximum=TRUE)$objective
## 	z <- runif(n)
## 	u <- runif(n, max=a)
## 	ind <- (u <= dbeta(z,shape1,shape2))
## 	return(z[ind])
## }
## sample2 <- rsBeta2(10000)

## ------------------------------------------------------------------------
# percentage of rejection
(1- (pcauchy(5) - pcauchy(4))) * 100
v <- rcauchy(1000)
v <- v[v >= 4 & v <= 5]
v
v[1:10]

## ------------------------------------------------------------------------
Fa <- pcauchy(4)
Fb <- pcauchy(5)
u <- runif(10, min = 0, max = Fb - Fa)
qcauchy(Fa + u)

## ------------------------------------------------------------------------
## Simple random walk Markov chain:
n <- 10; set.seed(123)
x <- numeric(n)
for(i in 2:n){
  x[i] <- x[i-1] + rnorm(1)
}
x

## ------------------------------------------------------------------------
set.seed(123)
x <- numeric(n)
for(i in 2:n){
  x[i] <- rnorm(1, mean = x[i-1])
}
x

## ------------------------------------------------------------------------
f <- function(x, sigma){
  if(any(x < 0)) return(0)
  stopifnot(sigma > 0)
  return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
}

## ------------------------------------------------------------------------
i <- 2
xt <- x[i-1] <- rchisq(1, 1)
y <- rchisq(1, df=xt)

## ------------------------------------------------------------------------
rrai <- function(n = 10000, burnin = 1000, thin = 10, sigma = 4, verbose = TRUE){
  ## raileigh density
  f <- function(x, sigma){
    if(any(x < 0)) return(0)
    stopifnot(sigma > 0)
    return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
  }
  x <- numeric(n)
  x[1] <- rchisq(1, df=1)
  k <- 0; u <- runif(n)
  for(i in 2:n){
    xt <- x[i-1]
    y <- rchisq(1, df=xt)
    num <- f(y, sigma) * dchisq(xt, df=y)
    den <- f(xt, sigma) * dchisq(y, df=xt)
    if(u[i] <= num/den){
      x[i] <- y
    } else {
      x[i] <- xt
      k <- k + 1 # y is rejected
    }
  }
  if(verbose) cat("acceptance rate:", (k-burnin)/n/thin, "\n")
  ## burn-in:
  if(burnin > 1) x <- x[(burnin+1):length(x)]
  ## thining:
  return(x[seq(1, length(x), thin)])
}

r <- rrai(n = 10000, thin = 1, burnin = 1)
r <- rrai(n = 10000, thin = 10, burnin = 1000)
length(r)

## ----B05113_04_16--------------------------------------------------------
qplot(1:length(r), r, geom="line", xlab="", ylab="random numbers from Rayleigh(4)")

## ----B05113_04_17--------------------------------------------------------
a <- ppoints(length(r))
sigma <- 4
QR <- sigma * sqrt (-2 * log (1-a)) # quantiles of Rayleigh
Q <- quantile(r, a)
qqplot(QR, Q, main = "", xlab = "Rayleigh quantile", ylab = "sample quantile")

## ----B05113_04_18, echo=FALSE--------------------------------------------
par(mfrow=c(1,2))
SEQ <- seq(-4,4,0.05)
par(mar=c(2.5,3.8,3,1.1))
plot(SEQ, dnorm(SEQ), xaxt="n", type="l", lwd=2)
axis(1,at=c(-2,-1), label=c(expression(x[t]), expression(y[t])))
segments(x0=-2,x1=-2,y1=dnorm(-2),y0=-1)
segments(x0=-2,x1=-5,y1=dnorm(-2),y0=dnorm(-2))
segments(x0=-1,x1=-1,y1=dnorm(-1),y0=-1)
segments(x0=-1,x1=-5,y1=dnorm(-1),y0=dnorm(-1))
axis(2,at=c(dnorm(-2),dnorm(-1)), 
		label=c(expression(f(x[t])), expression(f(y[t]))), las=1)
abline(h=0, lty=2)
SEQ <- seq(-4,4,0.05)
par(mar=c(2.5,3,3,0.1))
plot(SEQ, dnorm(SEQ), xaxt="n", type="l", lwd=2)
axis(1,at=c(-1.85,-1.5), label=c(expression(y[t]), expression(x[t])))
segments(x0=-1.85,x1=-1.85,y1=dnorm(-1.85),y0=-1)
segments(x0=-1.85,x1=-5,y1=dnorm(-1.85),y0=dnorm(-1.85))
segments(x0=-1.5,x1=-1.5,y1=dnorm(-1.5),y0=-1)
segments(x0=-1.5,x1=-5,y1=dnorm(-1.5),y0=dnorm(-1.5))
axis(2,at=c(dnorm(-1.85),dnorm(-1.5)), 
		label=c(expression(f(y[t])), expression(f(x[t]))), las=1)
abline(h=0, lty=2)

## ------------------------------------------------------------------------
mh <- function(n=10000, burnin=1000, thin=10, cand=runif, 
		target=dbeta, shape1=2, shape2=2){
	if(burnin >= n){ 
		stop("burnin is larger than the number of simulations")
	}
	x <- rep(cand(1), n) # initialization
	for(i in 2:n){
		y <- cand(1)
		rho <- target(y,shape1,shape2)/
				      target(x[i-1], shape1, shape2)
		x[i] <- x[i-1] + (y - x[i-1]) * (runif(1) < rho)
	}
  # burn-in
	x <- x[(burnin+1):n]
  return(x[seq(1, length(x), thin)])
}

## ----B05113_04_19--------------------------------------------------------
par(mfrow=c(1,2))
plot(density(mh()), main = "", xlab = "")
plot(density(mh(shape1=1)), main = "", xlab = "")

## ------------------------------------------------------------------------
rgamma <- mh(cand = rnorm, target = dgamma)

## ----B05113_04_20--------------------------------------------------------
gibbs_bivariate <- function(n = 1000, rho = 0.9, start = 0, burnin = 100, thin = 1){
  x <- y <- numeric(n)
	s <- 1 - rho^2
	x[1] <- start # to show effect of burnin
	for(t in 1:(n-1)){
	   y[t+1] <- rnorm(1, rho*x[t], s)
	   x[t+1] <- rnorm(1, rho*y[t+1], s)
	}
	s <- seq(burnin+1, n, thin)
	return(cbind(x[s], y[s]))
}	
par(mfrow=c(1,3))
set.seed(123)
## bad start:
b0 <- gibbs_bivariate(n=200, start = 30, burnin=0)
## plot the results
plot(b0, type="o", xlab="x", ylab="y", main="burnin 0",
		cex.main=1.3, cex.lab=1.3)
set.seed(123)
plot(b0[20:200,], type="o", xlab="x", ylab="y", main="burnin 20",
		cex.main=1.3, cex.lab=1.3, col=grey(20:200/200))
set.seed(123)
plot(b0[20:200,], pch=20, xlab="x", ylab="y", main="burnin 20",
		cex.main=1.3, cex.lab=1.3)

## ------------------------------------------------------------------------
lreg <- function(y, x, time, alpha = 0, beta = -2, tau = 1, burnin = 0, thin = 1){
	n <- length(y)
	## alpha, beta, tau defining varibles
	res <- matrix(, ncol=3, nrow=time)
	for(i in 1:time){
		alpha <- rnorm(1, mean(y)  -beta * mean(x), 1 / (n  *tau))
		m <- (sum(x * y) - alpha * n * mean(x)) / sum(x**2)
		s <- 1 / (tau * sum(x**2))
		beta <- rnorm(1, m, s)
		w <- y - alpha - beta * x
		tau <- rgamma(1, ((n / 2) + 1), (sum(w**2) / 2))
		res[i,] <- c(alpha, beta, tau)
	}
	s <- seq(1, length((burnin + 1):nrow(res)), thin)
	res <- res[((burnin+1):nrow(res))[s], ]
	res <- data.frame(res)
	colnames(res) <- c("alpha", "beta", "tau")
	return(res)
}	

## ----B05113_04_21--------------------------------------------------------
data(Cars93, package = "MASS")
set.seed(123)
time <- 100
res <- lreg(Cars93$Price, Cars93$Horsepower, time = time)
par(mar = c(4,4,0.1,0.1))
plot(Cars93$Horsepower, Cars93$Price, pch=20, xlab = "Horsepower", ylab = "Price", type = "n")
range <- 1 - sqrt(1:time/time)
range <- range + 0.1
#range <- range/sum(2*range) 
for(i in 1:time){
	abline(a = res[i, 1], b = res[i, 2], col=gray(range[i]))#sqrt(1-i/size)))
}
abline(a = res[i, 1], b = res[i, 2], col="red", lty=2,lwd=3)#sqrt(1-i/size)))
points(Cars93$Horsepower, Cars93$Price, pch=20)

## ----B05113_04_22, message=FALSE, warning=FALSE, fig.width=7, fig.height=4----
set.seed(123)
g <- lreg(Cars93$Price, Cars93$Horsepower, time = 500)
g1 <- cbind(g, "index" = 1:nrow(g))
g1 <- reshape2::melt(g1, id=c("index"))
ggplot(g1, aes(x = index, y = value)) + geom_line() + facet_wrap(~variable, scales = "free_y") 

## ----B05113_04_23, message=FALSE, warning=FALSE--------------------------
plot(acf(g), mar=c(2.9,3.2,2,0), cex.lab = 1.4, cex.main = 1.3)

## ---- fig.height=4, fig.width=7, cache=FALSE-----------------------------
library("coda")
time <- 2000; M <- 5
set.seed(12345)
df <- lreg(Cars93$Price, Cars93$Horsepower, time = time)
for(i in 2:M){
  df <- rbind(df, lreg(Cars93$Price, Cars93$Horsepower, time = time))
}
df$M <- factor(rep(1:M, each = time))
df$index <- rep(1:time, M)
df <- reshape2::melt(df, id = c("M", "index"))

## ----B05113_04_24, fig.height=4, fig.width=7, cache=FALSE----------------
ggplot(df, aes(x = index, y = value, group = M, colour=M)) + geom_line(alpha = 0.5) + facet_wrap(~variable, scales = "free_y")

## ----gelmandiag, eval = FALSE--------------------------------------------
## ## Brooke-Gelman
## gl <- list()
## M <- 15
## set.seed(12345)
## for(i in 1:M){
##   gl[[i]] <- lreg(Cars93$Price, Cars93$Horsepower, time = time)
## }
## gl <- lapply(gl, function(x) mcmc(as.matrix(x)))
## ## look also at summary(g) (not shown here)
## gelman.diag(gl, autoburnin = FALSE)
## 
## ## Potential scale reduction factors:
## ##
## ##      Point est. Upper C.I.
## ## alpha       1.07       1.07
## ## beta        1.07       1.07
## ## tau         1.00       1.00
## ##
## ## Multivariate psrf

## ----B05113_04_25, fig.height=4, fig.width=6, eval=FALSE-----------------
## gelman.plot(gl, autoburnin = FALSE)

## ---- eval=FALSE---------------------------------------------------------
## burnin <- 1000
## time <- burnin + time * 20
## g <- lreg(Cars93$Price, Cars93$Horsepower, time = time, burnin = burnin, thin = 20)

## ------------------------------------------------------------------------
circle <- function(x, r=0.05){
    repeat{
        x1 <- runif(1,-1,1)
        x2 <- runif(1,-1,1)
        if( sqrt(x1^2 + x2^2) <= (1 - r) ) break
    }
    inCircle <- ((x[,1] - x1)^2 + (x[,2] - x2)^2) <= r^2
    return(inCircle)
}

## ------------------------------------------------------------------------
set.seed(123)
## take possible radii
x <- matrix(runif(10000, -1, 1), ncol=2)
## radii to the square
r2 <- rowSums(x^2)
## r2 smaller than 1 are kept
x1 <- x[r2 <= 1, ]

## ----B05113_04_26--------------------------------------------------------
par(mar = c(2,2,0.1,0.1))
plot(data.frame(x1), pch=20)
for(k in 1:8) points(data.frame(x1[circle(x1, 0.2),]), col=k, pch=20)

## ------------------------------------------------------------------------
set.seed(123)
z <- replicate(2000, sum(circle(x1)))

## ------------------------------------------------------------------------
TAB <- table(z)
TAB

## ------------------------------------------------------------------------
laeken::weightedMedian(as.numeric(names(TAB)), as.numeric(TAB))

## ----B05113_04_27--------------------------------------------------------
lambda <- nrow(x1) * 0.05^2
PROB <- dpois(as.numeric(names(TAB)), lambda)
b <- barplot(TAB / length(z))
points(b, PROB, col="red", pch=16) 

## ------------------------------------------------------------------------
## the five classes:
QP <- qpois(seq(0,1,by=1/6), lambda)
QP
## frequency counts in those classes
TAB1 <- table(cut(z, QP, include.lowest=TRUE))
TAB1

## ------------------------------------------------------------------------
ppois(QP, lambda)
## 0 should be in the left class:
QP1 <- QP
QP1[1] <- -1
## probablities for each class:
PROB1 <- diff(ppois(QP1, lambda))
PROB1
## goodness-of-fit test:
chisq.test(TAB1, p=PROB1)

## ------------------------------------------------------------------------
sessionInfo()

