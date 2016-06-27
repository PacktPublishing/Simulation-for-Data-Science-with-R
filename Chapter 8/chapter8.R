## ------------------------------------------------------------------------
library("robustbase")
data("hbk")
## structure of the data
str(hbk)

## ------------------------------------------------------------------------
lm_ols <- lm(Y ~ ., data = hbk)
## print summary
summary(lm_ols)

## ----B05113_08_01--------------------------------------------------------
plot(lm_ols, which = 3)

## ------------------------------------------------------------------------
lm_rob <- lmrob(Y ~ ., data = hbk)
## print summary
summary(lm_rob)

## ----B05113_08_02--------------------------------------------------------
plot(lm_rob, which = 5)

## ----B05113_08_03--------------------------------------------------------
pairs(hbk, pch = ".")

## ---- warning=FALSE, message=FALSE---------------------------------------
data(Prestige, package = "car")
rob <- ltsReg(prestige ~ log(income) + women + type, data = Prestige)
summary(rob)

## ------------------------------------------------------------------------
boot.lts <- function(x, indices){
  x <- x [indices,]
  model <- ltsReg(prestige ~ log(income) +     
                 women + type, data = x)
  coefficients(model)
}

## ---- cache=TRUE, message=FALSE,warning=FALSE----------------------------
library("boot")
set.seed(123)
rob_boot <- boot(Prestige, boot.lts, 1000)
## estimated standard errors
rob_boot

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(boot)
library(MASS)
library(e1071)

## ----B05113_08_04--------------------------------------------------------
hist(rob_boot$t[,2], 50, xlab = "bootstrap repl., log(income)", main = "")

## ----B05113_08_05--------------------------------------------------------
hist(rob_boot$t[,3], 50, xlab = "bootstrap repl., women", main = "")

## ------------------------------------------------------------------------
boot.ci(rob_boot, index = 2, 
        type = c("norm", "perc", "bca"))

## ----B05113_08_06, fig.width=6, fig.height=7-----------------------------
par(mfrow = c(2,1), mar = c(4,4,2,0.5))
jack.after.boot(rob_boot, index = 2, main = 'log (income) coefficient')
jack.after.boot(rob_boot, index = 3, main = 'woman coefficient')

## ------------------------------------------------------------------------
set.seed(12)
df <- data.frame(x = 1:7, y = 1:7 + rnorm(7))

## ----B05113_08_07, fig.width=6, fig.height=7-----------------------------
par(mfrow = c(2,1), mar = c(4,4,1,0.3))
## fit to original data
lm_orig <- lm(y ~ x, data = df)
## plot original data
plot(y ~ x, data = df)
## add regression line from original data
abline(lm_orig)
## show the connection lines 
## between original and fitted y
segments(x0 = df$x, x1=df$x,  
         y0=df$y, y1=lm_orig$fit)
## fitted y
points(df$x, lm_orig$fit, pch=20, col="red")
legend("topleft", legend = c("y", expression(hat(y))),
       pch = c(1,20), col = c(1,2))
## second plot ---------------------
## plot of fitted values
plot(lm_orig$fit ~ df$x, col="red", pch = 20, 
     ylab="y", xlab = "x")
## bootstrap sample by adding sampled residuals
y1 <- lm_orig$fit + sample(lm_orig$res, replace = TRUE)
## new bootstrap sample
points(df$x, y1, col="blue", pch = 3)
## connection lines new bootrap sample to 
## fitted values from original data
segments(x0 = df$x, x1 = df$x,
         y0 = lm_orig$fit, y1 = y1, col ="blue")
## regression line from original data
abline(lm_orig)
## regression line from bootstrap sample
abline(lm(y1 ~ df$x), col = "blue", lty = 2)
legend("topleft", legend = c("original", "bootstrap repl. 1"), lty = c(1,2), col = c(1,4))

## ---- cache=TRUE, message=FALSE, warning=FALSE---------------------------
Prestige <- na.omit(Prestige)
## fit model on original data
rob2 <- ltsReg(prestige ~ log(income) + women + type,
               data = Prestige)

## ------------------------------------------------------------------------
residuals <- residuals(rob2)
fit <- fitted(rob2)
## fix X, model matrix
X <- model.matrix(rob2, Prestige)[, -1]

## ------------------------------------------------------------------------
ltsBoot2 <- function(x, indices){
  y <- fit + residuals[indices]
  model <- ltsReg(y ~ X)
  coefficients(model)
}

## ---- message=FALSE, warning=FALSE---------------------------------------
rob2_boot <- boot(Prestige, ltsBoot2, R = 2000)
## show results
rob2_boot

## ----B05113_08_08--------------------------------------------------------
par(mfrow = c(1,2), mar = c(4,4,1,0.2))
hist(rob2_boot$t[,2], 50, xlab = "bootstrap repl., log(income)", main = "")
hist(rob2_boot$t[,3], 50, xlab = "bootstrap repl., women", main = "")

## ----B05113_08_09, fig.width=6, fig.height=7-----------------------------
par(mfrow = c(2,1), mar = c(4,4,2,0.5))
jack.after.boot(rob2_boot, index = 2, main = 'log (income) coefficient')
jack.after.boot(rob2_boot, index = 3, main = 'woman coefficient')

## ---- echo=FALSE---------------------------------------------------------
gendat <- function(n, d)
{
 set.seed(1083)
 s <- matrix(rnorm(d^2,0,0.2), ncol=d, nrow=d)
 diag(s) <- 1
 xorig <- mvrnorm(n ,mu = rep(0, d),Sigma = s)
 xorig <- rbind(xorig, mvrnorm(round(n*0.05) ,mu = c(10, rep(0, d-1)), Sigma = s))
 ## fix me: generell auch fuer mehrere Dimensionen
 x <- xorig
 
 for (i in 1:d)
 {
       miss <- sample(1:dim(x)[1], sample(n/5), replace = TRUE)
       x[miss,i]<-NA
 }
 w <- 0
 w <- which(rowSums(is.na(x)) == d)
 x <- x[-w,]

}
x <- gendat(1000,3)

## ---- message=FALSE, warning=FALSE---------------------------------------
library("VIM")
data("sleep")

## ----B05113_08_10--------------------------------------------------------
aggr(sleep, numbers = TRUE, prop = FALSE, cex.axis = 0.75)

## ----B05113_08_11, message=FALSE, warning=FALSE--------------------------
par(mar = c(6,4,0.2,0.2))
matrixplot(sleep, sortby = "BrainWgt", interactive = FALSE)

## ----B05113_08_12--------------------------------------------------------
set.seed(123)
sleep_boot1 <- sleep[sample(1:nrow(sleep), replace = TRUE), ]
aggr(sleep_boot1, numbers = TRUE, prop = FALSE,
     cex.axis = 0.75)

## ----B05113_08_13, message=FALSE, warning=FALSE--------------------------
par(mar = c(6,4,0.2,0.2))
matrixplot(sleep_boot1, sortby = "BrainWgt")

## ------------------------------------------------------------------------
bootmiss <- function(x, R = 1000, 
                     imputation = "median", 
                     theta = "mean"){
  ## initializations
  d <- dim(x)[2]
  n <- dim(x)[1]
  thetaM <- matrix(NA, ncol=2, nrow=d)
  xs <- theta_hat1 <- matrix(0, nrow=n, ncol=d)
  med <- matrix(NA, ncol=d, nrow=R)
  for(i in 1:R){
    ## bootstrap sample
    s1 <- x[sample(1:n, replace=TRUE), ]
    ## imputation
    if(method %in% c("median", "mean")){
      simp <- impute(s1, what=method)
    }
    if(method == "kNN"){
      simp <- knn(x)$data
    }
    ## estimation
    med[i, ] <- apply(simp, 2, theta)
  }
  ## bootstrap mean (for each column)
  thetaHat <- apply(med, 2, mean)
  ## confidence intervals
  for(i in 1:d){
   thetaM[i,1] <- quantile(med[,i], 0.025)
   thetaM[i,2] <- quantile(med[,i], 0.975)
  }
  ## results
  res <- list(thetaHat = thetaHat,
             ci = thetaM)
  return(res)
}

## ---- cache=TRUE, message=FALSE, warning=FALSE, eval=FALSE---------------
## ci <- quantile(replicate(10000, mean(sleep[sample(1:n, replace = TRUE), "sleep"])), c(0.025, 0.975))
## ci

## ---- eval=FALSE---------------------------------------------------------
## for(i in 1:ncol(x)){
##   indmiss <- sample(1:nrow(x), round(sample(nrow(x)/10)), replace = TRUE)
##   x[indmiss, i] <- NA
## }

## ---- cache=TRUE, message=FALSE, warning=FALSE---------------------------
n <- nrow(sleep)
imputed <- kNN(sleep, imp_var = FALSE)
ci_single <- quantile(replicate(10000, mean(imputed[sample(1:n, replace = TRUE), "Sleep"])), c(0.025, 0.975))
ci_single

## ---- cache=TRUE, message=FALSE, warning=FALSE, results='hide'-----------
ci_boot <- quantile(replicate(10000, mean(kNN(sleep[sample(1:n, replace = TRUE), ], imp_var = FALSE)$Sleep)), c(0.025, 0.975))

## ------------------------------------------------------------------------
ci_boot

## ------------------------------------------------------------------------
set.seed(123)
tseries <- rnorm(50)   
## introduce auto-correlation
tseries[-1] <- tseries[-1] + tseries[-50]    

## ----B05113_08_014-------------------------------------------------------
plot(ts(tseries), ylab = "values")

## ------------------------------------------------------------------------
mbb <- function(x, R=1000, blocksize=6){
  ## initialization
  nc <- length(x)
  lagCorMBB <- ct <- numeric(R)  
  seriesBl <- numeric(nc) 
  ## function for moving blocks bootstrap
  corT <- function(x=tseries, N=nc, bl=blocksize){ 
    ## for N/bl blocks
  	for(i in 1:ceiling(N/bl)) {    
  	  ## endpoint of block
  		endpoint <- sample(bl:N, size=1) 
  		## put blocks together, bootstrap sample
  		seriesBl[(i-1)*bl+1:bl] <- x[endpoint-(bl:1)+1] 
  	}
  	seriesBl <- seriesBl[1:N]
  	## autocorrelation
  	a <- cor(seriesBl[-1],seriesBl[-N])  
  	return(a)
  }
  ct <- replicate(R, corT(x))
  return(ct)
}

## ------------------------------------------------------------------------
mb <- mbb(x=tseries, R=10000)
## first 10 boostrap replicates
mb[1:10]
## auto-correlation cofficient from mean of bootstrap replicates
mean(mb)

## ---- fig.keep='none'----------------------------------------------------
acf(tseries)$acf[2]

## ------------------------------------------------------------------------
qu_mbb <- quantile(mb, c(0.025,0.975))
cat("CI(mbb) : [", round(qu_mbb[1], 2), ",", round(qu_mbb[2], 2), "]")

## ---- fig.keep='none', cache=TRUE, warning=FALSE, message=FALSE----------
library("forecast") 
ac <- taperedacf(tseries)
cat("CI(classical) : [", round(ac$lower[1], 2), ",", round(ac$upper[1], 2), "]")

## ---- tidy=FALSE---------------------------------------------------------
x <- data.frame("location" = rep("Asten", 8), 
      "income" = c(2000,2500,2000,2500,1000,1500,2000,2500), 
      "weight" = c(1000,500,1000,500,1500,1000,1000,2000))
x

## ------------------------------------------------------------------------
sum(x$weight)

## ------------------------------------------------------------------------
sum(x$income * x$weight) 

## ------------------------------------------------------------------------
set.seed(123)
y <- x[sample(1:8, replace = TRUE), ] # Bootstrap Sample
y

## ------------------------------------------------------------------------
# non-calibrated estimation
sum(y$income * y$weight) 

## ------------------------------------------------------------------------
sum(y$weight)

## ------------------------------------------------------------------------
constant <- sum(x$weight) / sum(y$weight) 
## calibrated estimation
sum(y$x * y$w * constant)

## ------------------------------------------------------------------------
library("laeken")
data("eusilc")
## point estimate of poverty rate
a <- arpr("eqIncome", weights = "rb050", data = eusilc)
## bootstrap with calibration
## define auxiliary 0-1 variables for regions
aux <- sapply(levels(eusilc$db040), 
    function(l, x) as.numeric(x == l), 
    x = eusilc$db040)
## retrieve population totals from underlying sample
totals <- sapply(levels(eusilc$db040), 
    function(l, x, w) sum(w[x == l]), 
    x = eusilc$db040, w = eusilc$rb050)
# bootstrap variance
variance("eqIncome", weights = "rb050", design = "db040", 
    data = eusilc, indicator = a, X = aux, totals = totals, 
    seed = 123)	

## ------------------------------------------------------------------------
temp <- read.table("http://venus.unive.it/romanaz/statistics/data/bodytemp.txt", header = TRUE)
temp$gen <- factor(temp$gen, labels = c("male", "female"))
str(temp)

## ------------------------------------------------------------------------
temp$celsius <- (temp$tf - 32) * 5 / 9

## ----B05113_08_015-------------------------------------------------------
library("ggplot2")
ggplot(temp, aes(x = celsius, colour = gen, linetype = gen)) + geom_density(size = 1.2) + theme(text = element_text(size=16)) + theme_bw()

## ------------------------------------------------------------------------
temperature <- temp$celsius

## ------------------------------------------------------------------------
n <- length(temperature)
temperature <- sort(temperature)
y <- (0:(n-1)) / n

## ----B05113_08_016-------------------------------------------------------
plot(temperature, y, pch=20, cex = 0.3)
lines(temperature, y, type="S")

## ----B05113_08_017-------------------------------------------------------
plot(temperature, y, type="S")
m <- mean(temperature)
s <- sd(temperature)
yn <- pnorm(temperature, mean = m, sd = s)
lines(temperature, yn, col=2)

## ------------------------------------------------------------------------
z <- round(sort(rnorm(n, mean = m, sd = s)), 1)

## ----B05113_08_018-------------------------------------------------------
set.seed(123)
plot(temperature, y, type="S")
for(k in 1:100){
    z <- rnorm(n, mean=m, sd=s)
    lines(sort(z), y, type="S", col="green")
}

## ------------------------------------------------------------------------
Z <- NULL
for(k in 1:1000){
    z = rnorm(n, mean = m, sd = s)
    Z = cbind(Z, sort(z))
}
dim(Z)

## ------------------------------------------------------------------------
## mean of original temperature data
m
## simulated mean
(mean(Z[65, ]) + mean(Z[66, ])) / 2
## simulated median
(median(Z[65, ]) + median(Z[66, ])) / 2

## ------------------------------------------------------------------------
plot(temperature, y, type="S")
middle <- apply(Z, 1, median)
lines(middle, y, col = "blue", lwd = 2, type = "S")
## lower and upper bounds
lower <- apply(Z, 1, quantile, prob = 0.025)
upper <- apply(Z, 1, quantile, prob = 0.975)
lines(lower, y, col = 2, lwd = 2, type = "S")
lines(upper, y, col = 2, lwd = 2, type = "S")

## ----B05113_08_020-------------------------------------------------------
par(mfrow = c(2,1), mar = rep(1,4))
plot(temperature, y, type="S")
lines(temperature, yn, col=2)
lines(lower, y, col=2, lwd=2, type="S")
lines(upper, y, col=2, lwd=2, type="S")
plot(temperature, y - yn, type="h")
abline(h = 0)
## maximum deviation
D <- max(abs(y - yn))
w <- which.max(abs(y - yn))
points(temperature[w], y[w] - yn[w], col=2, pch=16, cex=3)

## ------------------------------------------------------------------------
## theoretical distribution
Z1 <- pnorm(Z, mean = m, sd = s)
## y will be recycled column-wise,
## extract the maximum for each column
D1 <- apply(abs(y - Z1), 2, max)

## ------------------------------------------------------------------------
summary(D1)
D

## ------------------------------------------------------------------------
mean(D1>D)

## ---- message=FALSE, warning=FALSE---------------------------------------
ks.test(temperature, "pnorm", mean = m, sd = s)

## ------------------------------------------------------------------------
data(Duncan, package = "car")
x <- subset(Duncan, type %in% c("bc", "wc"), select = c("income", "type"))
x$type <- factor(x$type)
## first four observations on income and type
head(x, 4)

## ------------------------------------------------------------------------
t.test(income ~ type, data=x)

## ------------------------------------------------------------------------
## first 6 observations with permuted grouping structure
head(cbind(x, "p1" = sample(x$type), 
              "p2" = sample(x$type), 
              "p3" = sample(x$type)))

## ------------------------------------------------------------------------
## define test statistics (workhorse)
teststat <- function(vals, group, lev){
  g <- sample(group)
  abs(mean(vals[g == lev[1]]) - mean(vals[g == lev[2]]))
}
## permutation test
permtest <- function(x, g, R = 1000, conf.level = 0.95){
    ## levels of the group vector
    lg <- levels(g)
    ## test statistics for original groups
    mdiff <- abs(mean(x[g==lg[1]]) - mean(x[g==lg[2]]))
    ## test statistics for permuted group data
    z <- replicate(R, teststat(x, g, lg))
    ## make nice print output
    DATA <- paste(deparse(substitute(x)),
                  "by",
                  deparse(substitute(g)))
    alpha <- 1 - conf.level
    conf.int <- quantile(z, prob = c(alpha/2, (1 - alpha)/2))
    attr(conf.int, "conf.level") <- conf.level
    res <- list(statistic=c(mdiff = mdiff),
                   p.value = mean(abs(z) > abs(mdiff)),
                   parameter = c(nrep = R),
                   conf.int = conf.int,
                   data.name = DATA,
                   method = "Permutation test for difference in means")
    class(res) <- "htest"
    res
}

## ------------------------------------------------------------------------
permtest(x$income, x$type, R = 10000)

## ------------------------------------------------------------------------
data(Duncan, package = "car")
pairwise.t.test(Duncan$income, Duncan$type)

## ---- message=FALSE, warning=FALSE---------------------------------------
mean(Duncan$income)
library("dplyr")
Duncan %>% group_by(type) %>% summarize(mean = mean(income))

## ------------------------------------------------------------------------
tstat <- function(x, mu=0){
    (mean(x)-mu) / (sd(x) / sqrt(length(x)))
}
stats <- tapply(Duncan$income, Duncan$type, tstat, mu=mean(Duncan$income))
stat <- max(abs(stats))
stat

## ------------------------------------------------------------------------
maxt.test <- function(x, g, R = 10000, conf.level = 0.05){
    m <- mean(x)
    stat <- tapply(x, g, tstat, mu = m)
    stat <- max(abs(stat))
    gsize = table(g)
    z <- NULL
    for(k in 1:length(gsize)){
        ## from a t-distribution:
        z <- cbind(z, rt(n=n, df=gsize[k]-1))
    }
    ## z now is a list with length(gsize) elements
    ## we need the maximum absolute value for each element
    z <- abs(z)
    z <- z[cbind(1:n,max.col(z))]
    ## make nice print output
    DATA <- paste(deparse(substitute(x)),
                  "by",
                  deparse(substitute(g)))
    alpha <- 1 - conf.level
    conf.int <- quantile(z, prob = c(alpha/2, (1 - alpha)/2))
    attr(conf.int, "conf.level") <- conf.level
    res <- list(statistic=c(stat = stat),
                   p.value =  mean(z > stat),
                   parameter = c(nrep = R),
                   conf.int = conf.int,
                   data.name = DATA,
                   method = "Maximum t-test")
    class(res) <- "htest"
    res
}

## ------------------------------------------------------------------------
maxt.test(Duncan$income, Duncan$type)

## ------------------------------------------------------------------------
maxp.test <- function(x, g, R = 10000, conf.level = 0.05){
    m <- mean(x)
    stat <- tapply(x, g, tstat, mu=m)
    stat <- max(abs(stat))
    z <- numeric(n)
    for(k in 1:n){
        g1 <- sample(g)
        z[k] <- max(abs(tapply(x, g1, tstat, mu = m)))
    }
    
    retval <- list(tstat=stat, pval=mean(z>stat),
                   name="Permutation maximum t-test")
    class(retval) <- "ttest"
    retval
    ## make nice print output
    DATA <- paste(deparse(substitute(x)),
                  "by",
                  deparse(substitute(g)))
    alpha <- 1 - conf.level
    conf.int <- quantile(z, prob = c(alpha/2, (1 - alpha)/2))
    attr(conf.int, "conf.level") <- conf.level
    res <- list(statistic=c(stat = stat),
                   p.value =  mean(z > stat),
                   parameter = c(nrep = R),
                   conf.int = conf.int,
                   data.name = DATA,
                   method = "Permutation maximum test")
    class(res) <- "htest"
    res
}

## ------------------------------------------------------------------------
maxp.test(Duncan$income, Duncan$type)

## ------------------------------------------------------------------------
boottest <- function(x, g, n=10000){
    lg <- levels(g)
    n1 <- length(x[g == lg[1]])
    N <- length(x)
    mdiff <- abs(mean(x[g == lg[1]]) - mean(x[g == lg[2]]))
    z <- double(n)
    for(k in 1:n){
        x1 <- sample(x, replace=TRUE)
        z[k] <- abs(mean(x1[1:n1]) - mean(x1[(n1+1):N]))
    }
    mean( z > mdiff )
}

## ------------------------------------------------------------------------
Duncan$type <- factor(Duncan$type)
boottest(Duncan$income, Duncan$type)

## ------------------------------------------------------------------------
mvad.test <- function(x, R=1000){ 
	n <- nrow(x)
	## test statistics
  stat <- function(x, N = n){
    cmean <- colMeans(x)
	  cvar  <- var(x)
    u <- mahalanobis(x, center = cmean, cov = cvar)
    z <- pchisq(u, ncol(x))
    p <- sort(z)
    h <- (2 * seq(1:N) - 1) * (log(p) + log(1 - rev(p)))
    A <- -N - mean(h)
		return(A)
  }
  ## value of test statistics for original sample  
  A <- stat(x)
	cmean <- colMeans(x)
  cvar <- var(x)
  p <- numeric(R)
  ## values of test statistics for draws of mvn
  p <- replicate(R, stat(mvrnorm(n, cmean, cvar)))
  pvalue <- mean(p > A)
	RVAL <- list(statistic = c(A = A),
	             method = "A-D radius test",                                  
			         p.value = pvalue)                                      
	class(RVAL) <- "htest" 
	RVAL
}

## ---- eval=FALSE---------------------------------------------------------
## library("MASS")
## set.seed(123)
## r <- replicate(1000, mvad.test(mvrnorm(100, mu=rep(0,3),
##              Sigma=diag(3)))$p.value)
## size <- mean(r < 0.05)

## ---- echo=FALSE---------------------------------------------------------
size = 0.05

## ------------------------------------------------------------------------
size

## ---- cache = TRUE-------------------------------------------------------
library("mvtnorm")
library("ICS")
## Monte Carlo AD test 100 times replicted
r <- replicate(100, mvad.test(rmvt(30, diag(3), df = 5), R=100)$p.value) 
mean(r  < 0.05) 
## Skewness test 1000 times replicted
r2 <- replicate(1000, mvnorm.skew.test(rmvt(30, diag(3), df = 5))$p.value)
mean(r2  < 0.05) 

## ------------------------------------------------------------------------
sessionInfo()

