## ---- message=FALSE,warning=FALSE----------------------------------------
dat <- matrix(c(104,11037,189,11034),2,2, byrow=TRUE)
dat
library("vcd")
## confidence intervals
confint(oddsratio(dat, log=FALSE))

## ---- cache=TRUE---------------------------------------------------------
## original surveyed data
s1 <- rep(c(TRUE, FALSE), times = c(104, 11037))  
s2 <- rep(c(TRUE, FALSE), times = c(189, 11034))
## function for drawing a bootstrap sample 
## and estimating the boostrap replicate
boot_aspirin <- function(s1, s2){
  ## odds ratio
  sum(sample(s1, replace = TRUE)) / sum(sample(s2, replace = TRUE))
}  
## 10000 draws and replicates
boot_repl <- replicate(10000, boot_aspirin(s1, s2))  
## confidence interval
quantile(boot_repl, c(0.025, 0.975))

## ------------------------------------------------------------------------
x <- c(5, 7, 8, 2, 15, 12, 3)

## ------------------------------------------------------------------------
## for reproducibility we use a seed
set.seed (123) 
## bootstrap sample (with replacement)
s1 <- sample(x, replace = TRUE)
s1

## ------------------------------------------------------------------------
s2 <- sample(x, replace = TRUE)
s2

## ------------------------------------------------------------------------
mean(x)

## ------------------------------------------------------------------------
mean(s1)
mean(s2)

## ------------------------------------------------------------------------
library("car") 
data("Prestige") 
mean(Prestige$income)

## ------------------------------------------------------------------------
set.seed(123) 
mean(sample(Prestige$income, replace = TRUE))

## ------------------------------------------------------------------------
mean(sample(Prestige$income, replace = TRUE))

## ------------------------------------------------------------------------
library("robustbase")
## data
df <- Prestige[, c("income", "prestige")]
## robust MCD-based covariance
covMcd(df, cor=TRUE)$cor

## ------------------------------------------------------------------------
set.seed(1234) ## for reproducibility (seed)
## standard error with bootstrap
sd(replicate(200, 
        covMcd(df[sample(rownames(df),  replace=TRUE), ], 
               cor=TRUE)$cor[1,2]))

## ---- message=FALSE,warning=FALSE----------------------------------------
library("boot")
## function for bootstrapping in boot
cr <- function(d, w) covMcd(d[w, ], cor=TRUE)$cor[1,2]
## application of the boot function
boot(data=df, statistic=cr, R=200)

## ------------------------------------------------------------------------
## MASS needed for drawing random numbers from multivariate normal
library("MASS")
## parameters from empirical data (income and prestige)
m1 <- colMeans(df)
m2 <- cov(df)
## number of observations
n <- dim(df)[1]
## parametric bootstrap 
parboot <- replicate(200,  covMcd(mvrnorm(n, mu=m1, Sigma=m2), cor=TRUE)$cor[1,2])
## standard error
sd(parboot)

## ------------------------------------------------------------------------
## parametric bootstrap
system.time(sd(replicate(5000,  
        cor(mvrnorm(n, mu=m1, Sigma=m2))[1,2])))
## non-parametric bootstrap
system.time(sd(replicate(5000, 
        cor(df[sample(rownames(df),  replace=TRUE), ])[1,2])))

## ------------------------------------------------------------------------
## parametric bootstrap
range(replicate(20, sd(replicate(50,  
                          cor(mvrnorm(n, mu=m1, Sigma=m2))[1,2]))))
## non-parametric bootstrap
range(replicate(20, sd(replicate(50, 
        cor(df[sample(rownames(df),  replace=TRUE), ])[1,2]))))

## ----B05113_07_03--------------------------------------------------------
## parametric bootstrap
pboot <-replicate(1000,  
	cor(mvrnorm(n, mu=m1, Sigma=m2))[1,2])
## non-parametric bootstrap
npboot <- replicate(1000, 
	cor(df[sample(rownames(df), 
	replace=TRUE), ])[1,2])
mi <- min(pboot, npboot)
ma <- max(pboot, npboot)
## Now plot Figure 7.3.
par(mfrow=c(1,2), pty="s")
hist(npboot, 
	main="non-parametric", 
	xlab="1000 bootstrap replicates", 
	xlim=c(mi,ma), breaks = 25)
hist(pboot, 
	main="parametric", 
	xlab="1000 bootstap replicates",
	xlim=c(mi,ma), breaks = 25)

## ------------------------------------------------------------------------
x <- Prestige[, "income"]
v <- function(x) sd(x) / mean(x)
v(x)

## ------------------------------------------------------------------------
vboot <- replicate(1000, v(sample(x, replace = TRUE)))

## ------------------------------------------------------------------------
vbias <- mean(vboot) - v(x)
vbias

## ------------------------------------------------------------------------
v(x) - vbias

## ------------------------------------------------------------------------
cat("CI(e): [", v(x) - vbias - qt(0.975, length(x)-1) * sd(vboot), ", ", v(x) - vbias + qt(0.975, length(x)-1) * sd(vboot), " ]\n") 

## ------------------------------------------------------------------------
cat("CI(p): [", quantile(vboot, 0.025), ", ", quantile(vboot, 0.975), " ]\n")

## ------------------------------------------------------------------------
cat("CI(h): [", 2*v(x) - quantile(vboot, 0.975), ", ", 2*v(x) - quantile(vboot, 0.025), " ]\n")

## ------------------------------------------------------------------------
## some crazy data (10 outliers)
x <- c(rnorm(100), rnorm(10,10))
## non-parametric bootstrap replicates
mb <- replicate(10000, mean(sample(x, replace=TRUE)))
## percentile method
cat("\nCI(perc): [", quantile(mb, 0.025), ", ", quantile(mb, 0.975), " ]\n") 
## BCa method
library("bootstrap")
b <- bcanon(x, 10000, mean, alpha=c(0.025,0.975))
cat("\nCI(BCa): [", b$confpoints[1,2], ", ", b$confpoints[2,2], " ]\n") 

## ----B05113_07_04, echo=FALSE--------------------------------------------
n <- length(x)
## histogram of data
hist(x, main="Daten")
## draw mean
segments(x0=mean(x), x1=mean(x), y0=-1, y1=10, col="red", lwd=3)
## draw confidence intervals
arrows(x0=quantile(mb, 0.025), x1=quantile(mb, 0.975), y0=5, y1=5, code=3, length=0.1)
arrows(x0=b$confpoints[1,2], x1=b$confpoints[2,2], y0=4, y1=4, code=3, length=0.1, col="blue")
arrows(x0=mean(x)-qt(0.975, n-1)*sd(x)/sqrt(n), x1=mean(x)+qt(0.975, n-1)*sd(x)/sqrt(n), y0=3, y1=3, code=3, length=0.1, col="brown")
h1 <- 2*mean(x) - quantile(mb, 0.975)
h2 <- 2*mean(x) - quantile(mb, 0.025)
arrows(x0=h1, x1=h2, y0=2, y1=2, code=3, length=0.1, col="orange")
bias <- mean(mb) - mean(x)
h1 <- mean(x) - bias - qt(0.975, n-1) * sd(mb)
h2 <- mean(x) - bias + qt(0.975, n-1) * sd(mb)
arrows(x0=h1, x1=h2, y0=1, y1=1, code=3, length=0.1, col="grey")
legend("topright", legend=c("percentile", "BCa", "classical", "Hall", "bias corrected","arithm. mean"), 
		lwd=c(1,1,1,1,1,3), col=c("black","blue","brown","orange","grey","red"))

## ----B05113_07_05, echo = FALSE------------------------------------------
par(mar = c(1,3,0.5,0.1))
hist(x, main="", xlim=c(0.1,1.9), ylim=c(0,10))
segments(x0=mean(x), x1=mean(x), y0=-1, y1=10, col="red", lwd=3)
arrows(x0=quantile(mb, 0.025), x1=quantile(mb, 0.975), y0=5, y1=5, code=3, length=0.1)
arrows(x0=b$confpoints[1,2], x1=b$confpoints[2,2], y0=4, y1=4, code=3, length=0.1, col="blue")
arrows(x0=mean(x)-qt(0.975, n-1)*sd(x)/sqrt(n), x1=mean(x)+qt(0.975, n-1)*sd(x)/sqrt(n), y0=3, y1=3, code=3, length=0.1, col="brown")
h1 <- 2*mean(x) - quantile(mb, 0.975)
h2 <- 2*mean(x) - quantile(mb, 0.025)
arrows(x0=h1, x1=h2, y0=2, y1=2, code=3, length=0.1, col="orange")
bias <- mean(mb) - mean(x)
h1 <- mean(x) - bias - qt(0.975, n-1) * sd(mb)
h2 <- mean(x) - bias + qt(0.975, n-1) * sd(mb)
arrows(x0=h1, x1=h2, y0=1, y1=1, code=3, length=0.1, col="grey")
legend("topright", legend=c("percentile", "BCa", "classical", "Hall", "bias corrected","arithm. mean"), lwd=c(1,1,1,1,1,3), col=c("black","blue","brown","orange","grey","red"))

## ------------------------------------------------------------------------
## toy data
x <- c(1,2,2,2,2,2,7,8,9,10)
## remember, this is the variation coefficient
v <- function(x) sd(x)/mean(x)
## initialisation
n <- length(x)
vjack <- rep(0, n-1)
vpseudo <- rep(0, n)
## leave-one-out jackknife
for(i in 1:n){
  vjack[i] <- v(x[-i])
}
## jackknife pseudo values
pseudo <- n * v(x) - (n-1)*vjack
## confidence interval with pseudo values
cat("\nKI(pseudo): [", mean(pseudo) - qt(0.975, n-1) * sd(pseudo)/n, ", ", mean(pseudo) + qt(0.975, n-1) * sd(pseudo)/n, " ]\n")
## confidence interval with classical jackknife
se2 <- sqrt(((n-1)/n) * sum((vjack - mean(vjack))^2))
jbias <- (n-1) * (mean(vjack) - v(x))
cat("\nKI(jse): [", v(x) - jbias - qt(0.975, n-1) * se2 , ", ", v(x) - jbias + qt(0.975, n-1) * se2, " ]\n") 

## ------------------------------------------------------------------------
quantile(replicate(10000, v(sample(x, replace = TRUE))), c(0.025, 0.975))

## ------------------------------------------------------------------------
## sample estimate
median(x)
## non-parametric bootstrap
qu <- quantile(replicate(10000, 
          median(sample(x, replace = TRUE))), 
        c(0.025, 0.975))
cat("\nCI(boot): [", qu[1], ", ", qu[2], " ]\n")
## jackknife, initialisation
n <- length(x)
jack <- rep(0, n-1)
pseudo <- rep(0, n)
for(i in 1:n){
  jack[i] <- median(x[-i])
}
## jackknife pseudo values approach
pseudo <- n * median(x) - (n-1)*jack
cat("\nCI(pseudo): [", mean(pseudo) - qt(0.975, n-1) * sd(pseudo)/n, ", ", mean(pseudo) + qt(0.975, n-1) * sd(pseudo)/n, " ]\n") 
## classical jackknife
se2 <- sqrt(((n-1)/n) * sum((jack - mean(jack))^2))
jbias <- (n-1) * (mean(jack) - median(x))
cat("\nCI(jse): [", median(x) - jbias - qt(0.975, n-1) * se2 , ", ", median(x) - jbias - qt(0.975, n-1) * se2, " ]\n") 

## ------------------------------------------------------------------------
## all combinations
co <- combn(10, 2) 
## first 6 out of 45
co[, 1:6]
## delete-2 jackknife replicates
jack_d <- apply(co, 2, function(i) median(x[-i])) 
## standard error
n <- length(x)
r <- 2 / n
## n over 2
nd <- choose(n, 2)
## inflation factor
fac <- r / nd
m <- mean(jack_d)
## standard error
se_d <- sqrt(fac * sum((jack_d - m)^2))
## confidence interval:
cat("\nKI(jse): [", median(x)  - qt(0.975, n-1) * se_d , ", ", median(x)  + qt(0.975, n-1) * se_d, " ]\n") 

## ------------------------------------------------------------------------
choose(45, 10)

## ------------------------------------------------------------------------
data(Prestige, package = "car")
x <- Prestige$income
v <- function(x, indices){ 
  x <- x[indices]
  est <- sd(x)/mean(x)
  return(est)
}

## ----B05113_07_06--------------------------------------------------------
library("boot")
bx <- boot(x, v, 2000)
## Figure 7.6.
jack.after.boot(bx)

## ----B05113_07_07--------------------------------------------------------
set.seed(12345)
## generate some data
x1 <- runif(100, 0, pi)
s <- data.frame(x1 = x1, x2 = rnorm(100, 0, 0.1) + sin(x1))
## plot data points for Figure 7.6
plot(s, xlab = "x", ylab = "y")
## simple model
reg1 <- lm(s[, 2] ~ s[, 1], data = s)
abline(reg1, lwd = 2)
## sinus model
reg2 <- lm(s[, 2] ~ sin(s[, 1]), data = s)
f <- function(x, coef) coef[1] + coef[2] * sin(x)
ss <- seq (-0.02, 3.2, 0.01)
lines(ss, f (ss, coef(reg2)), lty = 2, col = "blue", lwd = 2)
## locally reweighted regression
reg3 <- lowess(x = s[, 1], y = s[, 2], f = 0.1)
lines (reg3, col = "red", lty = 3, lwd = 2)
## legend for Figure 7.6
legend("bottom", col = c("black", "blue", "red"), 
       lty = c(1, 2, 3), lwd = c(2, 2, 2),
       legend = c(expression(y = beta[0] + beta[1]*x), 
                  expression(y = beta[0] + sin(x)),
                  "loess, 0.1"))

## ------------------------------------------------------------------------
str(s)

## ------------------------------------------------------------------------
## index of training data
training_ind <- sample(1:nrow(s), 70) 
## index of test data
test_ind <- which(!(1:100 %in% training_ind))

## ------------------------------------------------------------------------
lm1 <- lm(s[training_ind, 2] ~ s[training_ind, 1], data = s)

## ------------------------------------------------------------------------
## expected values
f <- function(x) reg1$coef[1] + reg1$coef[2] * x
## prediction error, squared sum of expected and observed test data
error <- sum((f(s[test_ind, 1]) - s[test_ind, 2])^2)
error

## ----B05113_07_08, echo=FALSE, fig.width=7, fig.height=7, fig=TRUE-------
par(mfrow = c(2,2), pty = "s", mar = c(2,2,1.5,0.5))
## training and test data
training <- as.numeric(sample(rownames(s), 70))
plot(s, main = "OLS regression", pch = 20, xlab = "", ylab = "")
points(s[training, ], pch = 20)
points(s[-training, ], col = "red", pch = 15)
legend("bottom", pch = c(20,15), col = c("black","red"), 
       legend = c("training", "test"))
## training data plus regression line
plot(s[training, ], main = "OLS regression", pch = 20, xlab = "", ylab = "")
reg1 <- lm(s[training, 2] ~ s[training, 1], data = s)
abline(reg1)
legend("bottom", pch = 20, col = "black", legend = c ("training"))
## evaluation on test data
plot(s, main = "OLS regression", pch = 20, xlab = "", ylab = "")
points(s[-training, ], col = "red", pch = 15)
f <- function(x) reg1$coef[1] + reg1$coef[2] * x
segments(x0 = s[-training, 1], x1 = s[-training, 1], y0 = s[-training, 2], y1 = f(s[-training, 1]), col = "red")
abline(reg1)
legend("bottom", pch = c(20,15), col = c("black", "red"), legend = c("training", "test"))
error1 <- sum((f(s[-training, 1]) - s[-training, 2])^2)
# error1
text(x = 1.4, y = 0.16, expression(sum(e[i]^2, i = test) == 4.609), col = "red", cex=1.4)
## sinus model, evaluation
plot(s, main = "OLS regression, sinus", pch = 20, xlab = "", ylab = "")
points(s[-training, ], col = "red", pch = 15)
reg2 <- lm(s[training, 2] ~ sin(s[training, 1]), data = s)
f <- function(x) reg2$coef[1] + reg2$coef[2] * sin(x)
f2 <- function(x, coef) coef[1] + coef[2] * sin(x)
segments(x0 = s[-training, 1], x1 = s[-training, 1], 
         y0 = s[-training, 2], y1 = f(s[-training, 1]), col = "blue" )
#abline (reg2, col = "black")
ss <- seq(-0.02,3.2,0.01)
lines(ss, f2 (ss, coef (reg2)), lty = 2, col = "blue")
legend("bottom", pch = c(20,15), col = c("black", "red"), legend = c("training", "test"))
error1 <- sum((f(s[-training, 1]) - s[-training, 2])^2)
# error1
text(x = 1.4, y = 0.16, expression(sum(e[i]^2, test) == 0.467), col = "blue", cex=1.4)

## ------------------------------------------------------------------------
f <- function (x) reg2$coef[1] + reg2$coef[2] * sin(x)
error1 <- numeric(1000)
n <- nrow(s)
for (i in 1:1000){
  training_ind <- sample(1:n, 70)
  reg2 <- lm(s[training, 2] ~ sin(s[training, 1]), data = s)
  error1[i] <- sum((f(s[-training_ind, 1]) - s[-training_ind, 2])^2)
}
summary (error1)

## ----B05113_07_09, echo = FALSE, fig.width=7, fig.height=7, fig=TRUE-----
par(mfrow = c(2,2), pty = "s", mar = c(2,2,1.5,0.5))
for (i in 1:4){
plot(s, main = paste ( "OLS, Model 2, IndexTestObs .:", i), pch = 20, 
xlab = "", ylab = "")
training = (1:nrow(s))[-i]
# print("omitted th observation:", paste(i, sep = ""))
# print(training)
points(s[-training, ], col = "red", pch = 15, cex = 1.4)
reg2 <- lm(s[training, 2] ~ sin(s[training, 1]), data = s)
f <- function (x) reg2$coef[1] + reg2$coef [2] * sin(x)
f2 <- function (x, coef) coef[1] + coef[2] * sin(x)
segments(x0 = s[-training, 1], x1 = s[-training, 1], y0 = s[-training, 2], y1 = f(s[-training, 1]), col = "blue" )
#abline (reg2, col = "black")
ss <- seq(-0.02,3.2,0.01)
lines (ss, f2 (ss, coef (reg2)), lty = 2, col = "blue")
legend("bottom", pch = c(20,15), col = c("black", "red"), legend = c("training", "test"))
error1 <- sum ((f(s[-training, 1]) - s[-training, 2])^2)
# print(paste("errors regarding the.", i, "- th observation", sep = ""))
# print (error1)
# cat("\n")
text(x = 1.4, y = 0.16, 
     paste("e", " = ", round(error1,5), sep=""), 
     col="blue", cex=1.4)
}

## ------------------------------------------------------------------------
n <- nrow(s)
error1 <- numeric(n)
for(i in 1:n){
  reg2 <- lm(x2 ~ x1, data = s[-i, ])
  error1[i] <- sum((f(s[i, 1]) - s[i, 2])^2)
}
mean(error1)

## ---- warning=FALSE, message=FALSE---------------------------------------
library("cvTools")
fit <- lm(x2 ~ x1, data = s)
# perform cross-validation
cvFit(fit, data = s, y = s$x2, cost = mspe, 
    K = 5, R = 10, seed = 1234)

## ------------------------------------------------------------------------
library("robustbase")
# set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
## compare LS, MM and LTS regression
## perform cross-validation for an LS regression model
fitLm <- lm(prestige ~ ., data = Prestige)
cvFitLm <- cvLm(fitLm, cost = mspe, 
    folds = folds)
fitLm2 <- lm(prestige ~ income:type + education + women, data = Prestige)
cvFitLm2 <- cvLm(fitLm, cost = mspe, 
    folds = folds)
## perform cross-validation for an MM regression model
fitLmrob <- lmrob(prestige ~ ., data = Prestige)
cvFitLmrob <- cvLmrob(fitLmrob, cost = mspe, 
    folds = folds)
fitLmrob2 <- lmrob(prestige ~ income:type + education + women, data = Prestige)
cvFitLmrob2 <- cvLmrob(fitLmrob, cost = mspe, 
    folds = folds)
## compare cross-validation results
cvSelect(LS = cvFitLm, LS2 = cvFitLm2, 
         MM = cvFitLmrob, MM2 = cvFitLmrob2)

## ------------------------------------------------------------------------
sessionInfo()

