## ---- message=FALSE, warning=FALSE, tidy=FALSE, eval=TRUE----------------
library("RCurl")
URL <- "https://www.national-lottery.co.uk/results/euromillions/draw-history/csv"
lotto <- read.csv(textConnection(getURL(URL)))

## ------------------------------------------------------------------------
str(lotto)

## ----B05113_06_01, fig.align='center'------------------------------------
numbers <- unlist(c(lotto[,2:5]))
library("ggplot2")
qplot(factor(numbers), xlab = "winning numbers")  + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90)) + 
  scale_y_continuous(breaks=0:10)

## ------------------------------------------------------------------------
simulateDice <- function(n=200){
  A <- c(2, 4, 6)
  B <- 1:4
  C <- 3:6
  AB <- intersect(A, B)
  AC <- intersect(A, C)
  BC <- intersect(B, C)
  ABC <- intersect(AB, AC)
  df <- data.frame(d=rep(0,n), A=rep(0,n), B=rep(0,n), C=rep(0,n),
                   AB=rep(0,n), AC=rep(0,n), BC=rep(0,n), ABC=rep(0,n))
  s <- sample(1:6, n, replace=TRUE)
  df[s %in% ABC, "ABC"] <- 1 
  df[s %in% BC & !(df$ABC == 1), "BC"] <- 1
  df[s %in% AC & !(apply(df[, c("BC","ABC")], 1, function(x) any(x == 1))), "AC"] <- 1
  df[s %in% AB & !(apply(df[, c("AC","BC","ABC")], 1, function(x) any(x == 1))), "AB"] <- 1
  df[s %in% C & !(apply(df[, c("AB","AC","BC","ABC")], 1, function(x) any(x == 1))), "C"] <- 1
  df[s %in% B & !(apply(df[, c("C","AB","AC","BC","ABC")], 1, function(x) any(x == 1))), "B"] <- 1
  df[s %in% A & !(apply(df[, c("B","C","AB","AC","BC","ABC")], 1, function(x) any(x == 1))), "A"] <- 1
  df[, "d"] <- s
  return(df)
}
set.seed(123)
simulateDice(10)

## ------------------------------------------------------------------------
set.seed(123)
s100 <- simulateDice(100)
## count the outcomes regarding the different events:
colSums(s100[, 2:ncol(s100)])

## ------------------------------------------------------------------------
set.seed(123)
s1000 <- simulateDice(1000)
colSums(s1000[, 2:ncol(s1000)])

## ------------------------------------------------------------------------
s1000 <- simulateDice(1000)
colSums(s1000[, 2:ncol(s1000)])

## ----B05113_06_03, echo=FALSE, fig.align='center'------------------------
a <- c(0.65, 0.62, 0.41, 0.43, 0.61, 0.49, 0.44, 0.58, 0.47, 0.53, 0.52, 0.48, 0.51)
plot(1:length(a), a, ylim = c(0,1), xlab="n", yaxt="n", ylab="")
#axis(2, at=0.5, labels = "a", las=1)
abline(h=0.5 + 0.03)
abline(h=0.5 - 0.03)
axis(2, at=0.5+0.03, labels = expression(a + epsilon), las=1)
axis(2, at=0.5-0.03, labels = expression(a - epsilon), las=1)
polygon(x=c(0,14,14,0,0), c(0.5-0.03,0.5-0.03,0.5+0.03,0.5+0.03,0.5-0.03), col="lightgrey")
points(1:length(a), a)
d <- dnorm(seq(-5,5,length.out = 100)) *10 + 6
df <- data.frame(d=d, s=seq(-0.2+0.5, 0.2+0.5, length.out = length(d)))
lines(df$d, df$s)
abline(v=6, col="grey")
d <- dnorm(seq(-10,10,length.out = 100)) *3.5 + 2
df <- data.frame(d=d, s=seq(-1.4+0.5, 1.4+0.5, length.out = length(d)))
lines(df$d, df$s)
abline(v=2, col="grey")

## ------------------------------------------------------------------------
dbinom(x = 0, size = 1, prob = 0.5)

## ------------------------------------------------------------------------
sample(c("head", "number"), size = 1)
# alternativ:
rbinom(n = 1, size = 1, prob = 0.5)

## ------------------------------------------------------------------------
simCoin <- function(n, p = 0.5, repl = 1){
  stopifnot(n > 0 | !is.vector(n) | p < 0 | p > 0 | !is.vector(repl))
  ## function for one simulation
  r <- function(){
    res <- rbinom(n, 1, p)
    tosses <- 1:n
    pA <- cumsum(res) / 1:n
    abserror <- abs(pA - p) 
    return(data.frame(res = res, tosses = tosses, pA = pA, abserror = abserror))
  }
  ## simulation
  df <- r()
  if(repl > 1){
    for(i in 2:repl){
      df <- rbind(df, r())
    }
  }
  ## return
  df$repl <- rep(1:repl, each = n)
  ll <- list(res = df$res, tosses = df$tosses, pA = df$pA, 
             absfehler = df$abserror, repl = as.factor(df$repl))
  class(ll) <- "Coin"
  return(ll)
}
## print 
print.Coin <- function(x, ..., s = NULL){
  if(!is.null(s)){
  cat("After", s, "random draws: the estimated P(A) =", x$pA[s], "\nand the absolute error", x$absfehler[s], "\n")
  } else {
    m <- max(x$tosses)
  cat("After", m, "random draws: the estimated P(A) =", x$pA[m], "\nand the absolute error", x$absfehler[m], "\n")   
  }  
}

## ------------------------------------------------------------------------
## for reproducibility
set.seed(1234) 
# 10 throws
simCoin(10)

## ---- cache=TRUE---------------------------------------------------------
set.seed(1234) 
sim <- simCoin(5000)
print(sim, s=100)
print(sim, s=1000)
print(sim, s=5000)

## ----lehr1---------------------------------------------------------------
plot.Coin <- function(x, y, ...){
  df <- data.frame(res = x$res, tosses = x$tosses, pA = x$pA, repl=x$repl)
  if(length(unique(df$repl)) == 1){
     ggplot(df, aes(x=tosses, y=pA)) + 
      geom_line() + geom_abline(intercept = 0.5) + ylim(c(0,1)) +  
      theme(legend.position="none")
  } else if(length(unique(df$repl)) > 10){
    gg <- ggplot(df, aes(x=tosses, y=pA, group=repl)) + 
      geom_line() + geom_abline(intercept = 0.5) + ylim(c(0,1)) 
    ## add median line and confidence interval
    dfwide <- reshape2::dcast(df, tosses ~ repl, value.var="pA")
    dfwide <- dfwide[, 2:ncol(dfwide)]
    med <- apply(dfwide, 1, median)
    q025 <- apply(dfwide, 1, quantile, 0.025)
    q975 <- apply(dfwide, 1, quantile, 0.975)
    stat <- data.frame(med=med, q025=q025, q975=q975, 
                       n=1:max(x$tosses),
                       repl=max(as.numeric(df$repl)))
    gg + 
      geom_line(data=stat, aes(x = n, y = med), colour = "red", size=1) + 
      geom_line(data=stat, aes(x = n, y = q025), colour = "orange", size=0.7) +
      geom_line(data=stat, aes(x = n, y = q975), colour = "orange", size=0.7) +
      theme(legend.position="none")
  } else {
   ggplot(df, aes(x=tosses, y=pA, colour = repl)) + 
      geom_line() + geom_abline(intercept = 0.5) + ylim(c(0,1))    
  }
}

## ----B05113_06_04--------------------------------------------------------
plot(sim)

## ----B05113_06_05, cache=TRUE--------------------------------------------
set.seed(1234) 
sim <- simCoin(n = 5000, repl = 10)
plot(sim)

## ----B05113_06_06, cache=TRUE--------------------------------------------
sim <- simCoin(n = 5000, repl = 1000)
plot(sim)

## ----B05113_06_07, fig.height=7------------------------------------------
plotbinomcoin <- function(n){
  plot(0:n/n, dbinom(0:n, n, 0.5), type = "h", 
       xlab = paste("relative frequencies (n =", n,")"), 
       ylab = "p")
}
par(mar = c(4,4,0.5,0.5), mfrow = c(4,2))
plotbinomcoin(10)
plotbinomcoin(20)
plotbinomcoin(40)
plotbinomcoin(80)
plotbinomcoin(160)
plotbinomcoin(320)
plotbinomcoin(5000)
plotbinomcoin(10000)

## ------------------------------------------------------------------------
dbinom(0, 1, 0.5)

## ------------------------------------------------------------------------
set.seed(10230)
s <- sample(c(FALSE,TRUE), 5000, replace = TRUE)
s[1:10]
ev <- function(x){
  res <- c(length(x), sum(x), sum(x) / length(x), abs(sum(x) / length(x) - 0.5))
  names(res) <- c("n", "h(A)", "p(A)", "|p(A) - 0.5|")
  return(res)
}
# summary of first 10 outcomes
ev(s[1:10])

## ------------------------------------------------------------------------
# summary of first 100 outcomes
ev(s[1:100])
# summary of first 1000 outcomes
ev(s[1:1000])
# summary of first 5000 outcomes
ev(s)

## ----B05113_06_01x-------------------------------------------------------
p <- numeric(length(s))
for(i in 1:length(s)){
  p[i] <- sum(s[1:i]) / i
}
plot(p, type = "l", xlab = "number of coin flips", ylab = "p(A)")
abline(h = 0.5)

## ----B05113_06_02x-------------------------------------------------------
plotbinomcoin <- function(n){
  plot(0:n/n, dbinom(0:n, n, 0.5), type = "h", xlab = "relative frequency", ylab = "p")
}
par(mar = c(4,4,0.5,0.5), mfrow = c(2,2))
plotbinomcoin(20)
plotbinomcoin(100)
plotbinomcoin(1000)
plotbinomcoin(5000)

## ------------------------------------------------------------------------
newlines <- function(n = 5000){
  s <- sample(c(FALSE,TRUE), n, replace = TRUE)
  for(i in 1:length(s)){ 
    p[i] <- mean(s[1:i])
  }
  p
}

## ----B05113_06_03x, results='hide'---------------------------------------
plot(p, type = "l", xlab = "number of coin flips", ylim = c(0,1), ylab = "p(A)")
abline(h = 0.5)
replicate(10, lines(newlines()))

## ----B05113_06_04x, cache=TRUE-------------------------------------------
plot(p, type = "l", xlab = "number of coin flips", ylim = c(0,1), ylab = "p(A)")
abline(h = 0.5)
## calculate 1000 sequences
n1 <- replicate(1000, newlines())
med <- apply(n1, 1, median)
q1 <- apply(n1, 1, quantile, 0.025)
q2 <- apply(n1, 1, quantile, 0.975)
apply(n1, 2, lines)
lines(med, col = "red")
lines(q1, col = "green", cex = 2)
lines(q2, col = "green", cex = 2)

## ---- echo=FALSE, eval=FALSE---------------------------------------------
## plot(function(x) dnorm(x), -4, 4,
##      main = "normal")
## curve(dnorm(x), add = TRUE, col = "red", lwd = 2)

## ---- warning=FALSE, message=FALSE---------------------------------------
cltSim <- function (n = 1, reps = 10000, nclass = 16, pop = TRUE, estimator = mean) {
    old.par <- par(oma = c(0, 0, 1.5, 0), mfrow = c(2, 2), mar = c(4,4,2,0.5))
    on.exit(par(old.par))
    ## normal:
    norm.mat <- matrix(rnorm(n * reps), ncol = n)
    norm.mean <- apply(norm.mat, 1, estimator)
    x <- seq(min(norm.mean), max(norm.mean), length = 50)
    normmax <- max(dnorm(x, mean(norm.mean), sd(norm.mean)))
    tmp.hist <- hist(norm.mean, plot = FALSE, prob = TRUE, nclass = nclass)
    normmax <- max(tmp.hist$density, normmax) * 1.05
    hist(norm.mean, main = "normal", xlab = "x", col = "skyblue", 
        prob = TRUE, ylim = c(0, normmax), nclass = nclass)
    lines(x, dnorm(x, mean(norm.mean), sd(norm.mean)))
    ## exponential:
    exp.mat <- matrix(rexp(n * reps, 1/3), ncol = n)
    exp.mean <- apply(exp.mat, 1, estimator)
    x <- seq(min(exp.mean), max(exp.mean), length = 50)
    expmax <- max(dnorm(x, mean(exp.mean), sd(exp.mean)))
    tmp.hist <- hist(exp.mean, plot = FALSE, prob = TRUE, nclass = nclass)
    expmax <- max(tmp.hist$density, expmax) * 1.05
    hist(exp.mean, main = "exponential", xlab = "x", col = "skyblue", 
        prob = TRUE, ylim = c(0, expmax), nclass = nclass)
    if(pop) lines(x, dexp(x, 1/3)) else lines(x, dnorm(x, mean(exp.mean), sd(exp.mean)))
    ## uniform:
    unif.mat <- matrix(runif(n * reps), ncol = n)
    unif.mean <- apply(unif.mat, 1, estimator)
    x <- seq(min(unif.mean), max(unif.mean), length = 50)
    unimax <- max(dnorm(x, mean(unif.mean), sd(unif.mean)))
    tmp.hist <- hist(unif.mean, plot = FALSE, prob = TRUE, nclass = nclass)
    unimax <- max(tmp.hist$density, unimax) * 1.05
    hist(unif.mean, main = "uniform", xlab = "x", col = "skyblue", 
        prob = TRUE, ylim = c(0, unimax), nclass = nclass)
    if(pop) lines(x, dunif(x)) else lines(x, dnorm(x, mean(unif.mean), sd(unif.mean)))
    ## Beta:
    beta.mat <- matrix(rbeta(n * reps, 0.35, 0.25), ncol = n)
    beta.mean <- apply(beta.mat, 1, estimator)
    x <- seq(min(beta.mean), max(beta.mean), length = 50)
    betamax <- max(dnorm(x, mean(beta.mean), sd(beta.mean)))
    tmp.hist <- hist(beta.mean, plot = FALSE, prob = TRUE, nclass = nclass)
    betamax <- max(tmp.hist$density, betamax)
    hist(beta.mean, main = "Beta", xlab = "x", col = "skyblue", 
        prob = TRUE, ylim = c(0, betamax), nclass = nclass)
    if(pop){
      lines(x, dbeta(x, 0.35, 0.25))
      mtext(paste("Populations"), outer = TRUE, cex = 1.2)
    } else {
      lines(x, dnorm(x, mean(beta.mean), sd(beta.mean)))
      mtext(paste("sample size =", n), outer = TRUE, cex = 1.2)
    }
}

## ----B05113_06_08, warning=FALSE, message=FALSE--------------------------
cltSim()

## ----B05113_06_09, warning=FALSE, message=FALSE--------------------------
cltSim(2, pop = FALSE)

## ----B05113_06_10, warning=FALSE, message=FALSE--------------------------
cltSim(10, pop = FALSE)

## ----B05113_06_11, warning=FALSE, message=FALSE--------------------------
cltSim(n = 100, pop = FALSE, estimator = median)

## ------------------------------------------------------------------------
library("car")
data("Prestige")
m <- mean(Prestige$income)
m
p <- dim(Prestige)[1]
se <- sd(Prestige$income) / sqrt(p)
tval <- qt(0.975, df = p - 1)
cat(paste("KI: [", round(m - tval * se, 2), ",", round(m + tval * se, 2), "]"))

## ---- B05113_06_12-------------------------------------------------------
set.seed(11112)
alpha <- 0.05
normval <- qnorm(1 - alpha/2)
numsamp <- 50; numsim <- 10 
normmat <- matrix(0, nrow = numsim, ncol = 2)
y <- 1:numsim; ymat <- rbind(y, y)
for (i in 1:numsim) {
  samp <- rexp(numsamp)    # generate random exponentials
  sampmean <- mean(samp)
  sampse <- sqrt(var(samp) / numsamp)
  normmat[i, ] <- c(sampmean - normval * sampse, sampmean + normval * sampse)
}
matplot(t(normmat), ymat , pch = " ", yaxt = "n", ylab = "", xlab="confidence intervals") # empty plot
matlines(t(normmat), ymat, lty = rep(1, numsim), col = 1)
abline(v = 1)

## ------------------------------------------------------------------------
sessionInfo()

