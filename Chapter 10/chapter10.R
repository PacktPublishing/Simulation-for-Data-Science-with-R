## ------------------------------------------------------------------------
library("mvtnorm")
synth <- rmvnorm(100, mean = rep(0,5), sigma = diag(5))
## first three observations
head(synth, 3)

## ------------------------------------------------------------------------
data(Prestige, package = "car")
## first three observations of Prestige
head(Prestige, 3)
## subset of variables
real <- Prestige[, 1:4]

## ------------------------------------------------------------------------
## set seed for reproducibility
set.seed(12)
## simulate from multivariate normal
synth2 <- data.frame(rmvnorm(100, mean = colMeans(real), sigma = cov(real)))
colnames(synth2) <- colnames(real)
## first three observations
head(synth2, 3)

## ------------------------------------------------------------------------
summary(real$women)
summary(synth2$women)

## ----B05113_10_01--------------------------------------------------------
par(mar = c(4,4,0.2,0.2))
plot(prestige ~ income, data = real)
points(prestige ~ income, data = synth2, col = "red", pch = 20)
legend("bottomright", legend = c("original/real", "synthetic"), col = 1:2, pch = c(1,20))

## ----B05113_10_02--------------------------------------------------------
library("robustbase")
cv <- covMcd(real)
synth3 <- rmvnorm(100, mean = cv$center, sigma = cv$cov)
par(mfrow = c(1,2), mar = c(4,4,0.2,0.2))
plot(prestige ~ income, data = real)
points(prestige ~ income, data = synth3, col = "red", pch = 20)
## add outliers
rmd <- mahalanobis(real, center = cv$center, cov = cv$cov)
## outliers defined by large Mahalanobis distances
out <- rmd > qchisq(0.975, ncol(real) - 1)
cv_good <- covMcd(real[!out, ])
## simulate good points
synth3_good <- rmvnorm(100, mean = cv_good$center, sigma = cv_good$cov)
cv_out <- covMcd(real[out, ])
## simulate outliers
synth3_out <- rmvnorm(100, mean = cv_out$center, sigma = cv_out$cov)
## Figure 10.2.
plot(prestige ~ income, data = real)
points(prestige ~ income, data = synth3_good, col = "red", pch = 20)
points(prestige ~ income, data = synth3_out, col = "red", pch = 20)

## ---- warning=FALSE,message=FALSE----------------------------------------
synth4 <- real
lm1 <- lm(education ~ ., data = real)
synth4$education <- predict(lm1, synth4[, 2:ncol(synth4)]) + sample(residuals(lm1))

## ---- warning=FALSE,message=FALSE----------------------------------------
p <- ncol(real)
for(i in 1:ncol(real)){
  df <- real[, i]
  df <- cbind(df, real[,-i])
  colnames(df)[1] <- "response"
  lm1 <- lm(response ~ ., data = df)
  synth4[, i] <- predict(lm1, synth4[, -i]) + sample(residuals(lm1))
}

## ------------------------------------------------------------------------
simLatent <- function(n = 200, p = 50, k = 3){
  T <- matrix(rnorm(n * k, 0, 1), ncol = k)
  B <- matrix(runif(p * k, -1, 1), ncol = k)
  X <- T %*% t(B)
  E <-  matrix(rnorm(n * p, 0, 0.1), ncol = p)
  XE <- X + E
  return(XE)  
}

## ------------------------------------------------------------------------
x <- simLatent(n = 50, p = 1000, k = 6)
dim(x)

## ---- warning=FALSE,message=FALSE----------------------------------------
library("simPop")
data("eusilcS")
dim(eusilcS)

## ------------------------------------------------------------------------
length(unique(eusilcS$db030))

## ------------------------------------------------------------------------
inp <- specifyInput(eusilcS, hhid = "db030", hhsize = "hsize", 
                     strata = "db040", weight = "rb050")

## ------------------------------------------------------------------------
print(inp)

## ---- warning=FALSE,message=FALSE----------------------------------------
synthP <- simStructure(data = inp, 
                        method = "direct", 
                        basicHHvars = c("age", "rb090", "db040"))

## ------------------------------------------------------------------------
synthP <- simCategorical(synthP, 
                          additional = c("pl030", "pb220a"), 
                          method = "multinom")

## ---- cache=TRUE---------------------------------------------------------
synthP <- simContinuous(synthP, additional = "netIncome",
                         upper = 200000, equidist = FALSE,
                         imputeMissings = FALSE)

## ------------------------------------------------------------------------
synthP

## ---- results='hide', message=FALSE,warning=FALSE------------------------
library("mice")
library("VIM")
x <- orig <- simLatent(n = 50, p = 10, k = 6)
## evaluation criteria
eval <- function(real, imputed, nas){
  sqrt(sum((real - imputed)^2)) / nas
}
set.seed(123)
R <- 100
e1 <- e2 <- e3 <- e4 <- numeric(R)
for(i in 1:R){
  x <- orig
  x[sample(1:nrow(x), 10), 1] <- NA    
  e1[i] <- eval(orig, e1071::impute(x), 10)
  e2[i] <-   eval(orig, kNN(data.frame(x), imp_var = FALSE), 10)
  e3[i] <-   eval(orig, irmi(x), 10)
  e4[i] <-   eval(orig, complete(mice(x, m = 1, printFlag = FALSE)), 10)
}
df <- data.frame("error" = c(e1,e2,e3,e4), method = rep(c("mean", "kNN", "irmi", "mice"), each = R))

## ----B05113_10_03--------------------------------------------------------
library("ggplot2")
ggplot(df, aes(x = method, y=error)) + geom_boxplot() + theme(text = element_text(size = 20)) + theme_bw()

## ------------------------------------------------------------------------
library("robustbase")
set.seed(123)
x <- rexp(n = 50, rate = 1)
mean(x)
huberM(x)$mu

## ------------------------------------------------------------------------
m <- mean(replicate(10000, mean(rexp(n = 50, rate = 1))))
m
m - 1

## ------------------------------------------------------------------------
mh <- mean(replicate(10000, huberM(rexp(n =50, rate = 1))$mu))
mh
mh - 1

## ------------------------------------------------------------------------
set.seed(123)
alpha <- 0.05
ci <- function(x, z = qnorm(1 - alpha / 2)){
  s <- rexp(n = 50, rate = 1)
  m <- mean(s)
  se <- sd(s) / sqrt(50)
  ci_est <- c(m - z * se, m + z *se)
  ci_est
}
ci()

## ---- cache=TRUE---------------------------------------------------------
set.seed(123)
ciR_n <- replicate(100000, ci())
isCovered <- function(x){
  apply(x, 2, function(x){
  if(x[1] > 1 & x[2] > 1) return(FALSE)
  if(x[1] < 1 & x[2] < 1) return(FALSE)
  return(TRUE)})
}
cn <- isCovered(ciR_n)
sum(cn) / length(cn)

## ---- cache=TRUE---------------------------------------------------------
ciR_t <- replicate(100000, ci(z = qt(1 - alpha / 2, 49)))
ct <- isCovered(ciR_t)
sum(ct) / length(ct)

## ---- cache = TRUE-------------------------------------------------------
ci_boot <- function(x, R = 1000){
  s <- rexp(n = 50, rate = 1)
  ci_est <- quantile(replicate(R, 
            mean(sample(s, replace = TRUE))), 
            c(0.025, 0.975))
  return(ci_est)
}
ciR_boot <- replicate(1000, ci_boot())
cb <- isCovered(ciR_boot)
sum(cb) / length(cb)

## ----B05113_10_04, message=FALSE,warning=FALSE---------------------------
df <- data.frame(t(ciR_n))
df <- data.frame(rbind(t(ciR_n), t(ciR_t), t(ciR_boot)))
df$method <- rep(c("normal", "t", "boot"), times = c(100000,100000,1000))
colnames(df) <- c("lower", "upper", "method")
library("reshape2")
df <- melt(df)
library("ggplot2")
ggplot(df, aes(x = value, colour = method)) + geom_density() + facet_wrap(~ variable) + theme(text = element_text(size=16))

## ---- cache = TRUE, message=FALSE,warning=FALSE--------------------------
simMean <- function(simFun = function(x) rnorm(100)){
  ## 1000 samples
  set.seed(123)
  R <- 1000
  m <- list()
  ## 1000 data sets
  for(i in 1:R){
    m[[i]] <- simFun()
  }
  ## estimation
  df <- data.frame("thetahat" = c(sapply(m, mean), sapply(m, mean, trim = 0.1), sapply(m, median), sapply(m, function(x) huberM(x)$mu)), 
                   "method" = rep(c("mean","trim","median","huber"), each = R)) 
  ## summary
  vm <- var(df[df$method == "mean", 1])
  df %>% 
    group_by(method) %>% 
    summarize("bias" = mean(thetahat) - 0, 
              "variance" = var(thetahat),
              "mse" = variance + bias^2,
              "re" = vm / var(thetahat))
}

## ------------------------------------------------------------------------
library("robustbase"); library("dplyr")
simMean()

## ---- cache = TRUE, warning=FALSE, message=FALSE-------------------------
set.seed(123)
simMean(simFun = function(){c(rnorm(95), rnorm(5,15))})

## ---- message=FALSE, warning=FALSE---------------------------------------
library("simFrame"); library("robCompositions"); library("mvtnorm"); library("mice")
set.seed(123)

## ------------------------------------------------------------------------
## data generation 
crnorm <- function(n, mean, sigma) data.frame(isomLRinv(rmvnorm(n, mean, sigma)))
sigma <- matrix(c(1, -0.5, 1.4, -0.5, 1, -0.6, 1.4, -0.6, 2), 3, 3)
## data control class
dc <- DataControl(size = 150, distribution = crnorm,
      dots = list(mean = c(0, 2, 3), sigma = sigma))

## ------------------------------------------------------------------------
nc <- NAControl(NArate = c(0.05, 0.1))

## ------------------------------------------------------------------------
sim <- function(x, orig) {
     i <- apply(x, 1, function(x) any(is.na(x)))
     ni <- length(which(i))
     x <- x[, -ncol(x)]
     xMean <- e1071::impute(x)
     xMice <- mice(x, printFlag = FALSE, diagnostics = FALSE, m = 1)
     xMice <- complete(xMice)
     xKNNa <- impKNNa(x)$xImp
     xLS <- impCoda(x, method = "lm")$xImp
     xLTSrob <- impCoda(x, method = "ltsReg")$xImp
     c(xMean = aDist(xMean, orig)/ni,
       xMice = aDist(xMice, orig)/ni,
       knn = aDist(xKNNa, orig)/ni, 
       LS = aDist(xLS, orig)/ni,
       LTSrob = aDist(xLTSrob, orig)/ni)
}

## ---- cache = TRUE, message=FALSE, warning=FALSE-------------------------
results <- runSimulation(dc, 
                         nrep = 25, 
                         NAControl = nc, 
                         fun = sim)

## ------------------------------------------------------------------------
aggregate(results)

## ----B05113_10_05--------------------------------------------------------
simBwplot(results)

## ---- cache=TRUE, message=FALSE, warning=FALSE---------------------------
dcarc <- ContControl(target = c("X1"), 
          epsilon = c(0.01,0.03,0.05,0.1), 
          dots = list(mean = 150, sd = 1), type = "DCAR")
results <- runSimulation(dc, 
                          nrep = 3, 
                          NAControl = nc, 
                          contControl = dcarc,
                          fun = sim)
aggregate(results)

## ----B05113_10_06, message=FALSE, warning=FALSE, fig.width=7, fig.height=7----
simBwplot(results)

## ------------------------------------------------------------------------
sim2 <- function(x, orig) {
    rdcm <- function(x, y){
    	ocov <- cov(isomLR(x))
    	rcov <- cov(isomLR(y))
    	return(frobenius.norm(ocov-rcov)/frobenius.norm(ocov))
    }
     i <- apply(x, 1, function(x) any(is.na(x)))
     ni <- length(which(i))
     x <- x[, -ncol(x)]
     xMean <- e1071::impute(x)
     xMice <- mice(x, printFlag = FALSE, diagnostics = FALSE, m = 1)
     xMice <- complete(xMice)
     xKNNa <- impKNNa(x)$xImp
     xLS <- impCoda(x, method = "lm")$xImp
     xLTSrob <- impCoda(x, method = "ltsReg")$xImp
     c(xMean = rdcm(xMean, orig),
       xMice = rdcm(xMice, orig),
       knn = rdcm(xKNNa, orig), 
       LS = rdcm(xLS, orig),
       LTSrob = rdcm(xLTSrob, orig))
}

## ---- cache=TRUE, message=FALSE, warning=FALSE---------------------------
library("matrixcalc")
results <- runSimulation(dc, 
                          nrep = 3, 
                          NAControl = nc, 
                          contControl = dcarc,
                          fun = sim2)
aggregate(results)

## ------------------------------------------------------------------------
data("eusilcP")

## ------------------------------------------------------------------------
colnames(eusilcP)

## ------------------------------------------------------------------------
sim <- function(x, k) {
  require("laeken")
  x <- x[!is.na(x$eqIncome), ]
  ## classical Gini
  g <- gini(x$eqIncome, x$.weight)$value
  ## Hill estimator
  eqIncHill <- fitPareto(x$eqIncome, k = k, method = "thetaHill",
    groups = x$hid)
  gHill <- gini(eqIncHill, x$.weight)$value
  ## partial density component estimator
  eqIncPDC <- fitPareto(x$eqIncome, k = k, method = "thetaPDC",
    groups = x$hid)
  gPDC <- gini(eqIncPDC, x$.weight)$value
  ## results as a vector
  c(standard = g, Hill = gHill, PDC = gPDC) 
}

## ------------------------------------------------------------------------
sc <- SampleControl(grouping = "hid", size = 1500, k = 100)

## ---- cache = TRUE-------------------------------------------------------
library("laeken") # for function gini
set.seed(123)
## run the simulation
results <- runSimulation(eusilcP, sc, fun = sim, k = 175)

## ------------------------------------------------------------------------
head(results)

## ------------------------------------------------------------------------
aggregate(results)

## ----B05113_10_08--------------------------------------------------------
tv <- laeken::gini(eusilcP$eqIncome)$value
plot(results, true = tv)

## ---- message=FALSE,warning=FALSE----------------------------------------
set.seed(12345)
sc <- SampleControl(design = "region", grouping = "hid",
  size = c(75, 250, 250, 125, 200, 225, 125, 150, 100),
  k = 100)
## run new simulation
results <- runSimulation(eusilcP, sc, fun = sim, k = 175)

## ------------------------------------------------------------------------
head(results)

## ------------------------------------------------------------------------
aggregate(results)

## ----B05113_10_07--------------------------------------------------------
tv <- gini(eusilcP$eqIncome)$value
plot(results, true = tv)

## ------------------------------------------------------------------------
set.seed(12345)
## define contamination
cc <- DCARContControl(target = "eqIncome", epsilon = 0.005,
  grouping = "hid", dots = list(mean = 5e+05, sd = 10000))
## run new simulation
results <- runSimulation(eusilcP, sc, contControl = cc, fun = sim, k = 175)

## ------------------------------------------------------------------------
head(results)

## ------------------------------------------------------------------------
aggregate(results)

## ----B05113_10_09--------------------------------------------------------
tv <- gini(eusilcP$eqIncome)$value
plot(results, true = tv)

## ------------------------------------------------------------------------
library("simFrame")
library("laeken")
data("eusilcP")

## ------------------------------------------------------------------------
set.seed(12345)
sc <- SampleControl(design = "region", grouping = "hid",
  size = c(75, 250, 250, 125, 200, 225, 125, 150, 100), k = 100)
cc <- DCARContControl(target = "eqIncome", epsilon = 0.005,
  grouping = "hid", dots = list(mean = 5e+05, sd = 10000))
results <- runSimulation(eusilcP, sc, contControl = cc,
  design = "gender", fun = sim, k = 125)

## ----B05113_10_10--------------------------------------------------------
tv <- simSapply(eusilcP, "gender", function(x) gini(x$eqIncome)$value)
plot(results, true = tv)

## ------------------------------------------------------------------------
set.seed(12345)
sc <- SampleControl(design = "region", grouping = "hid",
  size = c(75, 250, 250, 125, 200, 225, 125, 150, 100), k = 100)
cc <- DCARContControl(target = "eqIncome", epsilon = c(0, 0.0025, 0.005, 0.0075, 0.01), dots = list(mean = 5e+05, sd = 10000))
results <- runSimulation(eusilcP, sc, contControl = cc,
  design = "gender", fun = sim, k = 125)

## ------------------------------------------------------------------------
head(results)

## ------------------------------------------------------------------------
aggregate(results)

## ----B05113_10_11--------------------------------------------------------
tv <- simSapply(eusilcP, "gender", function(x) gini(x$eqIncome)$value)
plot(results, true = tv)

## ------------------------------------------------------------------------
set.seed(12345)
sc <- SampleControl(design = "region", grouping = "hid",
  size = c(75, 250, 250, 125, 200, 225, 125, 150, 100), k = 50)
cc <- DCARContControl(target = "eqIncome", epsilon = c(0, 0.005, 0.01), dots = list(mean = 5e+05, sd = 10000))
nc <- NAControl(target = "eqIncome", NArate = c(0, 0.05))
results <- runSimulation(eusilcP, sc, contControl = cc,
  NAControl = nc, design = "gender", fun = sim, k = 125)

## ------------------------------------------------------------------------
aggregate(results)

## ----B05113_10_12--------------------------------------------------------
tv <- simSapply(eusilcP, "gender", function(x) gini(x$eqIncome)$value)
plot(results, true = tv)

## ------------------------------------------------------------------------
sessionInfo()

