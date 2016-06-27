## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)

## ------------------------------------------------------------------------
y <- matrix(c(11, 22, 16, 24, 17, NA), nrow=2)
y

## ------------------------------------------------------------------------
m <- mean(y, na.rm = TRUE)
m
y[2,3] <- m
y

## ------------------------------------------------------------------------
## stop criterion
eps <- 0.001
## intitialisations
yalt <- y
n <- 0
converged <- FALSE
## iteration
while(!converged){
	n <- n + 1
	yalt <- y
	m1 <- mean(y)
	## E-step (estimate parameters)
	a <- rowMeans(y) - m1
	b1 <- colMeans(y) - m1	
	## M-step (update y23)
	y[2, 3] <- m1 + a[2] + b1[3]		
	## stop criterion
	converged <- (abs(y[2, 3] - yalt[2, 3]) < eps)		
}
list(yImp = y, iterations = n)

## ------------------------------------------------------------------------
distMan <- function(x, centers){
  if(class(x) == "data.frame") x <- as.matrix(x)
	d <- matrix(0, nrow=nrow(x), ncol=nrow(centers))
	## dist center to observations for each cluster
	for(k in 1:nrow(centers)){
		d[,k] <- abs( colSums((t(x) - centers[k,])) )
	}
	return(d)
}

## ------------------------------------------------------------------------
means <- function(x, cluster){
	cen <- matrix(NA, nrow=max(cluster), ncol <- ncol(x))
	## cluster means for each cluster
	for(n in 1:max(cluster)){
		cen[n,] <- apply(x[cluster==n,], 2, median)
	}
	return(cen)
}

## ------------------------------------------------------------------------
my_kmeans <- function(x, k, clmeans = means, distance = distMan, iter = 99999, seed = NULL){
  if(!is.null(seed)) set.seed(seed)
	cent <- newcent <- x[sample(1:nrow(x), size=k), ]
	oldclust <- 0
	j <- 0
	for(i in 1:iter){ # better: while()
	  j <- j + 1
	  cent <- newcent
	  ## M-step
		dist <- distance(x, cent)
		clust <- max.col(-dist)
		## E-step
		newcent <- clmeans(x, clust)
		if(all(clust == oldclust)) break()
		oldclust <- clust
	}                  
	res <- list(centers = cent, 
	            cluster = factor(clust), 
	            iterations = j)
	return(res)
}

## ----B05113_09_01--------------------------------------------------------
data(Nclus, package = "flexclust")
x <- data.frame(Nclus)
library("ggplot2")
qplot(X1, X2, data=data.frame(Nclus)) + 
  xlab("") + ylab("") + theme_bw()

## ---- warning=FALSE,message=FALSE----------------------------------------
set.seed(123456)
cl1 <- kmeans(x, centers = 4, iter.max = 1, algorithm = "MacQueen")
set.seed(123456)
cl2 <- kmeans(x, centers = 4, iter.max = 2, algorithm = "MacQueen")
set.seed(123456)
cl3 <- kmeans(x, centers = 4, iter.max = 3, algorithm = "MacQueen")
set.seed(123456)
cl4 <- kmeans(x, centers = 4, algorithm = "MacQueen")

## ------------------------------------------------------------------------
cl1$centers

## ---- echo=FALSE---------------------------------------------------------
initstep <- function(x = Nclus, cluster = cl1){
   plot(x, main = "E-step (1)", cex.main=1.8)
   points(cluster$center[, 1], cluster$center[, 2], cex = 3, pch = 18) 
}
estep <- function(x = Nclus, cluster = cl2, text = "E-step (1)"){
   plot(x, col = cluster$cluster, pch = as.numeric(cluster$cluster), main=text,
       cex.main=1.8)
   points(cluster$center[, 1], cluster$center[, 2], cex = 3, pch = 18, col = 1:4)
}
mstep <- function(x = Nclus, cluster = cl2, cluster2 = cl2, text="M-step (1)"){
  plot(x, col = cluster2$cluster, pch = as.numeric(cluster2$cluster), main=text,
       cex.main=1.8)
  points(cluster2$center[, 1], cluster2$center[, 2], cex = 3, pch = 18, col = 1:4)
    segments(x0 = Nclus[cluster$cluster == 1, 1], x1 = cluster$center[1,1],
           y0 = Nclus[cluster$cluster == 1, 2], y1 = cluster$center[1,2], col = "grey")
  segments(x0 = Nclus[cluster$cluster == 2, 1], x1 = cluster$center[2,1],
           y0 = Nclus[cluster$cluster == 2, 2], y1 = cluster$center[2,2], col = "grey")
  segments(x0 = Nclus[cluster$cluster == 3, 1], x1 = cluster$center[3,1],
           y0 = Nclus[cluster$cluster == 3, 2], y1 = cluster$center[3,2], col = "grey")
  segments(x0 = Nclus[cluster$cluster == 4, 1], x1 = cluster$center[4,1],
           y0 = Nclus[cluster$cluster == 4, 2], y1 = cluster$center[4,2], col = "grey")
  points(x=Nclus[,1], y=Nclus[,2], col = cluster2$cluster, pch = as.numeric(cluster2$cluster))
  points(cluster$center[, 1], cluster$center[, 2], cex = 3, pch = 18, col = 1:4)
}

## ----B05113_09_02, fig.width=9, fig.height=9, echo=FALSE-----------------
par(mfrow = c(3,2), mar = c(1,1,3,0.1), axis=FALSE, xaxt="n", yaxt="n")
initstep()
mstep(cluster = cl1, cluster2 = cl1)
estep(cluster = cl2, text="E-step (2)")
mstep(cluster = cl2, cluster2=cl2, text="M-step (2)")
estep(cluster = cl3, text="E-step (converged)")
mstep(cluster = cl3, cluster2=cl3, text="E-step (converged)")

## ---- message=FALSE, warning=FALSE---------------------------------------
library("MASS")
library("robustbase")
library("VIM")
data("sleep")
str(sleep)

## ------------------------------------------------------------------------
apply(sleep, 2, function(x) any(is.na(x)))

## ---- message=FALSE, warning=FALSE---------------------------------------
## index of missing values
ind <- data.frame(is.na(sleep))
## initialization
sleep <- kNN(sleep)  
## overwrite missing initialization with bad choice
sleep$Sleep[ind$Sleep] <- 2240  # bad initialization
## initialized missing values in variable sleep
sleep$Sleep[ind$Sleep]

## ---- message=FALSE, warning=FALSE---------------------------------------
## E-step (1)
lm1 <- lm(Sleep ~ log(BodyWgt) + log(BrainWgt) + NonD + Danger, data = sleep)
## M-step (1)
sleep$Sleep[ind$Sleep] <- predict(lm1)[ind$Sleep]
## print of updated missing values
sleep$Sleep[ind$Sleep]

## ---- message=FALSE, warning=FALSE---------------------------------------
## E-step (2)
lm1 <- lm(Sleep ~ log(BodyWgt) + log(BrainWgt) + NonD + Danger, data = sleep)
## M-step (2)
sleep$Sleep[ind$Sleep] <- predict(lm1)[ind$Sleep]
## print of updated missing values
sleep$Sleep[ind$Sleep]

## ------------------------------------------------------------------------
EMregSleep <- function(method = lm, eps = 0.001, init = "bad"){
  ## index of missing values
  ind <- is.na(sleep)
  colnames(ind) <- colnames(sleep)
  indsleep <- ind[, "Sleep"]
  ## initialization
  if(init == "bad"){
    sleep <- kNN(sleep, imp_var = FALSE)  
    sleep$Sleep[indsleep] <- 2240  # bad initialization
  }
  if(init == "worst"){
    sleep[ind] <- 2240  # worst initialization    
  }
  iteration <- 0
  criteria <- 99999
  while(criteria > eps){
    iteration <- iteration + 1
    prev_sol <- sleep$Sleep[indsleep]
    ## E-step
    lm1 <- method(Sleep ~ log(BodyWgt) + log(BrainWgt) + NonD + Danger, 
                  data = sleep)
    ## M-step
    sleep$Sleep[indsleep] <- predict(lm1)[indsleep]
    criteria <- sqrt(sum((prev_sol - sleep$Sleep[indsleep])^2))
  }
  res <- list("imputed" = sleep, 
              "iteration" = iteration, 
              lastmodel = lm1)
  return(res)
}

## ------------------------------------------------------------------------
data("sleep")
sleepImp <- EMregSleep()
missVals <- sleepImp$imputed$Sleep[ind$Sleep]
missVals
sleepImp$iteration

## ------------------------------------------------------------------------
missVals + sample(residuals(sleepImp$lastmodel), length(missVals))

## ---- message=FALSE, warning=FALSE---------------------------------------
data("sleep")
## OLS regression
lm_ols <- EMregSleep(method = lm, init = "worst")
## M-estimation
lm_rlm <- EMregSleep(method = rlm, init = "worst", eps= 0.01)
lm_ols$imputed$Sleep[ind[, "Sleep"]]
lm_rlm$imputed$Sleep[ind[, "Sleep"]]

## ------------------------------------------------------------------------
data("sleep")
sleepImp <- irmi(sleep)
sleepImp[ind[, "Sleep"], "Sleep"]

## ------------------------------------------------------------------------
library("mice")
data("sleep")
em_mice <- mice(sleep, m = 1)
em_mice$imp$Sleep
## now with bad intitialisation in predictors
sleep[is.na(sleep)] <- 2240
sleep$Sleep[ind[, "Sleep"]] <- NA
em_mice <- mice(sleep, m = 1)
em_mice$imp$Sleep

## ------------------------------------------------------------------------
args(irmi)

## ------------------------------------------------------------------------
sessionInfo()

