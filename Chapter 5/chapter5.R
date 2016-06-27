## ----rosenbroeck---------------------------------------------------------
mountains <- function(v) { 
  (1 - v[1])^2 + 100 * (v[2] - v[1]*v[1])^2 +
   0.3*(0.2 - 2*v[2])^2 + 100 * (v[1] - v[2]*v[2])^2 - 
   0.5*(v[1]^2 +5*v[2]^2) 
}

## ---- eval=FALSE---------------------------------------------------------
## library("animation")
## grad.desc()

## ---- eval=FALSE---------------------------------------------------------
## ani.options(nmax = 70)
## par(mar = c(4, 4, 2, 0.1))
## f2 = function(x, y) sin(1/2 * x^2 - 1/4 * y^2 + 3) * cos(2 * x + 1 -
##   exp(y))
## grad.desc(f2, c(-2, -2, 2, 2), c(-1, 0.5), gamma = 0.3, tol = 1e-04)

## ----B05113_05_02--------------------------------------------------------
n <- 300
## to define a grid
x <- seq(-1, 2, length.out = n)
y <- seq(-1, 2, length.out = n)
## evaluate on each grid point
z <- mountains(expand.grid(x, y))
## contour plot
par(mar = c(4,4,0.5,0.5))
contour(x, y,  matrix(log10(z), length(x)), 
        xlab = "x", ylab = "y", nlevels = 20)
## starting value
sta <- c(0.5,-1)
points(sta[1], sta[2], cex = 2, pch = 20)
## solutions for each of 20 steps
sol <- matrix(, ncol=2, nrow = 21)
sol[1, ] <- sta
for(i in 2:20){
	sol[i, ] <- nlm(mountains, sta, iterlim = i)$est
}
## optimal solution
sol[21, ] <- nlm(mountains, sta)$est
points(sol[21, 1], sol[21, 2], cex = 3, col = "red", pch = 20)
## path visually
lines(sol, pch=3, type="o")
## now let's start better (dashed line)
sta <- c(0,-1)
for(i in 2:20){
	sol[i, ] <- nlm(mountains, sta, iterlim = i)$est
}
sol[1, ] <- sta
sol[21, ] <- nlm(mountains, sta)$est
points(sta[1], sta[2], cex = 2, pch = 20)
points(sol[21, 1], sol[21, 2], cex = 3, col = "red", pch = 20)
lines(sol, pch=3, type="o")

## ----B05113_05_03, cache=FALSE-------------------------------------------
## wrapper for all methods of optim
optims <- function(x, meth = "Nelder-Mead", start = c(0.5, -1)){
  sol <- matrix(, ncol = 2, nrow = 21)
  sol[1, ] <- start
  for(i in 2:20){
    sol[i, ] <- optim(start, mountains, method = meth, 
                      control = list(maxit=i))$par
  }
  sol[21,] <- optim(start, mountains)$par
  points(start[1], start[2], pch=20, cex = 2)
  points(sol[21, ], sol[21, ], pch = 20, col = "red", cex = 3) 
  lines(sol[, 1], sol[, 2], type = "o", pch = 3)
}
## plot lines for all methods
par(mar=c(4,4,0.5,0.5))
contour(x, y,  matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
optims()  # Nelder-Mead 
optims("BFGS")
optims("CG")
optims("L-BFGS-B")
optims("SANN")
optims("Brent")
optims(start = c(1.5,0.5))

## ----B05113_05_04, cache=TRUE--------------------------------------------
## define grid
n <- 1500
set.seed(1234567)
x1 <- runif(n, min = -2, max = 5)
y1 <- runif(n, min = -2, max = 5) 
z1 <- matrix(, ncol = n, nrow = n)
## evaluate on each grid point
for(i in 1:n){
  for(j in 1:n){
	  z1[i,j] <- mountains(c(x1[i], y1[j]))
  }
}
## determine optima
w <- which(z1 == min(z1), arr.ind=TRUE)
## plot results
par(mar=c(4,4,0.5,0.5))
contour(x, y,  matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
points(x1[w[1]], y1[w[2]], pch = 20, col = "red", cex = 3)
points(x1, y1, pch=3)

## ----B05113_05_05, cache=TRUE--------------------------------------------
library("RCEIM")
set.seed(123)
sol <- best <- list() 
## save solution for each step
for(i in 2:20){
  a <- ceimOpt(mountains, nParam = 2, maxIter = i)
  sol[[i]] <- a$EliteMembers 
  best[[i]] <- a$BestMember
}
## plot the results for each step
par(mar=c(4,4,0.5,0.5))
contour(x, y,  matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
greys <- grey(rev(2:20 / 20 - 0.099))
for(i in 2:20){
  points(sol[[i]][,1], sol[[i]][,2], col = greys[i])
  points(best[[i]][1], best[[i]][2], col = "red", pch = 3)  
}
points(best[[i]][1], best[[i]][2], col = "red", pch = 20, cex = 3)

## ----randomwalk----------------------------------------------------------
## Simple random walk Metropolis Hastings:
rmh <- function(n = 20, start = c(0,-0.5), stepmult = 10){
  x <- matrix(, ncol = 2, nrow = n)
  x[1, ] <- start 
  sol <- mountains(start)
  for(i in 2:n){
    x[i, ] <- x[i-1, ] + rmvnorm(1, mean = c(0, 0), 
                            sigma = stepmult * diag(2) / n)
    solnew <- mountains(x[i, ])
    # accept only a better solution:
    if(solnew > sol) x[i, ] <- x[i-1, ]
    if(solnew < sol) sol <- solnew
  }
  return(x)
}

## ----walk----------------------------------------------------------------
library("mvtnorm")
set.seed(12345)
n <- 200
x1 <- rmh(n, start = c(1.5,0))
x2 <- rmh(n, start = c(1.5,0))

## ----B05113_05_06--------------------------------------------------------
par(mar=c(4,4,0.5,0.5))
contour(x, y,  matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
points(x1[1, 1], x1[1, 2], pch = 4, cex = 3)
points(x2[n, 1], x2[n, 2], pch = 20, col = "red", cex = 3)
points(x1[n, 1], x1[n, 2], pch = 20, col = "red", cex = 3)
lines(x1[, 1], x1[, 2], type = "o", pch = 3)
lines(x2[, 1], x2[, 2], type = "o", col = "blue", lty = 2)

## ----B05113_05_07code----------------------------------------------------
stoGrad <- function(start = c(0, -0.5), j = 1500, p = 0.1){
  theta <- matrix(start, ncol=2)
  diff <- iter <- 1
  alpha <- sapply(1:100, function(x) 1 / (j+1) )
  beta  <- sapply(1:100, function(x) 1 / (j+1)^(p) )
  
  while( diff > 10^-5 & !is.nan(diff) & !is.na(diff) ){
    zeta <- rnorm(2)
    zeta <- zeta / sqrt(t(zeta) %*% zeta)
    grad <- alpha[iter] * zeta * (mountains(theta[iter, ] + beta[iter] * zeta) - 
             mountains(theta[iter, ] - beta[iter] * zeta)) / beta[iter]
    theta <- rbind(theta, theta[iter, ] - grad)
    diff <- sqrt(t(grad) %*% grad )
    iter <- iter + 1
  } 
  list(theta = theta[1:(iter-1), ], diff = diff, iter = iter-1)
}

## ----B05113_05_07--------------------------------------------------------
set.seed(123)
s1 <- stoGrad()
par(mar=c(4,4,0.5,0.5))
contour(x, y,  matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
plotLine <- function(x, ...){
  lines(x$theta[,1], x$theta[,2], type = "o", ...)
  points(x$theta[x$iter, 1], x$theta[x$iter, 1], pch = 20, col = "red", cex = 3)
}
plotLine(s1, pch = 3)
points(0, -0.5, pch = 20, cex = 1.5)  
plotLine(stoGrad(), col = "blue", pch = 4)
plotLine(stoGrad(start = c(1.5, 0)), pch = 3, lty = 2)
plotLine(stoGrad(start = c(1.5, 0)), col = "blue", pch = 4, lty = 2)
points(1.5, 0, pch = 20, cex = 1.5)  

## ----B05113_05_08--------------------------------------------------------
set.seed(123)
s1 <- stoGrad(p = 2.5)
par(mar=c(4,4,0.5,0.5))
contour(x, y,  matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
plotLine <- function(x, ...){
  lines(x$theta[,1], x$theta[,2], type = "o", ...)
  points(x$theta[x$iter, 1], x$theta[x$iter, 1], pch = 20, col = "red", cex = 3)
}
plotLine(s1, pch = 3)
points(0, -0.5, pch = 20, cex = 1.5)  
plotLine(stoGrad(p = 2.5), col = "blue", pch = 4)
plotLine(stoGrad(start = c(1.5, 0), j=1500, p=2.5), pch = 3, lty = 2)
plotLine(stoGrad(start = c(1.5, 0), j=1500, p=2.5), col = "blue", pch = 4, lty = 2)
points(1.5, 0, pch = 20, cex = 1.5)  

## ----B05113_05_09, warning=FALSE, message=FALSE--------------------------
library("nloptr")
set.seed(123)
## mountains function with modified function parameters
mountains1 <- 
function(x) ((1 - x[1])^2 + 100 * (x[2] - x[1]*x[1])^2 +
   0.3*(0.2 - 2*x[2])^2 + 100 * (x[1] - x[2]*x[2])^2 - 
   0.5*(x[1]^2 +5*x[2]^2))
x0 <- c(0.5, -1)
lb <- c(-3, -3)
ub <- c(3, 3)
sol <- matrix(, ncol=2,nrow=21)
## solution on each step
for(i in 1:20){
  sol[i, ] <- isres(x0 = x0, fn = mountains1, lower = lb, upper = ub, maxeval = i)$par
}
par(mar=c(4,4,0.5,0.5))
contour(x, y,  matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
## start
points(sol[1, 1], sol[1, 2], pch = 20, cex = 2)
## optima found
sol[21,] <- isres(x0 = x0, fn = mountains1, lower = lb, upper = ub)$par
points(sol[21, 1], sol[21, 2], pch = 20, col = "red", cex = 3)
## way to optima
lines(sol[,1], sol[,2], type = "o", pch = 3)

## ------------------------------------------------------------------------
sessionInfo()

