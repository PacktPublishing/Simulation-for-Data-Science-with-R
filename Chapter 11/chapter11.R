## ------------------------------------------------------------------------
fert2Rate <- function(age, time, lastbirth){ 
 a <- ifelse(time <= 2020, 32, 33) 
 b <- ifelse(time <= 2020, 6.0, 5.7) 
 frate <- (b / a) * (a  / age) ^ (3 / 2) * exp(-b ^ 2 * (a / age + age / a - 2)) 
 frate[age <= 15 | age >= 45 | lastbirth < 0.75] <- 0 
 return(frate) 
} 

## ------------------------------------------------------------------------
fert2Rate(30, 2030, lastbirth = 3)

## ------------------------------------------------------------------------
fert2Rate(40, 2030, lastbirth = 3)

## ------------------------------------------------------------------------
mortRate <- function(age, time){ 
  a <- 0.0003 
  b <- ifelse(time <= 2020, 0.1, 0.097) 
  mrate <- a * exp(b * age) 
  return(mrate) 
} 

## ------------------------------------------------------------------------
mortRate(40, 2056)

## ---- message=FALSE, warning=FALSE---------------------------------------
library("simFrame")
data(eusilcP, package = "simFrame")
pop <- eusilcP[, c("age", "gender", "hsize", "hid")]

## ------------------------------------------------------------------------
pop$nchildWomen <- ifelse(pop$gender == "female" & as.integer(pop$hsize) > 2 & pop$age > 17, as.integer(pop$hsize) - 2, 0)
pop$partnered <- factor(ifelse(as.integer(pop$hsize) >= 2 & pop$age > 17, "P", "A"))

## ------------------------------------------------------------------------
head(pop)

## ------------------------------------------------------------------------
stateSpace <- expand.grid(sex = levels(pop$gender), 
                          partnered =  levels(pop$partnered)) 

## ------------------------------------------------------------------------
stateSpace

## ------------------------------------------------------------------------
trMatrix_f <- cbind(c("female/A->female/P", "female/P->female/A"),
                    c("rates1", "rates2")) 
trMatrix_m <- cbind(c("male/A-male/P", "male/P-male/A"), 
                    c("rates3", "rates4")) 
allTransitions <- rbind(trMatrix_f, trMatrix_m) 
absTransitions <- rbind(c("female/dead", "mortRate"), 
                        c("male/dead", "mortRate")) 

## ---- message=FALSE, warning=FALSE---------------------------------------
library("MicSim")
transitionMatrix <- buildTransitionMatrix(allTransitions = allTransitions, absTransitions = absTransitions, stateSpace = stateSpace) 

## ------------------------------------------------------------------------
transitionMatrix[1,3] <- "rates3"
transitionMatrix[3,1] <- "rates4"
transitionMatrix

## ------------------------------------------------------------------------
maxAge <- 100 

## ------------------------------------------------------------------------
love <- function(t, x, parms){
  with(as.list(c(parms, x)), {
    dPrince_Harry <- a * Chelsy_Davy
    dChelsy_Davy <- -b * Prince_Harry
    res <- c(dPrince_Harry, dChelsy_Davy)
    list(res)
  })
}

## ------------------------------------------------------------------------
parms  <- c(a = 1, b = 2)
times  <- seq(0, 30, length = 31)
## Start values for steady state
y <- xstart <- c(Prince_Harry = 1, Chelsy_Davy = -1)

## ---- message=FALSE, warning=FALSE---------------------------------------
library("deSolve")
out <-  ode(xstart, times, love, parms = parms) 

## ----B05113_11_01--------------------------------------------------------
matplot(out)

## ----B05113_11_02--------------------------------------------------------
y <- xstart <- c(Prince_Harry = 0.2, Chelsy_Davy = 1)
parms  <- c(a = 0.3, b = 0.7)
out <-  ode(xstart, times, love, parms = parms) 
matplot(out)

## ------------------------------------------------------------------------
lv_mod <- function (time, state, parms) {
    with(as.list(c(state, parms)), {
        dx <- k1 * x - k2 * x * y
        dy <- -k3 * y + k4 * x * y
        return(list(c(dx, dy)))
    })
}

## ------------------------------------------------------------------------
parms <- c(k1 = 1, k2 = 1.5, k3 = .2, k4 = .6)
state <- c(x = 10, y = 10)
time <- seq(0, 200, by = 1)

## ------------------------------------------------------------------------
res <- ode(func = lv_mod, y = state, parms = parms, times = time)
res <- as.data.frame(res)

## ----B05113_11_03--------------------------------------------------------
par(mar = c(4,4,0.5,0.1))
matplot(res[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Toads", "Snakes"), lty = c(1,2), col = c(1,2), box.lwd = 0)

## ------------------------------------------------------------------------
sessionInfo()

