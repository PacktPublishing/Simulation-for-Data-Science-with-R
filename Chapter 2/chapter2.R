## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(ggplot2)

## ------------------------------------------------------------------------
5 + 2 * log(3 * 3)

## ------------------------------------------------------------------------
mean(rnorm(10))

## ---- eval=FALSE---------------------------------------------------------
## res1 <- name_of_function(v1) # an input argument
## res2 <- name_of_function(v1, v2) # two input arguments
## res3 <- name_of_function(v1, v2, v3) # three input arguments

## ------------------------------------------------------------------------
x <- rnorm(5)
x

## ------------------------------------------------------------------------
options(digits = 4)
x

## ---- eval=FALSE---------------------------------------------------------
## install.packages("dplyr")

## ---- eval=FALSE---------------------------------------------------------
## library("dplyr")

## ---- eval=FALSE---------------------------------------------------------
## install.packages("devtools")
## library("devtools")
## install_github("hadley/dplyr")

## ---- eval=FALSE---------------------------------------------------------
## help.start()

## ---- eval=FALSE---------------------------------------------------------
## help(package="dplyr")

## ---- eval=FALSE---------------------------------------------------------
## ?group_by

## ---- eval=FALSE---------------------------------------------------------
## data(Cars93, package = "MASS")

## ---- eval=FALSE---------------------------------------------------------
## help.search("histogram")

## ---- eval=FALSE, echo=TRUE----------------------------------------------
## apropos("hist")

## ---- eval=FALSE---------------------------------------------------------
## RSiteSearch("group by factor")

## ------------------------------------------------------------------------
ls()

## ------------------------------------------------------------------------
getwd()

## ------------------------------------------------------------------------
v.num <- c(1,3,5.9,7)
v.num
is.numeric (v.num)

## ------------------------------------------------------------------------
v.num > 3

## ---- warning=FALSE, message=FALSE---------------------------------------
v1 <- c(1,2,3)
v2 <- c(4,5)
v1 + v2 

## ------------------------------------------------------------------------
v2 <- c (100, TRUE, "A", FALSE)
v2
is.numeric (v2)

## ---- eval=TRUE----------------------------------------------------------
data(Cars93, package = "MASS")
# extract a subset of variable Horsepower from Cars93
hp <- Cars93[1:10, "Horsepower"] 
hp
# positive indexing:
hp[c(1,6)]
# negative indexing:
hp[-c(2:5,7:10)]
# logical indexing:
hp < 150
# a logical expression can be written directly in []
hp[hp < 150]

## ------------------------------------------------------------------------
class(Cars93)
class(Cars93$Cylinders)
levels(Cars93$Cylinders)
summary(Cars93$Cylinders)

## ------------------------------------------------------------------------
model <- lm(Price ~ Cylinders + Type + EngineSize + Origin, data = Cars93)
## result is a list
class(model)
## access elements from the named list with the dollar sign
model$coefficients

## ---- tidy=FALSE---------------------------------------------------------
## extract cars with small number of cylinders and small power
w <- Cars93$Cylinders %in% c("3", "4")  & Cars93$Horsepower < 80
str(Cars93[w, ])

## ------------------------------------------------------------------------
library("vcd")
data(PreSex)
PreSex

## ------------------------------------------------------------------------
PreSex[, , 1, 2]

## ------------------------------------------------------------------------
PreSex[, , "Yes", "Men"]

## ------------------------------------------------------------------------
sum(is.na(Cars93))

## ----B05113_02_01, message = FALSE, warning=FALSE------------------------
require("VIM")
# colnames(Cars93) <- substr(colnames(Cars93), 1, 8)
par(mar=c(7,3,0.1,0.1))
matrixplot(Cars93, sortby = "Weight", cex.axis=0.9)

## ---- eval=FALSE, message=FALSE, warning=FALSE---------------------------
## m <- robCompositions::missPatterns(Cars93)

## ------------------------------------------------------------------------
## how often summary is overloaded with methods 
## on summary for certain classes
length(methods(summary))
class(Cars93$Cylinders)
summary(Cars93$Cylinders)
## just to see the difference, convert to class character:
summary(as.character(Cars93$Cylinders))

## ------------------------------------------------------------------------
## function to be applied afterwards
func <- function(x){
  return(sum(is.na(x)))
}
## apply func on all columns (second dimension (2))
## and store it to na
na <- apply(X = Cars93, MARGIN = 2, FUN = func)
## show those with missings
na[ na > 0 ]

## ------------------------------------------------------------------------
p <- ncol(Cars93)
na_for <- numeric(p)
for(i in 1:p){
  na_for[i] <- func(Cars93[, i])
}

identical(as.numeric(na), na_for)

## ------------------------------------------------------------------------
m <- robCompositions::missPatterns(Cars93)
class(m)

## ------------------------------------------------------------------------
lapply(m, length)

## ---- tidy=TRUE----------------------------------------------------------
s <- sapply(m, length)

## ------------------------------------------------------------------------
s
class(s)

## ------------------------------------------------------------------------
args(aggregate)

## ------------------------------------------------------------------------
methods(aggregate)

args(aggregate.data.frame)

## ------------------------------------------------------------------------
aggregate(Cars93[, c("Horsepower", "Weight")], by = list(Cars93$Cylinders), median)

## ---- eval = TRUE, echo = TRUE, message=FALSE, warning=FALSE-------------
library("dplyr") 

## ---- eval = TRUE, echo = TRUE-------------------------------------------
class (Cars93)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
Cars93 <- tbl_df(Cars93)
class(Cars93) 

## ---- eval = FALSE, echo = TRUE, results='hide'--------------------------
## print(Cars93) # output suppressed

## ---- eval = TRUE, echo = TRUE, results='hide'---------------------------
slice(Cars93, 1) # first line, output suppressed

## ---- eval = TRUE, echo = TRUE, tidy=FALSE-------------------------------
slice (Cars93, c(1,4,10,15, n())) 

## ---- eval = TRUE, echo = TRUE, tidy=FALSE-------------------------------
filter(Cars93, Manufacturer == "Audi" & Min.Price > 25)

## ---- eval = TRUE, echo = TRUE, results='hide'---------------------------
Cars93 <- arrange (Cars93, Price)
Cars93 ## output suppressed

## ---- eval = TRUE, echo = TRUE, tidy=FALSE-------------------------------
head(arrange(Cars93, desc (MPG.city), Max.Price), 7)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
head (select (Cars93, Manufacturer, Price), 3)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
head(select(Cars93, Manufacturer:Price), 3)

## ---- eval = TRUE, echo = TRUE, results='hide'---------------------------
select(Cars93, -Min.Price, -Max.Price) # output suppressed

## ---- eval = TRUE, echo = TRUE-------------------------------------------
head(select(Cars93, starts_with ("Man")), 3)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
head(select(Cars93, contains ("Price")), 3)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
head (select (Cars93, myPrize = Price, Min.Price))

## ---- eval = TRUE, echo = TRUE-------------------------------------------
Cars93_1 <- select(Cars93, Manufacturer, EngineSize)
dim (Cars93_1)
Cars93_1 <- distinct(Cars93_1)
dim (Cars93_1)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
dim(Cars93)
dim( distinct (Cars93, Manufacturer) )
# based on two variables, second is rounded:
dim(distinct(Cars93, Manufacturer, EngineSize))
# based on two variables, second is rounded:
dim(distinct(Cars93, Manufacturer, rr=round(EngineSize)))

## ---- eval = TRUE, echo = TRUE-------------------------------------------
m <- mutate(Cars93, is_ford = Manufacturer == "Ford")
m[1:3, c(1,28)]

## ---- eval = TRUE, echo = TRUE, results='hide'---------------------------
transmute(Cars93, is_ford = Manufacturer == "Ford", Manufacturer)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
head (transmute(Cars93, Manufacturer, is_ford = Manufacturer == "Ford", num_ford = ifelse (is_ford, -1, 1)), 3)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
by_type <- group_by (Cars93, Type)
summarize (by_type,
 count = n(),
 min_es = min(EngineSize),
 max_es = max(EngineSize)
)

## ---- results='hide'-----------------------------------------------------
Cars93 %>% 
  group_by(Type) %>% 
  summarize(count = n(), min_es = min(EngineSize), max_es = max(EngineSize) )
## output suppressed since equally to previous output

## ---- eval = TRUE, echo = TRUE, tidy=FALSE-------------------------------
by_type <- group_by(Cars93, Type)
slice(by_type, 1: 2)

## ---- echo = TRUE, results='hide'----------------------------------------
## output suppressed since the same as above
Cars93 %>% group_by(Type) %>% slice(1:2)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
Cars93 %>% 
  mutate(ES2 = EngineSize^2) %>% 
  group_by(Type) %>%
  summarize(min.ES2 = min(ES2)) %>% 
  arrange(desc(min.ES2))

## ---- eval = TRUE, echo = TRUE, tidy=FALSE-------------------------------
Cars93 %>% 
  group_by(Type) %>% 
  arrange(Type) %>% 
  select(Manufacturer:Price) %>% 
  mutate(cmean = cummean(Price), csum = cumsum(Price))

## ----eval=TRUE, echo=TRUE, results='hide'--------------------------------
require(data.table)
Cars93 <- data.table(Cars93)
Cars93 ## print output suppressed

## ----eval=TRUE, echo=TRUE------------------------------------------------
tables()

## ----eval=TRUE, echo=TRUE------------------------------------------------
Cars93$tmp1 <- Cars93[, j = Manufacturer == "Ford"] 

## ----eval=TRUE, echo=TRUE------------------------------------------------
Cars93[, tmp2 := rnorm(nrow(Cars93))] 

## ----eval=TRUE, echo=TRUE------------------------------------------------
Cars93[, tmp1:=NULL]
Cars93$tmp2 <- NULL

## ----eval=TRUE, echo=TRUE, results='hide'--------------------------------
Cars93[i = 2] # second row, all columns 
Cars93[i = c(1,5)] # first and fifth row, all columns
Cars93[i = -c(1:5)] # exclude the first five rows

## ----eval=TRUE, echo=TRUE, tidy=FALSE------------------------------------
Cars93[j = 3] # this does not work since 3 evaluates to 3
Cars93[j = "Price"] # extract "Price" does not work since "Price" evaluates to "Price"
Cars93[j = Price] # this works, since variable Price exists in the scope of Cars93 
Cars93[i=1:3, j = "Price", with = FALSE] # also works

## ----eval=TRUE, echo=TRUE------------------------------------------------
Cars93[1:3, .(Price, Horsepower, Diff.Price = Max.Price - Min.Price, Mean.Price = mean(Price))]  

## ----eval=TRUE, echo=TRUE------------------------------------------------
setkey(Cars93, Type) # equally: setkeyv(dt, "x")

## ----eval=TRUE, echo=TRUE------------------------------------------------
key(Cars93)

## ----eval=TRUE, echo=TRUE, results='hide'--------------------------------
setkey(Cars93, Type) 
Cars93["Van"] # all rows with Type == "Van" (output suppressed)

## ----eval=TRUE, echo=TRUE------------------------------------------------
setkey(Cars93, Type, DriveTrain, Origin)
Cars93[.("Van", "4WD", "non-USA")]

## ----eval=TRUE, echo=TRUE, cache=TRUE------------------------------------
require(microbenchmark)
N <- 1000000
dat<- data.table(
  x=sample(LETTERS[1:20], N, replace=TRUE),
  y=sample(letters[1:5], N, replace=TRUE))
head(dat, 3)

setkey(dat, x,y)

microbenchmark(
  data.table = dat[list(c("B", "D"), c("b", "d"))],
  dplyr = dat %>% slice(x %in% c("B", "D") & y %in% c("b", "d")),
  baseR = dat[x %in% c("B", "D") & y %in% c("b", "d")]
)

## ----eval=TRUE, echo=TRUE, cache=TRUE------------------------------------
Cars93[, .(mean = mean(Price), IQR = IQR(Price), median = median(Price)), by = Type]

## ------------------------------------------------------------------------
data(Cars93, package = "MASS")
set.seed(123)
system.time(lm(Price ~ Horsepower + Weight + Type + Origin, data=Cars93))

## ------------------------------------------------------------------------
library("robustbase")
system.time(lmrob(Price ~ Horsepower + Weight + Type + Origin, data=Cars93))

## ---- tidy=FALSE---------------------------------------------------------
ptm <- proc.time()
robustbase::lmrob(Price ~ Horsepower + Weight + Type + Origin, data=Cars93)
proc.time() - ptm	

## ---- cache=TRUE---------------------------------------------------------
require("robustbase")
s1 <- system.time(replicate(100, lm(Price ~ Horsepower + Weight + Type + Origin, data=Cars93)))[3]
s2 <- system.time(replicate(100, lmrob(Price ~ Horsepower + Weight + Type + Origin, data=Cars93)))[3]
(s2 - s1)/s1

## ------------------------------------------------------------------------
Rprof("Prestige.lm.out")
invisible(replicate(100,
				lm(Price ~ Horsepower + Weight + Type + Origin, data=Cars93)))
Rprof(NULL)
summaryRprof("Prestige.lm.out")$by.self

## ---- results='hide', warning=FALSE, message=FALSE-----------------------
require(profr)
parse_rprof("Prestige.lm.out") 

## ---- warning=FALSE, message=FALSE---------------------------------------
library(microbenchmark); library(plyr); library(dplyr); library(data.table); library(Hmisc)

## ---- tidy = FALSE-------------------------------------------------------
data(Cars93, package = "MASS")
Cars93 %>% group_by(Type, Origin) %>% summarise(mean = mean(Horsepower))

## ------------------------------------------------------------------------
meanFor <- function(x){
  sum <- 0
  for(i in 1:length(x)) sum <- sum + x[i]
  sum / length(x)
}

## groupwise statistics
myfun1 <- function(x, gr1, gr2, num){
  x[,gr1] <- as.factor(x[,gr1])
  x[,gr2] <- as.factor(x[,gr2])
  l1 <- length(levels(x[,gr1]))
  l2 <- length(levels(x[,gr1]))
  gr <-  numeric(l1*l2)
  c1 <- c2 <- character(l1*l2)
  ii <- jj <- 0
  for(i in levels(x[,gr1])){
    for(j in levels(x[,gr2])){
      ii <- ii + 1
      c1[ii] <- i
      c2[ii] <- j
      vec <- x[x[,gr2] == j & x[,gr1] == i, num]
      if(length(vec) > 0) gr[ii] <- meanFor(vec)
    }
  }
  
  df <- data.frame(cbind(c1, c2))
  df <- cbind(df, gr)
  colnames(df) <- c(gr1,gr2,paste("mean(", num, ")"))
  df
}

## groupwise using mean()
## attention mean.default is faster
myfun2 <- function(x, gr1, gr2, num){
  x[,gr1] <- as.factor(x[,gr1])
  x[,gr2] <- as.factor(x[,gr2])
  l1 <- length(levels(x[,gr1]))
  l2 <- length(levels(x[,gr1]))
  gr <-  numeric(l1*l2)
  c1 <- c2 <- character(l1*l2)
  ii <- jj <- 0
  for(i in levels(x[,gr1])){
    for(j in levels(x[,gr2])){
      ii <- ii + 1
      c1[ii] <- i
      c2[ii] <- j
      gr[ii] <- mean(x[x[,gr2] == j & x[,gr1] == i, num])
    }
  }
  
  df <- data.frame(cbind(c1, c2))
  df <- cbind(df, gr)
  colnames(df) <- c(gr1,gr2,paste("mean(", num, ")"))
  df
}

## ------------------------------------------------------------------------
Cars93dt <- data.table(Cars93)

## ---- tidy=FALSE---------------------------------------------------------
op <- microbenchmark(
  ## pure for loops
  MYFUN1 = myfun1(x=Cars93, gr1="Type", gr2="Origin", 
                  num="Horsepower"),
  ## pure for loops but using mean
  MYFUN2 = myfun2(x=Cars93, gr1="Type", gr2="Origin", 
                  num="Horsepower"),
  ## plyr
  PLYR = ddply(Cars93, .(Type, Origin), summarise, 
             output = mean(Horsepower)),
  ## base R's aggregate and by
  AGGR = aggregate(Horsepower ~ Type + Origin, Cars93, mean),
  BY = by(Cars93$Horsepower, 
          list(Cars93$Type,Cars93$Origin), mean),
  ## Hmisc's summarize
  SUMMARIZE = summarize(Cars93$Horsepower, 
                  llist(Cars93$Type,Cars93$Origin), mean),
  ## base R's tapply
  TAPPLY = tapply(Cars93$Horsepower, 
              interaction(Cars93$Type, Cars93$Origin), mean),
  ## dplyr
  DPLYR = summarise(group_by(Cars93, Type, Origin), 
                    mean(Horsepower)),
  ## data.table
  DATATABLE = Cars93dt[, aggGroup1.2 := mean(Horsepower), 
                       by = list(Type, Origin)], 
  times=1000L)

## ----B05113_02_02--------------------------------------------------------
m <- reshape2::melt(op, id="expr")
ggplot(m, aes(x=expr, y=value)) + 
  geom_boxplot() + 
  coord_trans(y = "log10") + 
  xlab(NULL) + ylab("computation time") +  
  theme(axis.text.x = element_text(angle=45))

## ---- cache = TRUE, warning=FALSE, message=FALSE-------------------------
library(laeken); data(eusilc)
eusilc <- do.call(rbind, 
    list(eusilc,eusilc,eusilc,eusilc,eusilc,eusilc,eusilc))
eusilc <- do.call(rbind,
    list(eusilc,eusilc,eusilc,eusilc,eusilc,eusilc,eusilc))
dim(eusilc)
eusilcdt <- data.table(eusilc)
setkeyv(eusilcdt, c('hsize','db040'))

op <- microbenchmark(
  MYFUN1 = myfun1(x=eusilc, gr1="hsize", gr2="db040", 
                  num="eqIncome"),
  MYFUN2 = myfun2(x=eusilc, gr1="hsize", gr2="db040", 
                  num="eqIncome"),
  PLYR = ddply(eusilc, .(hsize, db040), summarise, 
               output = mean(eqIncome)),
  AGGR = aggregate(eqIncome ~ hsize + db040, eusilc, mean),
  BY = by(eusilc$eqIncome, list(eusilc$hsize,eusilc$db040), mean),
  SUMMARIZE = summarize(eusilc$eqIncome, 
                        llist(eusilc$hsize,eusilc$db040), mean),
  TAPPLY = tapply(eusilc$eqIncome, 
              interaction(eusilc$hsize, eusilc$db040), mean),
  DPLYR = summarise(group_by(eusilc, hsize, db040), 
                    mean(eqIncome)),
  DATATABLE = eusilcdt[, mean(eqIncome), by = .(hsize, db040)],
  times=10)

## ----B05113_02_03--------------------------------------------------------
m <- reshape2::melt(op, id="expr")
ggplot(m, aes(x=expr, y=value)) + 
  geom_boxplot() + 
  coord_trans(y = "log10") + 
  xlab(NULL) + ylab("computation time") +  
  theme(axis.text.x = element_text(angle=45, vjust=1))

## ------------------------------------------------------------------------
Rprof("aggr.out")
a <- aggregate(eqIncome ~ hsize + db040, eusilc, mean)
Rprof(NULL)
summaryRprof("aggr.out")$by.self

## ---- cache = TRUE, message=FALSE, warning=FALSE-------------------------
R <- 10000
library(robustbase)
covMcd(Cars93[, c("Price", "Horsepower")], cor = TRUE)$cor[1,2]

## confidence interval:
n <- nrow(Cars93)
f <- function(R, ...){
  replicate(R, covMcd(Cars93[sample(1:n, replace=TRUE), 
      c("Price", "Horsepower")], cor = TRUE)$cor[1,2])
}
system.time(ci <- f(R))
quantile(ci, c(0.025, 0.975))

## ------------------------------------------------------------------------
library("snow")
cl <- makeCluster(3, type="SOCK")

## ---- results='hide'-----------------------------------------------------
clusterEvalQ(cl, library("robustbase"))
clusterEvalQ(cl, data(Cars93, package = "MASS"))
clusterExport(cl, "f")
clusterExport(cl, "n")

## ------------------------------------------------------------------------
clusterSetupRNG(cl, seed=123)

## ------------------------------------------------------------------------
system.time(ci_boot <-
				clusterCall(cl, f, R = round(R / 3)))
quantile(unlist(ci_boot), c(0.025, 0.975))

## ------------------------------------------------------------------------
stopCluster(cl)

## ------------------------------------------------------------------------
wmeanR <- function(x, w) {
  total <- 0
  total_w <- 0
  for (i in seq_along(x)) {
    total <- total + x[i] * w[i]
    total_w <- total_w + w[i]
  }
  total / total_w
}

## ------------------------------------------------------------------------
library("Rcpp")
## from 
## http://blog.revolutionanalytics.com/2013/07/deepen-your-r-experience-with-rcpp.html 
cppFunction('
  double wmean(NumericVector x, NumericVector w) {
  int n = x.size();
  double total = 0, total_w = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i] * w[i];
    total_w += w[i];
  }
  return total / total_w;
  }
')

## ---- cache = TRUE-------------------------------------------------------
x <- rnorm(100000000)
w <- rnorm(100000000)
library("laeken")
op <- microbenchmark(
  naiveR = wmeanR(x, w),
  weighted.mean = weighted.mean(x, w),
  weighedMean = weightedMean(x, w),
  Rcpp.wmean = wmean(x, w), 
  times = 1
)
op

## ----B05113_02_04--------------------------------------------------------
m <- reshape2::melt(op, id="expr")
ggplot(m, aes(x=expr, y=value)) + 
  geom_boxplot() + 
  coord_trans(y = "log10") + 
  xlab(NULL) + ylab("computation time") + 
  theme(axis.text.x = element_text(angle=45, vjust=1))

## ---- eval = FALSE-------------------------------------------------------
## pdf(file = "myplot.pdf")
## plot(Horsepower ~ Weight, data = Cars93)
## dev.off ()

## ----addfig, fig.align='center'------------------------------------------
x <- 1:20 / 2 # x ... 0.5, 1.0, 1.5, ..., 10.0
y <- sin(x) 
plot(x, y, pch = 16, cex = 10 * abs(y), col = grey(x / 14))

## ----B05113_02_05, fig.align='center'------------------------------------
plot(x, y, pch = 16, cex = 10 * abs(y), col = grey(x / 14)) 
text(x, y, 1:20, col="yellow")
curve(sin, -2 * pi, 4 * pi, add = TRUE, col = "red") 
abline(h = 0, lty = 2, col = "grey")

## ----B05113_02_06, fig.align='center'------------------------------------
par(mfrow = c(2, 2), mar = c(3,3,0.1,0.1))  
mpg <- mtcars$mpg
cyl <- factor(mtcars$cyl)
df <- data.frame(x1=cyl, x2=mpg)
tmpg <- ts(mpg)
plot(mpg); plot(cyl); plot(df); plot(tmpg)

## ---- echo = TRUE, eval = TRUE, size='tiny'------------------------------
tail(methods(plot)) ## last 6
## number of methods for plot
length(methods(plot)) 

## ---- eval = FALSE, fig.align='center'-----------------------------------
## plot(x=mtcars$mpg, y=mtcars$hp)
## plot(mtcars$mpg, mtcars$hp)
## plot(hp ~ mpg, data=mtcars)

## ------------------------------------------------------------------------
## min und max in both axis
xmin <- min(mtcars$mpg); xmax <- max(mtcars$mpg)
ymin <- min(mtcars$hp); ymax <- max(mtcars$hp)

## calculate histograms
xhist <- hist(mtcars$mpg, breaks=15, plot=FALSE)
yhist <- hist(mtcars$hp, breaks=15, plot=FALSE)
 
## maximum count
top <- max(c(xhist$counts, yhist$counts))
xrange <-  c(xmin,xmax) 
yrange <- c(ymin, ymax)

## ----B05113_02_07, fig.align="center"------------------------------------
m <- matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE)
## define plot order and size
layout(m, c(3,1), c(1, 3), TRUE)
## first plot
par(mar=c(3,0,1,1))
plot(mtcars[,c("mpg","hp")], xlim=xrange, ylim=yrange, xlab="", ylab="") 
## second plot -- barchart of margin
par(mar=c(0,0,1,1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
## third plot -- barchart of other margin
par(mar=c(3,0,1,1)) 
barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)

## ----B05113_02_08, fig.align="center", fig.height=4.5, fig.width=6-------
library("ggplot2")
ggplot(Cars93, aes(x = Horsepower, y = MPG.city)) + geom_point(aes(colour = Cylinders))

## ----B05113_02_09, fig.align="center", fig.height=4.5, fig.width=6-------
g1 <- ggplot(Cars93, aes(x=Horsepower, y=MPG.city))
g2 <- g1 + geom_point(aes(color=Weight)) 
g2 + geom_smooth()  

## ----B05113_02_10, fig.align="center", fig.height=4.5, fig.width=6-------
g1 <- g1 + geom_text(aes(label=substr(Manufacturer,1,3)), 
                     size=3.5)  
g1 + geom_smooth()

## ----image B05113_02_11--------------------------------------------------
gg <- ggplot(Cars93, aes(x=Horsepower, y=MPG.city))
gg <- gg + geom_point(aes(shape = Origin, colour = Price)) 
gg <- gg + facet_wrap(~ Cylinders)  + theme_bw()
gg

## ------------------------------------------------------------------------
sessionInfo()

