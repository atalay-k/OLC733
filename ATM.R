knitr::opts_chunk$set(echo = TRUE)

set.seed(26)
a <- round(rlnorm(8, meanlog=0.000, sdlog=0.200), 3)
a

b1 <- seq(from=-2.500, to=-0.750, by=0.250)

b2 <- b1+1.250
b3 <- b2+1.250
b4 <- b3+1.250
cbind(b1,b2,b3,b4)

k <- length(a)
k

set.seed(46)
birey <- rnorm(400)
n <- length(birey)
n

theta <- rep(birey, k)


aa  <- rep(a, each=n)
bb1 <- rep(b1, each=n)
bb2 <- rep(b2, each=n)
bb3 <- rep(b3, each=n)
bb4 <- rep(b4, each=n)

p1 <- 1/(1+exp(-((aa)*(theta-bb1))))
p2 <- 1/(1+exp(-((aa)*(theta-bb2))))
p3 <- 1/(1+exp(-((aa)*(theta-bb3))))
p4 <- 1/(1+exp(-((aa)*(theta-bb4))))


par <- round(cbind(p1,p2,p3,p4),2)
head(par)

rr <- runif(n*k, 0, 1)
head(par)
head(rr, 6)

puan <- 0
for (j in 1:(k*n)){
  if((rr[j]>p1[j]))puan[j] <- 0 
  else if((rr[j]<p1[j]&rr[j]>p2[j]))puan[j] <- 1 
  else if((rr[j]<p2[j]&rr[j]>p3[j]))puan[j] <- 2 
  else if((rr[j]<p3[j]&rr[j]>p4[j]))puan[j] <- 3 
  else puan[j] <- 4 
}

puan <- matrix(puan, ncol=k)
head(puan)

puan <- 0
for (j in 1:(k*n)){
  if((rr[j]>p1[j]))puan[j] <- 0 
  else if((rr[j]<p1[j]&rr[j]>p2[j]))puan[j] <- 1 
  else if((rr[j]<p2[j]&rr[j]>p3[j]))puan[j] <- 2 
  else if((rr[j]<p3[j]&rr[j]>p4[j]))puan[j] <- 3 
  else puan[j] <- 4 
}

puan <- matrix(puan, ncol=k)


puan[1:2,]
id <- matrix(1:n, ncol=1)

data <- cbind(id, puan)


maddeAT <- cbind(a,b1,b2,b3,b4)
birey <- rnorm(400)

## maddeAT <- cbind(a,b1,b2,b3,b4)
## birey <- rnorm(400)
## puanAT <- function(madde, birey){
##   a <- madde[, 1]
##   b1 <- madde[, 2]
##   b2 <- madde[, 3]
##   b3 <- madde[, 4]
##   b4 <- madde[, 5]
##   k <- length(a)
##   n <- length(birey)
##   theta <- rep(birey, k)
##   aa <- rep(a, each=n)
##   bb1 <- rep(b1, each=n)
##   bb2 <- rep(b2, each=n)
##   bb3 <- rep(b3, each=n)
##   bb4 <- rep(b4, each=n)
##   p1 <- 1/(1+exp(-((aa)*(theta-bb1))))
##   p2 <- 1/(1+exp(-((aa)*(theta-bb2))))
##   p3 <- 1/(1+exp(-((aa)*(theta-bb3))))
##   p4 <- 1/(1+exp(-((aa)*(theta-bb4))))
##   rr <- runif(n*k, 0, 1)
##   puan <- 0
##   for (j in 1:(k*n))  {
##     if((rr[j]>p1[j])) puan[j] <- 0
##     else if((rr[j]<p1[j]&rr[j]>p2[j])) puan[j] <- 1
##     else if((rr[j]<p2[j]&rr[j]>p3[j])) puan[j] <- 2
##     else if((rr[j]<p3[j]&rr[j]>p4[j])) puan[j] <- 3
##     else puan[j] <- 4
##   }
##   puan <- matrix(puan, ncol=k)
##   return(puan)
## }
## 
## id <- matrix(1:400, ncol=1)
## data <- cbind(id, puan)

library(mirt)
a <- matrix(rlnorm(20,.2,.3))
diffs <- t(apply(matrix(runif(20*4, .3, 1), 20), 1, cumsum))
diffs <- -(diffs - rowMeans(diffs))
d <- diffs + rnorm(20)
dat <- simdata(a, d, 500, itemtype = 'graded')
head(dat)
