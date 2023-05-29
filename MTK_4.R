knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)
options(digits=3)
library(dplyr)
library(tidyverse)
library(dplyr)
library(tuev)
library(emo)
library(knitr)

library(readr)
library(mirt)
ikikategorili <- read_csv("dichotomous.csv")[,-1]

birpl_model <- "F = 1-15
                CONSTRAIN = (1-15, a1)"
birpl_uyum <- mirt(data = ikikategorili, model = birpl_model,SE=TRUE)
ikipl_model <- "F = 1 - 15"
ikipl_uyum <- mirt(data = ikikategorili, model = ikipl_model,
itemtype = "2PL", SE=TRUE)
ucpl_model <- "F = 1 - 15"
ucpl_uyum <- mirt(data = ikikategorili, model = ucpl_model,
itemtype = "3PL")


## library(readr)
## library(mirt)
## ikikategorili <- read_csv("dichotomous.csv")[,-1]
## 
## birpl_model <- "F = 1-15
##                 CONSTRAIN = (1-15, a1)"
## birpl_uyum <- mirt(data = ikikategorili, model = birpl_model,SE=TRUE)
## ikipl_model <- "F = 1 - 15"
## ikipl_uyum <- mirt(data = ikikategorili, model = ikipl_model,
## itemtype = "2PL", SE=TRUE)
## ucpl_model <- "F = 1 - 15"
## ucpl_uyum <- mirt(data = ikikategorili, model = ucpl_model,
## itemtype = "3PL")
## 

ML   <- fscores(ikipl_uyum, method="ML",full.scores.SE=TRUE)
MAP  <- fscores(ikipl_uyum, method="MAP", full.scores.SE=TRUE)
EAP  <- fscores(ikipl_uyum, method="EAP",full.scores.SE=TRUE)

ML   <- fscores(ikipl_uyum, method="ML",full.scores.SE=TRUE)
MAP  <- fscores(ikipl_uyum, method="MAP", full.scores.SE=TRUE)
EAP  <- fscores(ikipl_uyum, method="EAP",full.scores.SE=TRUE)

head(ML)

head(MAP)


head(EAP)


yetenek <- data.frame(ML= ML[,1],MAP=MAP[,1],EAP=EAP[,1])

apply(yetenek,2,summary)

yetenek_v1 <- yetenek[!is.infinite(yetenek$ML),]

apply(yetenek_v1,2,summary)

cor(yetenek_v1)

pairs(yetenek_v1)


anova(birpl_uyum,ikipl_uyum)
anova(ikipl_uyum,ucpl_uyum)


p <- 1/(1+exp(-(1-1.2)))
p * (1-p)

b <- c(1.2)
theta <- seq(-4,4,0.01)

prob <- c()
 for(j in 1:length(theta)){
  dir <- 1/(1 + exp(-(theta[j] - b)))
  prob[j] <- dir
  j=j+1
}
bilgi =  prob * (1- prob)

p <- data.frame(prob,bilgi)
MBF <- 
ggplot(p, aes(theta, bilgi)) +
geom_line()


MBF 

b <- 1.2
a <- 0.8
theta <- seq(-4,4,0.01)
p <- 1/(1+exp(-(0.8*(1-1.2))))
a^2 * p * (1-p)


prob <- c()
  for(j in 1:length(theta)){
    dir <- 1/(1 + exp(-(a*(theta[j] - b))))
    prob[j] <- dir
    j=j+1
  }
bilgi =  a*a * prob * (1- prob)

p <- data.frame(prob,bilgi)
MBF2 <- ggplot(p, aes(theta, bilgi)) + 
  geom_line()



MBF2 

plot(birpl_uyum, 
type = "infotrace", 
which.items = 5)

plot(ikipl_uyum, 
type = "infotrace", 
which.items = 5)

plot(ucpl_uyum, 
type = "infotrace", 
which.items = 5)

plot(ikipl_uyum, 
type = "infotrace", 
which.items = 1:15, layout=c(5, 3))

plot(ikipl_uyum, 
type = "infotrace", 
which.items = 2:15, layout=c(5, 3))

madde1 <- extract.item(ikipl_uyum, 1)
Theta <- matrix(seq(-6,6, by = .1))
info.1 <- iteminfo(madde1, Theta)
plot(Theta, info.1, type = 'l', main = 'Item information')

knitr::include_graphics("img/bilgi.png")

tinfo <- 
testinfo(ikipl_uyum, 
Theta,
which.items = 1:5)
plot(Theta, 
     tinfo, 
     type = 'l')

tinfo <- 
testinfo(ikipl_uyum, 
Theta,
which.items = 1:10)
plot(Theta, 
     tinfo, 
     type = 'l')

tinfo <- 
testinfo(ikipl_uyum, 
Theta,
which.items = 1:15)
plot(Theta, 
     tinfo, 
     type = 'l')

tinfo <- testinfo(ikipl_uyum, Theta,which.items = c(1,3:5,7:10,11))
plot(Theta, tinfo, type = 'l')

plot(ikipl_uyum, type='infoSE')

# xaringanBuilder::build_pdf("mirt.Rmd")
 
