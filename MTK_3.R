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


ucpl_par <- coef(ucpl_uyum, IRTpars = TRUE, simplify = TRUE)
ucpl_par

M2(ucpl_uyum)


itemfit(ucpl_uyum)

ucpl_par$items

plot(ucpl_uyum, type = "trace", which.items = 1:15)

plot(ucpl_uyum, type = "trace", which.items = 1:15,facet_items = FALSE,
     abline=c(h=0.5))

b <- c(1,1,1,-1.5,-0.5,0.5)
c <- c(0,0,0.25,0,0.1,0.1,0.15)
a <- c(1.8,0.8,1.8,1.8,1.2,0.4)
theta <- seq(-4,4,0.01)

p <- matrix(ncol=6,nrow=length(theta))
prob <- c()
for(i in 1:6){
  for(j in 1:length(theta)){
    dir <- c[i] + ((1-c[i])/(1 + exp(-(a[i]*(theta[j] - b[i])))))
    prob[j] <- dir
    j=j+1
  }
  p[,i] <-  prob
}


library(tidyverse)
veri <- data.frame(theta,p)
colnames(veri) <- c("theta","madde_1","madde_2","madde_3","madde_4","madde_5","madde_6")
veri_2 <- gather(veri, key = "items", value = "measurement", -theta)
colnames(veri_2)<- c("theta",   "items", "olasilik")

library(ggplot2)

ucpl <- ggplot(veri_2, aes(theta, olasilik  , colour = items)) + 
  geom_line() + 
  ggtitle('Olasılık Eğrileri') + 
  xlab(expression(theta)) + 
  ylab(expression(P(theta))) + 
  geom_hline(aes(yintercept = 0.5)) + theme_bw() + 
  theme(text = element_text(size=16),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        legend.title=element_blank())

ucpl 

library(plotly) 

ucpl <- ggplot(veri_2, aes(theta, olasilik  , colour = items)) + 
  geom_line() + 
  ggtitle('Olasılık Eğrileri') 

ggplotly(ucpl) 
  

ggplotly(ucpl) 


ggplotly(ucpl) 


ggplotly(ucpl) 


ggplotly(ucpl) 


ucpl_q0 <- ucpl + geom_vline(xintercept = 0)

ggplotly(ucpl_q0) 

ggplotly(ucpl) 

ucpl_q1 <- ucpl + geom_vline(xintercept = -1)

ggplotly(ucpl_q1) 
  

ucpl_q3 <- ucpl + geom_vline(xintercept = 3)

 ggplotly(ucpl_q3) 
  
