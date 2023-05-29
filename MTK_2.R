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
library(ggplot2)
library(plotly)
library(mirt)



library(readr)
ikikategorili <- read_csv("dichotomous.csv")[,-1]
head(ikikategorili[,1:5])

birpl_model <- "F = 1-15
                CONSTRAIN = (1-15, a1)"
birpl_uyum <- mirt(data = ikikategorili, model = birpl_model,SE=TRUE)


b <- c(1,2,-1,0)
c <- c(0,0,0,0)
a <- c(1,0.5,1.5,1.2)
theta <- seq(-4,4,0.01)

p <- matrix(ncol=4,nrow=length(theta))
prob <- c()
for(i in 1:4){
  for(j in 1:length(theta)){
    dir <- 1/(1 + exp(-(a[i]*(theta[j] - b[i]))))
    prob[j] <- dir
    j=j+1
  }
  p[,i] <-  prob
}


library(tidyverse)
veri <- data.frame(theta,p)
colnames(veri) <- c("theta","madde_1","madde_2","madde_3","madde_4")
veri_2 <- gather(veri, key = "items", value = "measurement", -theta)


library(ggplot2)

ikipl <- ggplot(veri_2, aes(theta, measurement  , colour = items)) + 
  geom_line() + 
  ggtitle('Olasılık Eğrileri') + 
  xlab("Yetenek düzeyi") + 
  ylab("Olasilik") + 
  geom_hline(aes(yintercept = 0.5)) + theme_bw() + 
  theme(text = element_text(size=16),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        legend.title=element_blank())

ggplotly(ikipl)

ggplotly(ikipl)

ikipl_model <- "F = 1 - 15"

ikipl_uyum <- mirt(data = ikikategorili, model = ikipl_model,
itemtype = "2PL", SE=TRUE)


M2(ikipl_uyum)


itemfit(ikipl_uyum)

ikipl_par <- coef(ikipl_uyum, IRTpars = TRUE, simplify = TRUE)

ikipl_par$items

plot(ikipl_uyum, type = "trace", which.items = 1:15)

plot(ikipl_uyum, type = "trace", which.items = 1:15,facet_items = FALSE,
     abline=c(h=0.5))
