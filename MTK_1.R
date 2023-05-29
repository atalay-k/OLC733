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


library(mirt)
library(ggplot2)
library(psych)
library(readr)


# b <- c(1,2,-1,0)
# c <- c(0,0,0,0)
# a <- c(0,0,0,0)
# theta <- seq(-4,4,0.01)
# 
# p <- matrix(ncol=4,nrow=length(theta))
# prob <- c()
# for(i in 1:4){
#   for(j in 1:length(theta)){
#     dir <- 1/(1 + exp(-(theta[j] - b[i])))
#     prob[j] <- dir
#     j=j+1
#   }
#   p[,i] <-  prob
# }
# 
# 
# library(tidyverse)
# veri <- data.frame(theta,p)
# colnames(veri) <- c("theta","madde_1","madde_2","madde_3","madde_4")
# veri_2 <- as.data.frame(gather(veri, key = "items", value = "measurement", -theta))
# 
# write.csv(veri_2,row.names = FALSE, "veri_2.csv")

veri_2 <- read_csv("veri_2.csv")
library(ggplot2)
library(plotly)

birpl <- ggplot(veri_2, aes(theta, measurement  , colour = items)) + 
  geom_line() +
  ggtitle('Olasılık Eğrileri') + 
  xlab(expression(theta)) + 
  ylab(expression(P(theta))) + 
  geom_hline(aes(yintercept = 0.5)) + theme_bw() + 
  theme(text = element_text(size=16),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        legend.title=element_blank())
birpl


birpl <- ggplot(veri_2, aes(theta, measurement  , colour = items)) + 
  geom_line() +
  ggtitle('Olasılık Eğrileri') + 
  xlab("theta") + 
  ylab("Olasılık") + 
  geom_hline(aes(yintercept = 0.5)) + theme_bw() + 
  theme(text = element_text(size=16),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        legend.title=element_blank())
ggplotly(birpl)

# install.packages("mirt")
library("mirt")

birpl_model <- "F = 1-15  
                CONSTRAIN = (1-15, a1)"   # <2>

birpl_model_v1 <- "F = 1-15
                CONSTRAIN = (1-10, a1)" # <1>

library(readr)
ikikategorili <- read_csv("dichotomous.csv")[,-1]
head(ikikategorili[,1:5])


summary(ikikategorili)

veri <- read_csv("veri.csv")
library(mirt)
dat1 <- key2binary(veri[,-1],
    key = c(2,3,4,5,2,3,4,5,2,3,4,5))

head(veri[,1:5])

head(dat1[,1:5])

itemstats(ikikategorili)

## birpl_model <- "F = 1-15
##                 CONSTRAIN = (1-15, a1)"
## birpl_uyum <- mirt(data = ikikategorili, model = birpl_model,SE=TRUE)

library(psych)
summary(omega(ikikategorili, plot = F))

birpl_uyum <- mirt(data = ikikategorili, model = birpl_model,SE=TRUE)

 Q3 <- residuals(birpl_uyum, type = 'Q3', method = 'ML')


Q3[lower.tri(Q3,diag = TRUE)] <- NA
sum(abs(Q3) >0.2,na.rm=TRUE)

 M2(birpl_uyum)

itemfit(birpl_uyum)


birpl_model <- "F = 1-15
                CONSTRAIN = (1-15, a1)"
birpl_uyum <- mirt(data = ikikategorili, model = birpl_model)

birpl_par <- coef(birpl_uyum, 
                  IRTpars = TRUE, 
                  simplify = TRUE)
birpl_par$items

birpl_par <- coef(birpl_uyum, 
IRTpars = TRUE, simplify = TRUE)
birpl_par$items

birpl_par <- coef(birpl_uyum, 
                  IRTpars = TRUE, 
                  simplify = TRUE)
birpl_par$items

plot(birpl_uyum,type = "trace", which.items = 1:15)


plot(birpl_uyum, type = "trace", which.items = 1:15,
     layout=c(5, 3),theta_lim = c(-4, 4))

## plot(birpl_uyum,
##      type = "trace",
## which.items = 1:15,
## layout=c(5, 3),
## panel=function(x, y){
## panel.grid(h=-1, v=-1)
## panel.xyplot(x, y)
## panel.abline(h=0.5, lwd=1,
##         lty=1)})

plot(birpl_uyum,type = "trace", 
     which.items = 1:15, 
     layout=c(5, 3),
     panel=function(x, y) {
     panel.grid(h=-1, v=-1)
     panel.xyplot(x, y)
     panel.abline(h=0.5, lwd=1, lty=1)})

plot(birpl_uyum, type = "trace", which.items = 1:15,
facet_items = FALSE)
