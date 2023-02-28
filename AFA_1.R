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



df <- data.frame(matrix(c(
  1,6,5,4,8,6,2,
  2,8,7,2,7,5,3,
  3,9,8,1,9,7,1,
  4,5,4,5,9,7,1,
  5,4,3,6,9,7,1,
  6,7,6,3,7,5,3,
  7,3,2,7,7,5,3),nrow=7,byrow = TRUE))
colnames(df) <- c("id", "handsome", "beatiful","ugly","brillant","smart","dumb")

df %>% kable(align = "c")

cor(df[,-1])%>% kable(align = "c")

library(psych)
fa1 <- round(fa(df[,-1],2)$loading[,1:2],2)
cbind(fa1,fa1^2)%>% kable(align = "c",
col.names = c("MR1","MR2", "MR1*MR1","MR2*MR2"))

fa(df[,-1],2,n.obs=7) %>% target.rot()

 rbind(fa1*fa1,
 toplam= colSums(fa1*fa1)) %>%
  kable()
