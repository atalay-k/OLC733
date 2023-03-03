knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)
options(digits=3)
library(dplyr)
library(QuantPsyc)
library(knitr)
library(tidyverse)
library(tuev)
library(emo)


lise_not <- c(18,35,53,24,64,58,32,39,64,82,32,49,48,70,57)
uni_not  <- c(33,46,47,21,73,55,74,32,56,68,43,46,68,84,61)
veri <- data.frame(lise_not, uni_not)



veri

ggplot2::ggplot(veri, 
aes(x = lise_not, y = uni_not)) + 
geom_point() +   
geom_smooth(method = "lm", se = F)

basitreg <- lm(uni_not ~ lise_not , veri)
summary(basitreg)

knitr::include_graphics("LS.png")

n <- length(lise_not)
byx = (n*sum(lise_not*uni_not)-sum(lise_not)*sum(uni_not))/
  (n*sum(lise_not^2) - sum(lise_not)^2);byx

(sd(uni_not)/sd(lise_not))*cor(lise_not,uni_not)


attach(veri)
ayx = (sum(uni_not) - byx*sum(lise_not))/15
ayx



sqrt((sum(uni_not^2)-ayx*sum(uni_not)-
       byx*(sum(uni_not*lise_not)))/13)

res <- basitreg$residuals
sd(res)
sqrt(sum((res - mean(res)) ^ 2 / (length(res)-2)))


basitreg <- lm(uni_not ~ lise_not , veri)
library(broom)
glance(basitreg)

basitreg <- lm(uni_not ~ lise_not , veri)
library(broom)
glance(basitreg) %>% kable()

glance(basitreg)[,c(1,2,4,6,5)]

tidy(basitreg)
