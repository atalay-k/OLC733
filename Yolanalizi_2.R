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
veri <- read_table("illness.dat", col_names = FALSE)
colnames(veri) <- c("form", "stres", "hastalik", "egzersiz", "dayaniklilik")

library(lavaan)
yol_model <-  'stres     ~ egzersiz + dayaniklilik
               hastalik  ~ egzersiz + dayaniklilik + form + stres
               form      ~ egzersiz + dayaniklilik
egzersiz ~~ dayaniklilik'
yol_fit <- sem(yol_model, veri)


## library(semPlot)
## 
## semPaths(yol_fit,
##            rotation=2,
##            sizeMan = 10,
##            edge.label.cex = 1.15,
##            style = "ram")

library(semPlot)

semPaths(yol_fit,rotation=2, curvePivot = TRUE,
           sizeMan = 12, sizeInt = 1, 
            sizeLat = 4,
           edge.label.cex = 1.8,
           pastel=TRUE,
           nCharNodes = 0, nCharEdges = 0)


lavInspect(yol_fit, what = "sampstat")

lavInspect(yol_fit, what = "implied")

 lavInspect(yol_fit, what = "resid")

knitr::include_graphics("PA_1.PNG")

knitr::include_graphics("PA_2.PNG")

knitr::include_graphics("PA_3.PNG")
