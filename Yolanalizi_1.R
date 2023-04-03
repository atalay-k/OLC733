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


yol_model <-  'stres     ~ egzersiz + dayaniklilik
               hastalik  ~ egzersiz + dayaniklilik + form + stres
               form      ~ egzersiz + dayaniklilik'

library(readr)
veri <- read_table("illness.dat", col_names = FALSE)
colnames(veri) <- c("form", "stres", "hastalik", "egzersiz", "dayaniklilik")

library(lavaan)
yol_model <-  'stres     ~ egzersiz + dayaniklilik
               hastalik  ~ egzersiz + dayaniklilik + form + stres
               form      ~ egzersiz + dayaniklilik
               egzersiz ~~ dayaniklilik'

yol_fit <- sem(yol_model, veri)


library(semPlot)
semPaths(yol_fit,rotation=2, curvePivot = TRUE,
           sizeMan = 12, sizeInt = 1, 
            sizeLat = 4,
           edge.label.cex = 1.8,
           pastel=TRUE,
           nCharNodes = 0, nCharEdges = 0)


library(tidySEM)
graph_sem(layout = matrix("x"),rect_width = 2,rect_height=1.5) + coord_fixed()

library(tidySEM)
graph_sem(nodes = data.frame(name = "x", shape = "oval"), layout = matrix("x"), fix_coord = TRUE)

library(emo)
knitr::include_graphics("semboller.PNG")

library(emo)
knitr::include_graphics("edges.PNG")

knitr::include_graphics("PA_5.PNG")


library(semPlot)
semPaths(yol_fit,rotation=2, curvePivot = TRUE,
           sizeMan = 12, sizeInt = 1, 
            sizeLat = 4,
           edge.label.cex = 1.8,
           pastel=TRUE,
           nCharNodes = 0, nCharEdges = 0)


library(semPlot)
semPaths(yol_fit,rotation=2, curvePivot = TRUE,
           sizeMan = 12, sizeInt = 1, 
            sizeLat = 4,
           edge.label.cex = 1.8,
           pastel=TRUE,
           nCharNodes = 0, nCharEdges = 0)


library(semPlot)
semPaths(yol_fit,rotation=2, curvePivot = TRUE,
           sizeMan = 12, sizeInt = 1, 
            sizeLat = 4,
           edge.label.cex = 1.8,
           pastel=TRUE,
           nCharNodes = 0, nCharEdges = 0)


library(semPlot)
semPaths(yol_fit,rotation=2, curvePivot = TRUE,
           sizeMan = 12, sizeInt = 1, 
            sizeLat = 4,
           edge.label.cex = 1.8,
           pastel=TRUE,
           nCharNodes = 0, nCharEdges = 0)

