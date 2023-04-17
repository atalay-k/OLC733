
options(digits=3)
library(dplyr)
library(tidyverse)
library(dplyr)
library(tuev)
library(emo)
library(knitr)


# install.packages(c("lavaan","semPlot"))
library(lavaan)
library(semPlot)

# devtools::install_github("dr-JT/semoutput")
library(semoutput)


library(FCO)

## 
## cfa(model = NULL,
##     data = NULL,
##     ordered = NULL, sampling.weights = NULL,
##     sample.cov = NULL, sample.mean = NULL, sample.th = NULL,
##     sample.nobs = NULL, group = NULL, cluster = NULL,
##     constraints = "", WLS.V = NULL, NACOV = NULL, ...)

## 
## ' F1  =~ m1 + m2 +m3'
