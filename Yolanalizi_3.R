knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)
options(digits=5)
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


fitmeasures(yol_fit,fit.measures = c("chisq" ,"df" , "pvalue"))

  # summary(yol_fit, fit.measures = TRUE)
fitMeasures(yol_fit, c("rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue"))


fitMeasures(yol_fit, "srmr")


fitmeasures(yol_fit,fit.measures = c("cfi","tli"))

fitmeasures(yol_fit,fit.measures = c("chisq" ,"df" , "pvalue","cfi","tli",
                                     "rmsea",    "rmsea.ci.lower",   "rmsea.ci.upper"
                                     ,"srmr"))


# lavResiduals(yol_fit)
# resid(yol_fit)
resid(yol_fit, type='normalized')


modindices(yol_fit, sort = TRUE)
## modindices(yol_fit, sort = TRUE, maximum.number = 5)


library(lavaan)
yol_model <-  
'stres     ~ egzersiz + dayaniklilik
 hastalik  ~ egzersiz + dayaniklilik + form + stres
 form      ~ egzersiz + dayaniklilik
 egzersiz ~~ dayaniklilik'
yol_fit <- sem(yol_model, veri)

yol_model_v1 <- 
'stres     ~ egzersiz + dayaniklilik
 hastalik  ~ egzersiz + dayaniklilik + form + stres
 form      ~ egzersiz + dayaniklilik
 stres     ~ form
 egzersiz ~~ dayaniklilik' 
yol_fit_v1 <- sem(yol_model_v1, veri)


## library(semPlot)
## semPaths(yol_fit_v1,rotation=2, curvePivot = TRUE,
## sizeMan = 12, sizeInt = 1,
## sizeLat = 4,
## edge.label.cex = 1.8,
## pastel=TRUE,
## nCharNodes = 0, nCharEdges = 0)

library(semPlot)
semPaths(yol_fit_v1,rotation=2, curvePivot = TRUE,
sizeMan = 12, sizeInt = 1, 
sizeLat = 4,
edge.label.cex = 1.8,
pastel=TRUE,
nCharNodes = 0, nCharEdges = 0)

 lavInspect(yol_fit, what = "resid")

 lavInspect(yol_fit_v1, what = "resid")

fitmeasures(yol_fit_v1,fit.measures=c("chisq","p","df","cfi","rmsea","srmr"))


## p_pa <-
## semPaths(yol_fit_v1,
## whatLabels = "est",
## sizeMan = 10,
## edge.label.cex = 1.15,
## style = "ram",
## layout = "spring" ,
## nCharNodes = 0,
## nCharEdges = 0)
## semptools::mark_sig(p_pa,
##           yol_fit_v1)

p_pa <- 
semPaths(yol_fit_v1, whatLabels = "est",
sizeMan = 10,
edge.label.cex = 1.15,
style = "ram",layout = "spring" ,
nCharNodes = 0, nCharEdges = 0)
semptools::mark_sig(p_pa, yol_fit_v1)
p_pa_2 <- semptools::mark_sig(p_pa, yol_fit_v1)

plot(p_pa_2)


yol_model_v1 <- 
'stres     ~ egzersiz + dayaniklilik
hastalik  ~ egzersiz + dayaniklilik + form + stres
form      ~ egzersiz + dayaniklilik
stres     ~ form
egzersiz ~~ dayaniklilik' 


yol_model_v2 <- 
'stres     ~  dayaniklilik
hastalik  ~ form + stres
form      ~ egzersiz + dayaniklilik
stres     ~ form
egzersiz ~~ dayaniklilik' 
yol_fit_v2 <- sem(yol_model_v2, veri)

fitmeasures(yol_fit_v2,c("rmsea","cfi","srmr"))


p_pa <- semPaths(yol_fit_v2, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",layout = "spring" ,
           nCharNodes = 0, nCharEdges = 0)
p_pa_2 <- semptools::mark_sig(p_pa, yol_fit_v2)
plot(p_pa_2)

fitmeasures(yol_fit_v2,fit.measures = c("chisq" ,"df" , "pvalue","cfi","tli",
                                     "rmsea",    "rmsea.ci.lower",   "rmsea.ci.upper"
                                     ,"srmr"))


fitmeasures(yol_fit_v1,fit.measures = c("AIC","BIC"))

fitmeasures(yol_fit_v2,fit.measures = c("AIC","BIC"))


summary(yol_fit)


parameterEstimates(yol_fit, standardized=TRUE)

## out <- summary(yol_fit, rsquare=TRUE)
## out$PE[15:17,]

library(knitr)
parameterEstimates(yol_fit, standardized=TRUE) %>% 
  filter(op == "~") %>% 
  select('Bağımlı Değişkenler'=lhs, Gosterge=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  knitr::kable(digits = 3, booktabs=TRUE, format="markdown", caption="Faktör Yükleri")

# devtools::install_github("dr-JT/semoutput")
library(semoutput)
sem_sig(yol_fit)

library(semoutput)
sem_fitmeasures(yol_fit)

sem_paths(yol_fit)

library(semoutput)
sem_anova(yol_fit_v2,yol_fit_v1)

sem_modelcomp(yol_fit_v2,yol_fit_v1)


yol_model <-  'stres     ~ s_e*egzersiz + dayaniklilik
               hastalik  ~ h_e*egzersiz + dayaniklilik + h_f*form + h_s*stres
               form      ~ f_e*egzersiz + dayaniklilik
               egzersiz ~~ dayaniklilik
               # Doğrudan etki
               dir_fm:=h_f
               dir_sh:=h_s


               # Dolayli etki
               ind_h1:=f_e*h_f
               ind_h2:=s_e*h_s


               # Toplam dolaylı etki
               tot_ind:=ind_h1 +  ind_h2

               # Toplam etki
               tot:=tot_ind + h_e'

fsem1 <- sem(yol_model,veri)


parameterEstimates(fsem1,standardized = TRUE)[c(1,3,5,7,17:20),c(1:4,12)]


library(emo)
knitr::include_graphics("PA_8.PNG")

## xaringanBuilder::build_pdf("Yolanalizi.html")
