library(dplyr)
library(tidyverse)
library(dplyr)
library(tuev)
library(emo)
library(knitr)



library(haven)
AFA <- read_sav("data/AFA.sav")
matris <- round(cor(AFA[,-c(1,13)]),2)
matris[upper.tri(matris)] <- NA
matris

library(psych)
veri <- AFA[ ,-c(1,13)]
KMO(veri)


KMO(veri)


## kor_mat <- tetrachoric(veri)$rho
## KMO(kor_mat)
## 

bartlett.test(AFA)

 fa(veri)$e.values
 sum(fa(veri)$e.values)


 fa(veri)$e.values


out <- fa(veri, nfactors = 3,fm="pa",rotate="none")
out

knitr::include_graphics("img/AFA_2.PNG")

scree(veri)

(residuals <-round(out$residual,2))

sum(abs(residuals[lower.tri(residuals)])>0.05)


library(nFactors) 
PA<-nScree( x=out$e.values, 
aparallel=NULL,
cor=TRUE, 
model="factors", 
criteria=NULL) 
PA$Components

plotnScree(PA, legend=TRUE, 
ylab="Ozdegerler", main="Faktor Cozumu")

out <- fa(veri,3,fm="pa",
rotate="none")
out$loadings[,1:3]

  out

sum(out$loadings[,1]^2)

c(sum(out$loadings[,2]^2),sum(out$loadings[,3]^2))

out$Vaccounted

factor.model(out$loadings)

rep_matrix <- factor.model(out$loadings)
diag(rep_matrix)==out$communality
