library(dplyr)
library(tidyverse)
library(dplyr)
library(tuev)
library(emo)
library(knitr)


library(psych);library(haven)
veri <- read_sav("data/AFA.sav")[ ,-c(1,13)]
(out <- fa(veri, nfactors = 3,fm="pa",rotate="none"))

out$loadings

out_load <- as.data.frame(unclass(out$loadings))
library(scatterplot3d)
zz <- scatterplot3d(out_load, main="3D factor loadings", pch=20)
zz.coords <- zz$xyz.convert(out_load[,1], out_load[,2], out_load[,3]) 
text(zz.coords$x, 
     zz.coords$y,             
     labels = row.names(out_load),               
     cex = 1.5, 
     pos = 4) 



out_dik <- fa(veri,3,fm="pa",rotate="varimax")

print(out$loadings[,1:3], 
      digits = 3, cutoff = 0.30)

print(out_dik$loadings[,1:3],
      digits = 3, cutoff = 0.30)

sum(out_dik$loadings[,1]^2)

out$Vaccounted[2:3,]
out_dik$Vaccounted[2:3,]

library(scatterplot3d)
out_load_dik <- as.data.frame(unclass(out_dik$loadings))
zz <- scatterplot3d(out_load_dik, main="3D factor loadings", pch=20)
zz.coords <- zz$xyz.convert(out_load_dik[,1], out_load_dik[,2], out_load_dik[,3]) 
text(zz.coords$x, 
     zz.coords$y,             
     labels = row.names(out_load_dik),               
     cex = 1.5, 
     pos = 4) 

library(scatterplot3d)
zz <- scatterplot3d(out_load, main="3D factor loadings", pch=20)
zz.coords <- zz$xyz.convert(out_load[,1], out_load[,2], out_load[,3]) 
text(zz.coords$x, 
     zz.coords$y,             
     labels = row.names(out_load),               
     cex = 1.5, 
     pos = 4) 

print(out_dik$loadings[2:3,], digits = 3, cutoff = 0.30)



out_egik <- fa(veri,3,fm="pa",rotate="oblimin")

print(out_egik$loadings, digits = 3, cutoff = 0.30)


print(out_egik$Structure, digits = 3, cutoff = 0.30)



out_egik$Phi

print(out_dik$loadings, digits = 3, cutoff = 0.30)

print(out_egik$loadings, digits = 3, cutoff = 0.30)


library(scatterplot3d)
out_load_dik <- as.data.frame(unclass(out_dik$loadings))
zz <- scatterplot3d(out_load_dik, main="3D factor loadings", pch=20)
zz.coords <- zz$xyz.convert(out_load_dik[,1], out_load_dik[,2], out_load_dik[,3]) 
text(zz.coords$x, 
     zz.coords$y,             
     labels = row.names(out_load_dik),               
     cex = 1.5, 
     pos = 4) 


library(scatterplot3d)
out_load_egik <- as.data.frame(unclass(out_egik$loadings))
zz <- scatterplot3d(out_load_egik, main="3D factor loadings", pch=20)
zz.coords <- zz$xyz.convert(out_load_egik[,1], out_load_egik[,2], out_load_egik[,3]) 
text(zz.coords$x, 
     zz.coords$y,             
     labels = row.names(out_load_egik),               
     cex = 1.5, 
     pos = 4) 


fa_egik <- fa(veri, nfactors=3, rotate="oblimin", scores="regression")
head(fa_egik$scores)
