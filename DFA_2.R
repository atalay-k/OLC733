knitr::opts_chunk$set(echo = TRUE)

library(openxlsx)
yasamdoyum <- read.xlsx("yasamdoyum.xlsx")
head(yasamdoyum)

model_1 <- 
"
okul =~ okul1 + okul2 + okul3
kisi =~ kisi1 + kisi2
arkadas =~ arkadas1 + arkadas2
aile =~ aile1 + aile2 + aile3
"

library(lavaan)
model_1_fit <- cfa(model_1, data = yasamdoyum)
summary(model_1_fit, fit.measures = TRUE, standardized = TRUE)

# devtools::install_github("dr-JT/semoutput")
library(semoutput)
sem_sig(model_1_fit)

library(semoutput)
sem_fitmeasures(model_1_fit)

fitmeasures(model_1_fit,fit.measures = c("chisq" ,"df" , "pvalue","cfi","tli","rmsea","rmsea.ci.lower",   
"rmsea.ci.upper","srmr"))

library(FCO)
fits.esnek <- gen_fit(mod1 = model_1, x = yasamdoyum[,4:13], rep = 100)
flex_co(fits = fits.esnek, index = c("CFI", "SRMR"))$cutoff


recommend(fits.esnek)$cutoffs

sem_factorloadings(model_1_fit,standardized = FALSE)

sem_factorloadings(model_1_fit,standardized = TRUE)


varyanslar <- 
summary(model_1_fit, 
rsquare=TRUE)
# varyanslar <- varyanslar$pe
# varyanslar[,5:8] <- 
# round(varyanslar[,5:8],2)

library(DT)

#datatable(varyanslar)

sem_factorvar(model_1_fit)

(1.431^2 * 0.327) / var(yasamdoyum$okul2)


resid(model_1_fit, type = "normalized")


var(yasamdoyum$okul2)

pars <- parameterEstimates(model_1_fit,standardized = TRUE)
pars[12,]

library(semPlot)
semPaths(model_1_fit, what="par",
rotation = 2 )

semPaths(model_1_fit, what="std",
rotation = 2 )

sem_factorcor(model_1_fit)

model_1_v1 <- 
"okul =~ NA*okul1 + okul2 + okul3
kisi =~ NA*kisi1 + kisi2
arkadas =~ NA*arkadas1 + arkadas2
aile =~ NA*aile1 + aile2 + aile3
 kisi ~~ 1*kisi
 arkadas ~~ 1*arkadas
 aile ~~ 1*aile"
model_1_v1_fit <- cfa(model_1_v1,yasamdoyum)
parameterestimates(model_1_v1_fit,standardized = TRUE)[11:14,]

modindices(model_1_fit, sort=TRUE, standardized=FALSE)

model_2f <- "
okul =~ okul1 + okul2 + okul3
kisi_arkadas =~ kisi1 + kisi2 + arkadas1 + arkadas2
aile =~ aile1 + aile2 + aile3
"
model_2_fit <- cfa(model_2f, data = yasamdoyum)
sem_sig(model_2_fit)


sem_fitmeasures(model_2_fit)


fits.esnek2 <- gen_fit(mod1 = model_2f , x = yasamdoyum[,4:13], rep = 100)

flex_co(fits = fits.esnek2, index = c("CFI", "SRMR"))$cutoff

sem_factorloadings(model_2_fit)

anova(model_1_fit,model_2_fit )


model_2order <-  "
okul =~ okul1 + okul2 + okul3
kisi =~ kisi1 + kisi2
arkadas =~ arkadas1 + arkadas2
aile =~ aile1 + aile2 + aile3

# ikinci düzey model
doyum =~ okul + kisi + arkadas + aile
"

fit_model_2order <- cfa(model_2order, yasamdoyum)
summary(fit_model_2order, fit.measures = TRUE, standardized = TRUE)

sem_fitmeasures(fit_model_2order)

sem_factorloadings(fit_model_2order,standardized = TRUE)

anova(fit_model_2order, model_2_fit, model_1_fit )

semPaths(fit_model_2order, "std", weighted = FALSE, nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)
