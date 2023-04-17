
library(openxlsx)
yasamdoyum <- read.xlsx("yasamdoyum.xlsx")

model_1_v1 <- 
"okul =~ NA*okul1 + okul2 + okul3
kisi =~ NA*kisi1 + kisi2
arkadas =~ NA*arkadas1 + arkadas2
aile =~ NA*aile1 + aile2 + aile3
 kisi ~~ 1*kisi
 arkadas ~~ 1*arkadas
 aile ~~ 1*aile"

library(lavaan)
configural <- cfa(model_1_v1, data=yasamdoyum, group = "cinsiyet")

fit <- c("chisq", "df", "pvalue","rmsea", "srmr","cfi")

# bicimsel degimezlik
fitmeasures(configural, fit.measures = fit)


weak <- cfa(model_1_v1, data=yasamdoyum, group = "cinsiyet",
group.equal=c("loadings"))

# bicimsel degimezlik
fitmeasures(configural, fit.measures = fit)

# zayıf degimezlik
fitmeasures(weak, fit.measures = fit)

library(semoutput)
sem_modelcomp(weak, configural)


strong <-  cfa(model_1_v1, data=yasamdoyum,
               group = "cinsiyet", 
               group.equal=c("loadings","intercepts"))

# zayıf degimezlik
fitmeasures(weak,fit.measures = fit)

# guclu degimezlik
fitmeasures(strong,fit.measures = fit)



sem_modelcomp(strong, weak)


library(semTools)
measurementInvariance(model = model_1_v1, 
                      data = yasamdoyum, 
                      group = "cinsiyet")


## xaringanBuilder::build_pdf("DFA_3.html")
