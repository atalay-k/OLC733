
library(dplyr)
library(QuantPsyc)
library(knitr)
library(tidyverse)
library(tuev)
library(emo)




library(haven)
performans <- read_sav("Performans.sav")
psych::describe(performans)[,3:4]

cor_1 <- cor.test(~ Performans + Motivasyon , data = performans)
broom::tidy(cor_1)[,c(1,3)]

cor_2 <- cor.test(~ Performans + Kaygi , data = performans)
broom::tidy(cor_2)[,c(1,3)]


cor_3 <- cor.test(~ Motivasyon + Kaygi , data = performans)
broom::tidy(cor_3)[,c(1,3)]



library(broom)
model <- lm(Performans ~ Motivasyon + Kaygi,
            data=performans)
sqrt(glance(model)[,1])

model_s <- augment(model,data=performans)
cor(model_s[,1], model_s[,5])

model <- lm(Performans ~ Motivasyon + Kaygi,data=performans)
glance(model)[,1]

glance(model)[,2]

res <- model$residuals
sqrt(sum((res - mean(res))^2/(length(res)-3)))


glance(model)[,3]

glance(model)[,4:6]

library(knitr)
tidy(model) %>% kable()

artıkPER1 <- lm(Performans ~  Kaygi,data=performans)$residuals

artıkMOT <- lm(Motivasyon  ~  Kaygi,data=performans)$residuals

round(lm(artıkPER1  ~  artıkMOT,data=data.frame(artıkPER1,artıkMOT))$coefficients,3)

attach(performans)
((cor(Motivasyon,Performans) - cor(Kaygi ,Performans)*cor(Motivasyon,Kaygi))/
   (1-cor(Motivasyon,Kaygi)^2))*(sd(Performans)/sd(Motivasyon))


artıkPER2 <- lm(Performans ~  Motivasyon ,data=performans)$residuals


artıkKAY <- lm(Kaygi ~  Motivasyon ,data=performans)$residuals


round(lm(artıkPER2  ~  artıkKAY,data=data.frame(artıkPER2,artıkKAY))$coefficients,3)

((cor(Kaygi,Performans)- cor(Motivasyon ,Performans)*cor(Motivasyon,Kaygi))/
   (1-cor(Motivasyon,Kaygi)^2))*(sd(Performans)/sd(Kaygi))


mean(Performans)-
  (model$coefficients[2]*mean(Motivasyon))-
  (model$coefficients[3]*mean(Kaygi))

library(QuantPsyc)
lm.beta(model)

library(GGally)
ggpairs(performans[,1:3])

library(scatterplot3d)
scatterplot3d(performans[,1:3])

library(scatterplot3d)
scatterplot3d(performans[,1:3],pch = 16, color="steelblue", angle=75)

scatterplot3d(performans[,1:3],pch = 16, color="steelblue",angle=75,box = FALSE,type = "h")

library(rgl)
plot3d(Performans, Motivasyon, Kaygi,
xlab = "Performans", ylab = "Motivasyon", zlab = "Kaygi", type = "s",size = 1.5,col = "red")
rglwidget() 

p <- scatterplot3d(performans[,1:3], angle=55,type='h',
                   pch = 16, color = "steelblue")

# add a plane representing the fit of the model
p$plane3d(model, col='orangered')

# augment(model,data=performans)[,5]
model$fitted.values
model$residuals


library(outliers)
model$fitted.values %>% scores(type = "z")
model$residuals %>% scores(type = "z")

tidy(model) %>% kable()
0.686 /0.0984

# path model
library(lavaan)
library(lavaanPlot)
model_1 <- 'Performans ~  Motivasyon + Kaygi'
fit1 <- sem(model_1, data = performans)
coef(fit1)

lavaanPlot(model = fit1, coefs = TRUE, stand = TRUE, sig = 0.05) 


lavaanPlot(model = fit1, 
coefs = TRUE, stand = FALSE, sig = 0.05) 



sadece_kesisim  <- lm(Performans ~ Motivasyon, data=performans)
glance(sadece_kesisim)

tum  <- lm(Performans ~  Motivasyon + Kaygi, data=performans)
glance(tum)


asamali <- step(sadece_kesisim, direction='forward', scope=formula(tum), trace=0)
tidy(anova(tum,sadece_kesisim))

library(olsrr)
ols_plot_cooksd_bar(model)

ols_plot_dfbetas(model)

ols_plot_dffits(model)


ols_plot_resid_lev(model)


influence.measures(model, infl = influence(model))

influence.measures(model, infl = influence(model))

