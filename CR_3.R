library(haven)
Performansd1 <- read_sav("Performansd1.sav")
summary(Performansd1)

library(fastDummies)
# Performansd1$D1 <- ifelse(Performansd1$Medeni  == "Evli", 1, 0)
# Performansd1$D2<- ifelse(Performansd1$Medeni  == "Bekar", 1, 0)
dataf <- dummy_cols(Performansd1, select_columns = 'Medeni')
summary(dataf)

model_dummy <- lm(Performans ~ 
                Medeni_1 + Medeni_2 , 
                data=dataf)
model_dummy

model_dummy

library(haven)
zorluklar <- read_sav("Hassles.sav")[,c(1,2,4,5)]
colnames(zorluklar)  <- c("id","sorun","destek","belirtiler")

GGally::ggpairs(zorluklar[,-1])


zorluklar$csorun <- zorluklar$sorun -mean(zorluklar$sorun)
zorluklar$cdestek <- zorluklar$destek -mean(zorluklar$destek)
zorluklar$cross  <- zorluklar$sorun*zorluklar$destek
zorluklar$cross_m  <- zorluklar$csorun*zorluklar$cdestek

GGally::ggpairs(zorluklar[,c(2:4,7)])

GGally::ggpairs(zorluklar[,c(5,6,4,8)])

cross_model <- lm(belirtiler  ~ csorun  + cdestek + cross_m ,data=zorluklar);broom::glance(cross_model)

broom::tidy(cross_model)

library(dplyr)
zorluklar <- zorluklar %>% mutate(
  cdestek_kat = case_when(
  cdestek <= -15 ~ "dusuk",
  cdestek >-15 &    cdestek <15~ "orta",
  cdestek >=15 ~ "yuksek",
  )
)
zorluklar <- zorluklar %>%  arrange(csorun)

dusuk <- -15
orta <- 0
yuksek <- 15


csorun <- seq(from= -200, to= 200,length.out	=60)

belirtiler_dusuk <- coefficients(cross_model)[1]+coefficients(cross_model)[2]*csorun+  coefficients(cross_model)[3]*(-15) +   coefficients(cross_model)[4]*(csorun*-15) 

belirtiler_orta<- coefficients(cross_model)[1]+coefficients(cross_model)[2]*csorun+  coefficients(cross_model)[3]*(0) +   coefficients(cross_model)[4]*(csorun*0) 

belirtiler_yuksek <- coefficients(cross_model)[1]+coefficients(cross_model)[2]*csorun+  coefficients(cross_model)[3]*(15) +   coefficients(cross_model)[4]*(csorun*15)

yeni <- rbind(
data.frame(destek="dusuk",csorun,belirtiler = belirtiler_dusuk),
data.frame(destek="orta",csorun,belirtiler = belirtiler_orta),
data.frame(destek="yuksek",csorun,belirtiler= belirtiler_yuksek)
)

library(ggplot2)
library(plotly)

p <- ggplot(data=yeni, aes(x=csorun, y=belirtiler))+
    geom_line(aes(color=destek))


ggplotly(p)

ggplotly(p)
