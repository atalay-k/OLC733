knitr::opts_chunk$set(cache=TRUE)
knitr::opts_chunk$set(fig.path='figs/')
knitr::opts_chunk$set(cache.path='cache/')

knitr::opts_chunk$set(
  fig.process = function(x) {
    x2 = sub('-\\d+([.][a-z]+)$', '\\1', x)
    if (file.rename(x, x2)) x2 else x
  }
)
library(tidyverse)
library(stevemisc)
library(knitr)


library(haven)
screen <- read_sav("SCREEN.sav")
head(screen)

library(naniar)
any_na(screen)

n_miss(screen)

prop_miss(screen)

screen %>% is.na() %>% colSums()

miss_var_summary(screen)

miss_var_table(screen)

miss_case_summary(screen)

miss_case_table(screen)


gg_miss_var(screen)


vis_miss(screen) + theme(axis.text.x = element_text(angle=80))
gg_miss_upset(screen)

## 
## # değişkeni kopyala
## screen2 <- screen
## screen2$income_m <- screen2$income
## 
## library(finalfit)
## explanatory=c("timedrs", "attdrug", "atthouse")
## dependent="income_m"
## screen2 %>%
##   missing_compare(dependent, explanatory) %>%
##     knitr::kable(row.names=FALSE,
##         align = c("l", "l", "r", "r", "r"),
##         caption = "Eksik veriye sahip olan
##         ve olmayan değişkenlerin
##         ortalama karşılaştırması")


# değişkeni kopyala
screen2 <- screen
screen2$income_m <- screen2$income

library(finalfit)
explanatory = c("timedrs","attdrug","atthouse")
dependent = "income_m"
screen2 %>% 
  missing_compare(dependent, explanatory) %>% 
    knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r"), 
        caption = "Eksik veriye sahip olan ve olmayan 
        değişkenlerin ortalama karşılaştırması") 

screen2 <- screen %>% dplyr::mutate(mstatus =   case_when(mstatus ==1 ~ "not married",   mstatus ==2  ~ "married")) %>%dplyr::mutate_if(is.character,as.factor)
screen2 %>%   ff_glimpse()

miss_test <- screen2 %>%
  mutate(miss_income = is.na(income))
  
# evli olmayanlar için
notmarried <- miss_test %>%  filter(mstatus == "not married") %>% pull(miss_income)
  
# Evliler için
married <- miss_test %>%   filter(mstatus == "married") %>% pull(miss_income)
  
# Oran
t.test(notmarried, married)

screen <- screen %>%   mutate(mstatus = as.factor(mstatus))
gg_miss_fct(screen, fct = mstatus)


library(naniar)
mcar_test(data=screen[,2:5])

na.omit(screen) 

screen[complete.cases(screen),]

df = data.frame(x = 1:20, y = c(1:10,rep(NA,10)))
df$y[is.na(df$y)] = mean(df$y, na.rm=TRUE)

screen3 <- screen
screen3$income[is.na(screen3$income)]<- mean(screen3$income, na.rm=TRUE)
summary(screen3$income)

screen4 <- screen[,2:5]
for(i in 1:ncol(screen4)) {
screen4[ , i][is.na(screen4[ , i])] <- mean(screen4[ , i], na.rm = TRUE)
}


# df = transform(df, y = ifelse(is.na(y), mean(y, na.rm=TRUE), y))
screen5 <- screen

screen5 = transform(screen5, income = ifelse(is.na(income), mean(income, na.rm=TRUE), income))
summary(screen5)
   

screen %>%  
mutate(income = ifelse(is.na(income), mean(income, na.rm =TRUE), income))


library(mvdalab)
dat <- introNAs(iris, percent = 25)
imputeEM(dat)



library(missMethods)
dat2 <- delete_MCAR(iris[,2:4], p=0.2)
dat2<-impute_EM(dat2,stochastic = FALSE)
