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

screen <- 
  screen %>% 
  mutate(income = ifelse(is.na(income),
  mean(income, na.rm =TRUE), income)) %>% #ortalama atama
   na.omit() #liste bazında silme
summary(screen)

library(summarytools)
freq(screen$mstatus) %>%
  kable(format='markdown', 
      caption="Frekans Tablosu",digits = 2)
  


freq(screen$race) %>%
  kable(format='markdown', 
      caption="Frekans Tablosu",digits = 2)
  



library(outliers)
library(knitr)
z.scores <- screen %>%
 na.omit() %>% 
 select(2:5) %>% 
 scores(type = "z") %>%
 round(2)



summarytools::descr(z.scores,
 stats     = c("min", "max"),
 transpose = TRUE,
 headings  = FALSE) %>%   
 kable()

DT::datatable(z.scores)

library(ggplot2)
timedrs_plt <- 
ggplot(screen) +aes(x = timedrs)  + 
  geom_histogram()

timedrs_plt

library(ggplot2)
timedrs_plt <- 
timedrs_plt +
geom_density(fill = "#0c4c8a",alpha = 0.5)

timedrs_plt

library(plotly)
timedrs_plt <- 
plot_ly(x = screen$timedrs, 
type = "histogram", 
histnorm = "probability")


timedrs_plt

library(ggpmisc)
timedrs_plt <- 
ggplot(data = screen, aes(x = timedrs)) +
geom_histogram(bins = 30) 

timedrs_plt

library(ggpmisc)
timedrs_plt <- 
 timedrs_plt +
geom_vline(xintercept =7.914, 
  color = "red", 
linetype = "dashed") + 
annotate("text", 
label = "Mean = 7.913", 
x = 10, y = 100, 
color = "black")

timedrs_plt

timedrs_plt <- 
ggplot(screen, aes(y = timedrs)) + 
  geom_boxplot()  

timedrs_plt

boxplot.stats(screen$timedrs)$out
out <- boxplot.stats(screen$timedrs)$out
out
out_ind <- which(screen$timedrs %in% c(out))
out_ind


library(plotly)

timedrs_plt <- 
plot_ly(y = screen$timedrs, type = 'box') 


timedrs_plt

library(plotly)

timedrs_plt <-
timedrs_plt %>%
layout(title = 'Box Plot',
annotations = list(
x = -0.01, 
y = boxplot.stats(screen$timedrs)$out, 
text = paste(out_ind),
showarrow = FALSE,
xanchor = "right"
)
)


timedrs_plt

timedrs_plt <- 
ggplot(screen, 
aes(x = factor(mstatus), 
y = timedrs, 
fill = factor(mstatus))) 

timedrs_plt

timedrs_plt <- 
timedrs_plt+
geom_boxplot()

timedrs_plt

timedrs_plt <- 
timedrs_plt+
geom_boxplot()

timedrs_plt

timedrs_plt <- 
timedrs_plt  +
stat_summary(
aes(label = round(stat(y), 1)),
geom = "text", 
fun = function(y) 
{o <- boxplot.stats(y)$out; 
if(length(o) == 0) NA else o },
hjust = -1
)


timedrs_plt

attdrug_plt <- 
ggplot(screen) +
aes(x =  attdrug) 

attdrug_plt

attdrug_plt <-
attdrug_plt +
geom_histogram( bins = 6, fill = "#0c4c8a")+
theme_minimal()

attdrug_plt

atthouse_plt <- 
ggplot(screen) +
aes(x =  atthouse) +
geom_histogram( bins = 10, fill = "darkgreen") +
theme_minimal()

atthouse_plt

atthouse_plt <- 
ggplot(screen) +
aes(x = "", y = atthouse) +
geom_boxplot(fill = "#3357FF") +
theme_minimal()

atthouse_plt

screen[c(260,298),]
screen <- screen[-c(260,298),]



library(psych)
veri <- screen[,1:5]
md <- mahalanobis(veri, center = colMeans(veri), cov = cov(veri))
md


library(psych)
alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(veri)))
cutoff


ucdegerler <- which(md > cutoff)
veri[ucdegerler, ]

data_temiz <- veri[-ucdegerler, ]


veri[ucdegerler, ]

library(sur)
attach(screen)

skew(timedrs)
se.skew(timedrs)
skew.ratio(timedrs)

library(moments)
library(labelled)
# jarque.test(remove_labels(timedrs))

# jarque.test(remove_labels(attdrug)) 
# jarque.test(remove_labels(atthouse)) 

set.seed(0)
normal <- rnorm(200)
non_normal<-
rexp(200, rate=3)

par(mfrow=c(1,2))
hist(normal, col='steelblue', main='Normal')
hist(non_normal, col='steelblue', main='Non-normal')

par(mfrow=c(1,2))
qqnorm(normal, main='Normal')
qqline(normal)


par(mfrow=c(1,2))
qqnorm(non_normal, main='Non-normal')
qqline(non_normal)

ggplot(data = data_temiz, aes(sample = atthouse )) +
  geom_qq()+
  geom_qq_line( )

ggplot(data = data_temiz, aes(sample = atthouse )) +
  geom_qq(color = "pink") +
  geom_qq_line(color = "dark green")


qqnorm(data_temiz$atthouse ,
       col = "purple")

pairs(data_temiz[,2:5])

ltimedrs <- log(timedrs+1)
describe(timedrs)

describe(ltimedrs)

library(corrplot)
cor(screen[,2:5])


corrplot(cor(screen[,2:5]))


corrplot(cor(screen[,2:5]))

model <- lm(subno ~ timedrs  +   attdrug  +  atthouse  +income + race+ mstatus ,
data = screen)
library(olsrr)
ols_vif_tol(model)

