library(tidyverse)
library(stevemisc)
library(knitr)


library(haven)
screen <- read_sav("SCREEN.sav")
head(screen)

summary(screen)

library(psych)
describe(screen[,-1])

## library(gtsummary)
## screen %>% select(2:6) %>%
## tbl_summary(
## statistic = all_continuous() ~ c("{min}, {max}"),
## missing = "always")

library(gtsummary)
screen %>% select(2:6) %>%
tbl_summary(
statistic = all_continuous() ~ c("{min}, {max}"),
missing = "always")

library(vtable)
sumtable(screen, summ=c('notNA(x)','min(x)','max(x)'))

st(screen, summ = c('notNA(x)','min(x)','max(x)'),
 summ.names = c('Frekans','Minimum','Maximum'))

ozet <- describe(screen[,-1])
kable(ozet,format='markdown',caption="Betimsel İstatistikler",digits=2)
