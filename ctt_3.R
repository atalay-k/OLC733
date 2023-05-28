
  library("rmarkdown")
  library("fontawesome")
  library("kableExtra")
  library("emo")
  
  # Required packages
  library("dplyr")
  library("car")
  library("skimr")
  library("DataExplorer")
  library("ggcorrplot")
  library("psych")
  library("CTT")
  library("ShinyItemAnalysis")
  library("QME")
  library("difR")
  library("rmarkdown")


# automatically create a bib database for R packages
# knitr::write_bib(x = c(.packages()), file = "packages.bib")


## ----ctt4, eval=FALSE------------------------------------------------------
## library("dplyr") # veri düzenleme
## library("car")   # veri düzenleme
## library("skimr")  # veri inceleme
## library("DataExplorer") # veri inceleme
## library("ggcorrplot")   # veri inceleme
## library("psych")  #KTK analizler
## library("CTT")   #KTK analizler
## library("ShinyItemAnalysis") #KTK analizler
## # devtools::install_github("zief0002/QME")
## library("QME") #KTK analizler
## library("difR") #KTK analizler


## ----ctt42, R.options = list(width = 110)----------------------------------
hci <- read.csv("hci.csv", header = TRUE)
head(hci)
str(hci)


## ----ctt43, echo=TRUE, fig.width=9, fig.height=8---------------------------
DataExplorer::plot_bar(data = hci, nrow = 6, ncol = 4)


## ----ctt44, echo=TRUE------------------------------------------------------
DataExplorer::plot_histogram(data = hci[, c("study_year")])


## ----ctt45b----------------------------------------------------------------
key <- read.csv("hci_key.csv", header = TRUE)


## ----ctt46, echo=TRUE, R.options = list(width = 110)-----------------------
hci_items <- dplyr::select(hci, 
                           starts_with("item")) 
head(hci_items)


## ----ctt47, echo=TRUE, R.options = list(width = 110)-----------------------
CTT::distractorAnalysis(items = hci_items, key = key)


## ----ctt48, echo=TRUE, eval=TRUE-------------------------------------------
key2 <- read.csv("hci_key2.csv" , header = TRUE)
print(key2)
hci_analysis <- QME::analyze(test = hci_items, key = key2, id = FALSE)
QME::distractor_report(x = hci_analysis)


## ----ctt49, echo=TRUE, eval=FALSE, background = "gray85"-------------------
## 
## |Choice | Key| Proportions| Response Discrimination|
## |:------|---:|-----------:|-----------------------:|
## |A      |   0|        0.04|                   -0.23|
## |B      |   0|        0.09|                   -0.09|
## |C      |   0|        0.17|                   -0.29|
## |D      |   1|        0.70|                    0.29|


## ----ctt49b, echo=TRUE, eval=TRUE------------------------------------------
key3 <- as.vector(key$key)

ShinyItemAnalysis::plotDistractorAnalysis(Data = hci_items,       key = key3,
      num.groups = 3, 
      item = 1) 


## ----ctt50, echo=TRUE------------------------------------------------------
hci_scored <- CTT::score(items = hci_items,
     key = key3, 
     output.scored = TRUE, 
    rel = TRUE) 
str(hci_scored)


## ----ctt51, echo=TRUE------------------------------------------------------
scores <- hci_scored$score
summary(scores)
hist(x = scores, 
     xlab = "Toplam Puan", 
     main = "Toplam Puan Dağılımı", 
     col = "lightblue", 
     breaks = 15, 
     xlim = c(0, 20)) 
abline(v = mean(scores), col = "red", lwd = 2, lty = 2)


## ----ctt52, echo=TRUE------------------------------------------------------
hci_scored$reliability


## ----ctt53b, echo=TRUE-----------------------------------------------------
scores_scaled <- CTT::score.transform(scores = scores, 
                  mu.new = 50, 
                  sd.new = 15) 


hist(x = scores_scaled$new.scores,
     xlab = "Toplam Puan", 
     main = "Toplam Puan Dağılımı",  
     col = "lightblue", 
     xlim = c(0, 100))

abline(v = mean(scores_scaled$new.scores), col = "red", lwd = 2, lty = 2)


## ----ctt54, echo=TRUE, fig.width=9, fig.height=6---------------------------
cormat_hci <- psych::tetrachoric(x = hci_scored$scored)$rho

ggcorrplot::ggcorrplot(corr = cormat_hci,
                       type = "lower", 
                       show.diag = TRUE,
                       lab = TRUE, 
                       lab_size = 3) 


## ----ctt55, echo=TRUE------------------------------------------------------
hci_items_scored <- hci_scored$scored 
hci_itemanalysis <- CTT::itemAnalysis(items = hci_items_scored, pBisFlag = .2, bisFlag = .2)

hci_itemanalysis



## --------------------------------------------------------------------------
hci_itemanalysis$itemReport


## ----ctt56, echo=TRUE------------------------------------------------------
hci_itemanalysis2 <- CTT::itemAnalysis(items = hci_items_scored[, -c(7, 17)], 
      pBisFlag = .2, 
      bisFlag = .2)
hci_itemanalysis2


## --------------------------------------------------------------------------
hci_itemanalysis2$itemReport



## ----ctt56b, echo=TRUE-----------------------------------------------------
sem.ctt <- function(x, ci.level = 0.95) {
  require("CTT")
  rxx <- CTT::itemAnalysis(items = x)$alpha
  scores <- rowSums(x, na.rm = TRUE)
  sigma <- sd(scores, na.rm = TRUE)
  sem <- sigma*sqrt((1-rxx))
  z <- qnorm(1-(1-ci.level)/2)
  output <- data.frame(lower_CI = scores - (sem*z),
                       observed = scores,
                       upper_CI = scores + (sem*z))
  return(output)
}

sem_hci <- sem.ctt(x = hci_items_scored, ci.level = 0.95)
head(sem_hci)


## ----dif2, echo=FALSE, R.options = list(width = 110)-----------------------
hci_scored <- read.csv("hci_scored.csv", header = TRUE)
head(hci_scored)


## ----dif3, echo=TRUE-------------------------------------------------------
table(hci_scored$gender)
table(hci_scored$eng_first_lang)


## ----dif4, echo=TRUE-------------------------------------------------------
hci_items <- dplyr::select(hci_scored, starts_with("item"))
gender <- hci_scored$gender
language <- hci_scored$eng_first_lang


## ----dif5, echo=TRUE-------------------------------------------------------
# 1) Run the DIF analysis based on gender
gender_MH <- difR::difMH(Data = hci_items, 
                         group = gender, 
                         focal.name = "F",
                         match = "score",  
                         purify = TRUE) 
print(gender_MH)


## --------------------------------------------------------------------------
plot(gender_MH)



## ----dif6, echo=TRUE-------------------------------------------------------
lang_MH <- difR::difMH(Data = hci_items, 
                       group = language, 
                       focal.name = "no", 
                       match = "score", 
                       purify = TRUE) 
print(lang_MH)


## --------------------------------------------------------------------------
plot(lang_MH)



## ----dif7, echo=TRUE-------------------------------------------------------
# 1) Run the DIF analysis based on gender
gender_LR <- difR::difLogistic(Data = hci_items, 
                               group = gender, 
                               focal.name = "F", 
                               match = "score",
                               type = "both", 
                               purify = TRUE) 
print(gender_LR)



## --------------------------------------------------------------------------
plot(gender_LR)



## ----dif8, echo=TRUE-------------------------------------------------------
plot(gender_LR, item = 1, plot = "itemCurve")
plot(gender_LR, item = 19, plot = "itemCurve")


## ----dif9, echo=TRUE-------------------------------------------------------
# 2) Run the DIF analysis based on language
lang_LR <- difR::difLogistic(Data = hci_items, 
                             group = language, 
                             focal.name = "no", 
                             match = "score", 
                             type = "both", 
                             purify = TRUE) 

print(lang_LR)

plot(lang_LR)


## ----dif10, echo=TRUE------------------------------------------------------
plot(lang_LR, item = 10, plot = "itemCurve")
plot(lang_LR, item = 16, plot = "itemCurve")


## ----ctt0, eval=FALSE, echo=FALSE------------------------------------------
## # Run the following to extract the R codes from this file
## knitr::purl("ctt.Rmd")

