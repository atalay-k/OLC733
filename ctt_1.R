
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


## ----nfc, eval=TRUE, echo=FALSE--------------------------------------------
nfc_items <- data.frame(
  Items = 1:16,
  Description = c(
    "Enjoyment of tasks that involve problem-solving",
    "Preference for cognitive, difficult and important tasks",
    "Tendency to strive for goals that require mental effort",
    "Appeal of relying on one’s thought to be successful (R)",
    "Satisfaction of completing important tasks that required thinking and mental effort",
    "Preference for thinking about long-term projects (R)",
    "Preference for cognitive challenges (R)",
    "Satisfaction on hard and long deliberation (R)",
    "Attitude towards thinking as something one does primarily because one has to (R)",
    "Appeal of being responsible for handling situations that require thinking (R)",
    "Attitude towards thinking as something that is fun (R)",
    "Anticipation and avoiding of situations that may require in-depth thinking (R)",
    "Preference for puzzles to be solved",
    "Preference for complex over simple problems",
    "Preference for understanding the reason for an answer over simply knowing the answer without any background (R)",
    "Preference to know how something works over simply knowing that it works (R)")
)

nfc_items %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 16) %>%
  footnote(general = "Items marked with (R) were presented in an inverted form.",
           general_title = "Note: ",
           title_format = c("italic"),
           footnote_as_chunk = T)


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


## ----ctt6------------------------------------------------------------------
nfc <- read.csv("nfc_data.csv", header = TRUE)
paged_table(nfc, options = list(cols.print = 12, rows.print = 6))


## ----ctt9, echo=TRUE-------------------------------------------------------
str(nfc)


## ----ctt10-----------------------------------------------------------------
DataExplorer::introduce(nfc)


## ----ctt11, echo=FALSE-----------------------------------------------------
kbl(t(introduce(nfc)), 
    row.names = TRUE, col.names = "", 
    format.args = list(big.mark = ",")) %>%
  kable_styling()


## ----ctt12, echo=TRUE------------------------------------------------------
DataExplorer::plot_intro(nfc)


## ----ctt13a, echo=TRUE-----------------------------------------------------
DataExplorer::plot_missing(nfc)


## ----ctt13b, echo=TRUE, fig.height=4---------------------------------------
DataExplorer::plot_bar(data = nfc[, c("education", "sex")])


## ----ctt13c, echo=TRUE, fig.height=4---------------------------------------
DataExplorer::plot_histogram(data = nfc[, c("age", "self_control", "action_orientation", "effortful_control")])


## ----ctt13d, echo=TRUE, fig.height=4---------------------------------------
DataExplorer::plot_boxplot(data = nfc[!is.na(nfc$sex), # cinsiyet değişkeninde eksik verisi olmayanlar
c("sex", "self_control", "action_orientation", "effortful_control")],  by = "sex") # Kategorik değişken düzeyleri için


## ----ctt13e, echo=TRUE, eval=FALSE-----------------------------------------
## # id değişkeni analizlere dahil edilmediği için çıkarıldı
## nfc <- DataExplorer::drop_columns(nfc, "id")
## 
## DataExplorer::create_report(data = nfc,
##                             report_title = "Veri On Inceleme",
##                             output_file = "oninceleme.html")


## ----ctt14-----------------------------------------------------------------
skimr::skim(nfc)


## ----ctt15b, R.options = list(width = 200)---------------------------------
psych::describe(x = nfc)


## ----ctt16, echo=TRUE------------------------------------------------------
matris <- dplyr::select(nfc,
                          starts_with("nfc"))

head(matris)


## ----ctt18-----------------------------------------------------------------
cormat <- psych::polychoric(x = matris)$rho
cormat %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), font_size = 11)


## ----ctt20, echo=TRUE, fig.width=9, fig.height=6---------------------------
ggcorrplot::ggcorrplot(
  corr = cormat, # korelasyon matirisi
  type = "lower", # alt kösegen
  show.diag = TRUE, # kosegen
  lab = TRUE, # degerleri ekleme
  lab_size = 3) # 


## ----ctt21, echo=TRUE------------------------------------------------------
# ters kodlacak maddeler -1 ile belirtilmiştir.
nfc_key <- c(1,1,1,-1,1,-1,-1,-1,-1,-1,-1,-1,1,1,-1,-1)

ters_matris <- psych::reverse.code(
  keys = nfc_key, # ters kodlanacak maddeler
  items = matris, # veri
  mini = 1, # minumum deger
  maxi = 7) # maksimum deger


## ----ctt22, echo=TRUE, fig.width=9, fig.height=6---------------------------
cor_ters_matris <- 
psych::polychoric(ters_matris)$rho

ggcorrplot::ggcorrplot(
  corr = cor_ters_matris,
  type = "lower", 
  show.diag = TRUE,
  lab = TRUE, 
  lab_size = 3) 


## ----ctt23, echo=TRUE, R.options = list(width = 200)-----------------------
# yeniden adlandırma
colnames(ters_matris) <- colnames(matris)
ters_matris <- as.data.frame(ters_matris)
head(ters_matris)


## --------------------------------------------------------------------------
library("OpenMx")
means_cor <-
mean(vechs(cor_ters_matris))
library(qgraph)
qgraph(cor_ters_matris, 
cut=0, layout="spring", 
title=paste("Korelasyon matrisi, 
ortalama korelasyon = ",  
round(means_cor, digits=2), 
sep=" "))






## --------------------------------------------------------------------------
library(EGAnet)
nw_ega <- EGA(ters_matris)
summary(nw_ega)


