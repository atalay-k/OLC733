
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


## ----ctt6------------------------------------------------------------------
nfc <- read.csv("nfc_data.csv", header = TRUE)
matris <- dplyr::select(nfc,
                          starts_with("nfc"))

head(matris)


## ----ctt21, echo=TRUE------------------------------------------------------
nfc_key <- c(1,1,1,-1,1,-1,-1,-1,-1,-1,-1,-1,1,1,-1,-1)

ters_matris <- psych::reverse.code(
  keys = nfc_key,
  items = matris, 
  mini = 1, 
  maxi = 7) 


## ----ctt22, echo=TRUE, fig.width=9, fig.height=6---------------------------
cor_ters_matris <- psych::polychoric(ters_matris)$rho
colnames(ters_matris) <- colnames(matris)
ters_matris <- as.data.frame(ters_matris)

head(ters_matris)


## ----ctt24, echo=TRUE------------------------------------------------------
itemanalysis_ctt <- CTT::itemAnalysis(items = ters_matris, pBisFlag = .2, 
bisFlag = .2)
itemanalysis_ctt$itemReport


## ----ctt25, echo=TRUE------------------------------------------------------
itemanalysis_psych <- psych::alpha(x = ters_matris)
itemanalysis_psych


## ----ctt26a, echo=TRUE, R.options = list(width = 110)----------------------
itemanalysis_shiny <- ShinyItemAnalysis::ItemAnalysis(Data = ters_matris)

itemanalysis_shiny


## ----ctt26b, echo=TRUE, R.options = list(width = 100)----------------------
ShinyItemAnalysis::DDplot(Data = ters_matris, discrim = "RIR")


## ----ctt29a, echo=TRUE-----------------------------------------------------
psych::splitHalf(r = ters_matris)


## ----ctt29b, echo=TRUE-----------------------------------------------------
sp_rel <- psych::splitHalf(r = ters_matris, raw = TRUE)
hist(x = sp_rel$raw,
     breaks = 101,
     xlab = "İki yarı güvenirliği", 
     main = "Tüm İki yarı güvenirlikleri") 
abline(v = mean(sp_rel$raw), col = "red", lwd = 2, lty = 2)


## ----ctt30a, echo=TRUE-----------------------------------------------------
itemanalysis_ctt


## ----ctt30b, echo=TRUE, eval=TRUE, background = "gray85"-------------------
itemanalysis_psych


## ----ctt31, echo=TRUE------------------------------------------------------
reliability_qme <- QME::analyze(test = ters_matris, id = FALSE, na_to_0 = FALSE)

reliability_qme$test_level


## ----ctt32, echo=TRUE------------------------------------------------------
CTT::spearman.brown(r.xx = 0.87, input = 0.5, n.or.r = "n")


## ----ctt33, echo=TRUE------------------------------------------------------
n <- CTT::spearman.brown(r.xx = 0.87, input = 0.90, n.or.r = "r")
round(n$n.new * 16, digits = 0)


## ----ctt34, echo=TRUE------------------------------------------------------
recode_nfc <- function(x) {
  car::recode(x, "1=-3; 2=-2; 3=-1; 4=0; 5=1; 6=2; 7=3")
}
ters_matris <- apply(ters_matris,
                          2, # her bir sutuna uygulanir
                          recode_nfc) 
nfc_score <- rowSums(ters_matris)
summary(nfc_score)



## --------------------------------------------------------------------------
hist(nfc_score,
     xlab = "NFC Toplam Puan",
     main = "NFC Puan Dağılımı")


## ----ctt35, echo=TRUE------------------------------------------------------
scores <- cbind(nfc_score, nfc[, c("action_orientation",
"self_control", "effortful_control")])


## ----ctt36, echo=TRUE, eval=TRUE-------------------------------------------
cormat_scores <- 
cor(scores, use = "pairwise.complete.obs")

ggcorrplot::ggcorrplot(corr = cormat_scores,
type = "lower", 
show.diag = TRUE,
lab = TRUE, 
lab_size = 3) 


## ----ctt39, echo=FALSE, eval=TRUE------------------------------------------
psych::correct.cor(x = cormat_scores, 
                   y = c(0.87, 0.791, 0.817, 0.783)) %>%
  round(., 2) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), font_size = 13)


## ----ctt40, echo=TRUE------------------------------------------------------
ivi <- function(item, criterion) {
s_i <- sd(item, na.rm = TRUE)
r <- cor(item, criterion, use = "complete.obs")
index <- s_i * r
return(index)
}

nfc_ivi <- apply(ters_matris, 
2,                
function(x) ivi(item = x,
criterion = nfc$action_orientation))

nfc_ivi <- as.data.frame(nfc_ivi)
print(nfc_ivi)


## ----ctt0, eval=FALSE, echo=FALSE------------------------------------------
## knitr::purl("ctt.Rmd")

