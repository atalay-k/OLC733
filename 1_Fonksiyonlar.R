library(magrittr)
library(COUNT)
library(dplyr)

data(loomis)
 n_visits <- loomis$anvisits

vec1 <- sample(1:1000,30)
mean(vec1, 0.1, TRUE)

mean(na.rm= TRUE, trim=0.1,x=vec1)

library(readr)
df <- read_csv("df.csv")


BDK1 <- (sd(df$S1)/mean(df$S1)) *100
BDK2 <- (sd(df$S2)/mean(df$S2)) *100
BDK3 <- (sd(df$S3)/mean(df$S2)) *100 #<<
BDK4 <- (sd(df$S4)/mean(df$S4)) *100
BDK5 <- (sd(df$S5)/mean(df$S5)) *100
BDK6 <- (sd(df$S6)/mean(df$S6)) *100

F1 <- function(x) { #<<
(x+1) #<<
} #<<
girdi <- c(2,5,6,7,12)
cikti <- F1(girdi)
cikti

args(read.csv) 

us_alma <- function(x,us=2){
x^us
}
# Fonksiyonun olağan değeri ile çalıstırılması
us_alma(2) # ikinci dereceden üs alınır.
us_alma(2,5)

#us_alma() fonksiyonun gövdesinin incelenmesi
body(us_alma)

kup_ozellik <- function(){
}

# İfadenin bir nesneye tanımlanarak fonksiyon oluşturulması
# Tek argümanlı fonksiyon örneği
kup_hacim <- function(x){
hacim <- x^3 #hacimin tanımlanması
return(hacim)
}
# Kenar uzunluğu 3cm olan kupün hacmi
kup_hacim(3)
#İfadenin bir nesneye tanımlamadan fonksiyon oluşturulması
kup_hacim <- function(x){
return(x^3)   #hacimin tanımlanması
}

kup_hacim <- function(x){
hacim <- x^3 #hacimin tanımlanması
return(hacim)
}
body(kup_hacim) # Fonsiyonun gövdesi
formals(kup_hacim) # Fonsiyonun argümanlarının listesi

environment(kup_hacim) # Fonsiyonun çevresinin belirlenmesi

library(assertive)
 bolme <- function(x){
  assert_is_numeric(x)
  1/x }
  bolme(3)

ogrenci <- c("ARIF","ASLI","ATA",
            "AYSE","BURCAK",
             "CAGATAY","EMRE","FEYZI",
             "FURKAN", "HARUN","KORKUT",
             "MEHMET","RAMAZAN",
             "SEMIH","SINEM")
sample(ogrenci,1)

# taslak hazırlama
random_secici <- function() {
### burası fonksiyon kodlarının yazılacagı alan
}

random_secici <- function() {
ogrenci <- c("ARIF","ASLI","ATA",
            "AYSE","BURCAK",
             "CAGATAY","EMRE","FEYZI",
             "FURKAN", "HARUN","KORKUT",
             "MEHMET","RAMAZAN",
             "SEMIH","SINEM")
sample(ogrenci,1)
}

random_secici()


random_secici <- function() {
ogrenci <- c("ARIF","ASLI","ATA",
            "AYSE","BURCAK",
             "CAGATAY","EMRE","FEYZI",
             "FURKAN", "HARUN","KORKUT",
             "MEHMET","RAMAZAN",
             "SEMIH","SINEM")
sample(ogrenci,1)
}

random_secici <- function(x,n) {
sample(x,n) 
  }

random_secici <- function(x,n) {
sample(x,n) 
  }
random_secici(ogrenci,1)

harf_not <- function(x, n, na.rm, labels, interval_type) {
      probs <- seq(0, 1, length.out = n + 1)
      qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
      right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}


harf_not <- function(x, n, na.rm, labels, interval_type) {
      probs <- seq(0, 1, length.out = n + 1)
      qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
      right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}



harf_not(
  x,
  n,
  na.rm, 
  labels = c("very low", "low", "medium", "high", "very high"),
  interval_type = "(lo, hi]"
)

harf_not <- function(x, n, na.rm, labels, interval_type) {
      probs <- seq(0, 1, length.out = n + 1)
      qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
      right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}


x <- sample(0:100,30)

harf_not <- function(x, n = 5, na.rm = FALSE, labels, interval_type) {
      probs <- seq(0, 1, length.out = n + 1)
      qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
      right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
      cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

harf_not(
  x,
  labels = c("F", "D", "C", "B", "A"),
  interval_type = "(lo, hi]"
)


x <- sample(0:100,30)
harf_not <- function(x, n = 5, na.rm = FALSE, labels = NULL, 
                            interval_type = c("(lo, hi]", "[lo, hi)")) {
  interval_type <- match.arg(interval_type)
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

harf_not(x)

y <- 1:5
length(y)/sum(1/y) #ortalama işleminin tersi
harmonik_ort <- function() {
  
}

tersal <- function(x) {
    1/x
}
y <- 1:5
# harmonik ortalama hesaplama

y %>% tersal() %>% mean() %>% tersal


harmonik_ort <- function(x) {
    x%>% 
    tersal() %>% 
    mean() %>%
    tersal
  
}
harmonik_ort(y)

sp500 <- readRDS("sp500.rds")

sp500 %>%
  group_by(sector) %>%
  summarise(hmean_pe_ratio = harmonik_ort(pe_ratio))

harmonik_ort <- function(x,na.rm=FALSE) {
    x%>%
    tersal() %>%
    mean(na.rm=na.rm) %>%
    tersal()

}
sp500 %>%
  group_by(sector) %>%
  summarise(hmean_pe_ratio = harmonik_ort(pe_ratio,na.rm=TRUE))

harmonik_ort <- function(x,...) {
    x%>% 
    tersal() %>% 
    mean(...) %>%
    tersal()
  
}

sp500 %>%
  group_by(sector) %>%
  summarise(hmean_pe_ratio = harmonik_ort(pe_ratio,na.rm=TRUE))

library(assertive)

harmonik_ort <- function(x,...) {
    assert_is_numeric(x)
    x%>% 
    tersal() %>% 
    mean(...) %>%
    tersal()
  
}

# karakter deger girildiğinde
harmonik_ort(sp500$sector)


harmonik_ort <- function(x,...) {
    assert_is_numeric(x)
    if(any(is_non_positive(x), na.rm = TRUE)) {
    # Throw an error
    stop("x negatif degerler icermektedir..")
    }
    x%>% 
    tersal() %>% 
    mean(...) %>%
    tersal()
  
}

# karakter deger girildiğinde
harmonik_ort(sp500$pe_ratio -50)



mod <- lm(mpg ~ wt + qsec, data = mtcars)

str(mod)

library(broom)

list(
  # Get model-level values
  model = glance(mod),
  # Get coefficient-level values
  coefficients = tidy(mod),
  # Get observation-level values
  observations = augment(mod)
)
