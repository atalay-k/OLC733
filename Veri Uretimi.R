knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)
options(digits=3)
library(dplyr)
library(tidyverse)
library(dplyr)
library(tuev)
library(emo)
library(knitr)


x <- 0:2
y <- dbinom(x,size=2,p=0.5)
y

plot(x, y ,type="h",col="red", lwd=10,
main="bozuk parayi iki kere havaya atma")

x <- 0:4
y <- dbinom(x,size=4,p=0.5)
y

plot(x, y ,type="h",col="red", lwd=10,
main="bozuk parayi dört kere havaya atma")

x <- 0:30
y <- dbinom(x,size=30,p=0.5)
y

plot(x, y ,type="h",col="red", lwd=10,
     main="bozuk parayi iki kere havaya atma")

n=50
nd1 <- rnorm(n=n, mean=50, sd=10)
mean(nd1)
sd(nd1)

hist(nd1)

nd2 <- runif(n=n, min=50, max=150)
min(nd2)
sd(nd2)

hist(nd2)

set.seed(41)
x <- rnorm(2, mean=100, sd=20)
mean(x)

n_k <- 10                 # Küçük örneklem
n_b <- 50                 # Büyük örneklem

tekrar <- 10000              # Tekrar sayısı
kucuk_orn <- numeric(tekrar) # Küçük örneklem ort.
buyuk_orn <- numeric(tekrar) # Büyük örneklem ort.

orneklem.normal <- rnorm(n = 1000, mean = 35, sd = 15)#Sekil

for (i in 1:tekrar) {
  kucuk_orn[i] <- mean(rnorm(n = n_k,mean =  35, sd = 15)  ) 
  buyuk_orn[i] <- mean(rnorm(n = n_b,mean =  35, sd = 15)  ) 

} 

par(mfrow = c(3,1))
hist(orneklem.normal, breaks = 50, main = "Dağılımı", xlab = "",col = "steelblue")
hist(kucuk_orn, breaks = 50, main = "Küçük Örneklemlerin \n Ortalama Dağılımı", xlab = "Ortalama",col = "steelblue")
hist(buyuk_orn, breaks = 50, main = "Büyük Örneklemlerin \n Ortalama Dağılımı", xlab = "Ortalama",col = "steelblue")

n_k <- 10                 # Küçük örneklem
n_b <- 50                 # Büyük örneklem

tekrar <- 10000              # Tekrar sayısı
kucuk_orn <- numeric(tekrar) # Küçük örneklem ort.
buyuk_orn <- numeric(tekrar) # Büyük örneklem ort.

orneklem.carpik <- rexp(n = 10000, rate = 1.5)#Sekil

for (i in 1:tekrar) {
  kucuk_orn[i] <- mean(rexp(n = 10000, rate = 1.5))
  buyuk_orn[i] <- mean(rexp(n = 10000, rate = 1.5) ) 

} 

par(mfrow = c(3,1))
hist(orneklem.carpik, breaks = 50, main = "Dağılımı", xlab = "",col = "steelblue")
hist(kucuk_orn, breaks = 50, main = "Küçük Örneklemlerin \n Ortalama Dağılımı", xlab = "Ortalama",col = "steelblue")
hist(buyuk_orn, breaks = 50, main = "Büyük Örneklemlerin \n Ortalama Dağılımı", xlab = "Ortalama",col = "steelblue")

## dir.create("Simulasyon_k")
## dir.create("Simulasyon_b")
## 
## for (i in 1:5) {
##   write.table(rnorm(n = n_k,mean =  35, sd = 15), file=paste("Simulasyon_k/simulasyon_",i,".txt", sep=""))
## 
##   write.table(rnorm(n = n_b,mean =  35, sd = 15), file=paste("Simulasyon_b/simulasyon_",i,".txt", sep=""))
## }
## 
## 

## kucuk_orn <- list()
## buyuk_orn <- list()
## for(i in 1:5){
##   kucuk_orn[[i]] <- read.table(file=paste("Simulasyon_k/simulasyon_",i,".txt",sep=""))
##     buyuk_orn[[i]] <- read.table(file=paste("Simulasyon_b/simulasyon_",i,".txt",sep=""))
## }
## 

set.seed(41)
madde <- 8
maddepar <- cbind(
rnorm(madde, mean = 0, sd = 0.75)*1.702,    #a
rnorm(madde, mean = 0.30, sd = 0.51)*1.702, #b
rnorm(madde, mean = 0.16, sd = 0.05))       #c 

maddepar


set.seed(41)
birey <- 1000
yetenek <- rnorm(birey, mean = 0, sd = 1)
yetenek[1:10]

veri <- irtoys::sim(ip = maddepar, x = yetenek)
  colnames(veri) <- paste0("madde", 1:madde)
  head(veri)


library(irtoys)
ip = maddepar
x = yetenek
sim

library(mirt)
model3PL <- mirt(veri, # cevap matrisi
                 1,  # tek boyutlu model
                 itemtype = "3PL", # 3PL
                 verbose = FALSE, 
                 technical = list(NCYCLES = 1000,
                 message = FALSE))

mirt::coef(model3PL, IRTpars = TRUE, simplify = TRUE)$items

## veri_uretimi <- function(madde, birey ){
##   .....
## }

veri_uretimi <- function(maddesay, bireysay, seed) {
  # seed ayaralanır
  set.seed(seed)
  # madde parametreleri üretilir.
  maddepar <- cbind(
    rnorm(maddesay, mean = 1.13, sd = 0.25)*1.702, #a
    rnorm(maddesay, mean = 0.21, sd = 0.51)*1.702, #b
    rnorm(maddesay, mean = 0.16, sd = 0.05)) #c
  # yetenek parametreleri üretilir
  yetenek <- rnorm(bireysay, mean = 0, sd = 1)
  # 3PL modele göre veri uretimi
  cevaplar <- irtoys::sim(ip = maddepar, x = yetenek)
  colnames(cevaplar) <- paste0("madde", 1:maddesay)
  # Parametrelerin ve ciktinin nesnede toplanması
  veri <- list(maddepar = maddepar,
               yetenek = yetenek,
               seed = seed,
               cevaplar = cevaplar)
  # Cikti
  return(veri)
}

veri_1 <- veri_uretimi(madde = 8, birey = 1000, seed = 666)
head(veri_1$yetenek)
head(veri_1$maddepar,3)
head(veri_1$cevaplar,3)

## kestirilen_par <- function(veri, par=3){
## 
##   ....
## }

kestirilen_par <- function(veri, par=3){

  if(par==3){
    model <- mirt::mirt(veri, # cevap matrisi
                         1,  # tek boyutlu model
                         itemtype = "3PL", # 3PL
                         verbose = FALSE, 
                         technical = list(NCYCLES = 1000,
                                          message = FALSE))
  }else {
        model <- mirt::mirt(veri, # cevap matrisi
                         1,  # tek boyutlu model
                         itemtype = "2PL", # 2pl
                         verbose = FALSE, 
                         technical = list(NCYCLES = 1000,
                                          message = FALSE))
  }
# Madde parametreleri
kestirim <- as.data.frame(mirt::coef(model, IRTpars = TRUE, simplify = TRUE)$item[,1:3])
kestirim
}

kestirim <- kestirilen_par(veri_1$cevaplar)

## hata <- function(kestirim, gercek) {
## 
## }
## 
## 

kestirim =  kestirim
gercek = veri_1$maddepa
hata <- function(kestirim, gercek) {
  result <- data.frame(
    parametreler = c("a", "b", "c"),
    bias = sapply(1L:3L, function(i) mean((kestirim[, i] - gercek[,i]))),
    rmse = sapply(1L:3L, function(i) sqrt(mean((kestirim[, i] - gercek[,i])^2))),
    korelasyon = sapply(1L:3L, function(i) cor(kestirim[, i], gercek[,i])))
  return(result)
}
hata(kestirim,veri_1$maddepar)


temp2 <- veri_uretimi(maddesay = 10, bireysay = 1000)


veri_uretimi <- function(maddesay, bireysay, seed = NULL){
  if(!is.null(seed)) {
    set.seed(seed)}
  else {
    seed <- sample.int(10000, 1)
    set.seed(seed)
    cat("atanan seed = ", seed, "\n")
  }
# madde parametreleri üretilir.
  print("Madde parametresi uretme")
  
maddepar <- cbind(
  rnorm(maddesay, mean = 1.13, sd = 0.25)*1.702, #a
  rnorm(maddesay, mean = 0.21, sd = 0.51)*1.702, #b
  rnorm(maddesay, mean = 0.16, sd = 0.05)) #c

# yetenek parametreleri üretilir
yetenek <- rnorm(bireysay, mean = 0, sd = 1)
# 3PL modele göre veri uretimi
print("Üretilen veri")
cevaplar <- irtoys::sim(ip = maddepar, x = yetenek)
colnames(cevaplar) <- paste0("madde", 1:maddesay)
# Parametrelerin ve ciktinin nesnede toplanması
print("Cıktıların birleştirilmesi")
veri <- list(maddepar = maddepar,
             yetenek = yetenek,
             seed = seed,
             cevaplar = cevaplar)
# Cikti
return(veri)
}

 temp2 <- veri_uretimi(maddesay = 10, bireysay = 1000)


## replicate(tekrar sayisi, fonksiyon)

tekrarsayisi=5L
x <- replicate(tekrarsayisi, veri_uretimi(maddesay = 10, bireysay = 1000))

lapply(1L:tekrarsayisi, function(i) x[,i][1])

sapply(1L:tekrarsayisi, function(i) x[,i][1])

tekrar = 4 
seed = sample.int(10000, 100)
maddesay = 10 # 10, 15, 20, or 25
bireysay = 1000 # 250, 500, 750, or 1000

library("doParallel")
detectCores()
cl <- makeCluster(4) # en fazla n-2 
registerDoParallel(cl)

foreach(i=1:4) %dopar% sqrt(i)





#coklu arguman
foreach(i=1:4, j=1:4) %do%
	sqrt(i+j)

stopCluster(cl) # cekirdek atama işini bitirir.

## 
##   for(i in 1:5) {
##     sum(rnorm(1e6))
##     }

##   foreach(i=1:5) %dopar% {
##     sum(rnorm(1e6))
##     }
## 

cl <- makeCluster(4) 
registerDoParallel(cl)

tekrar = 4 
seed = sample.int(10000, 100)
maddesay = 10 # 10, 15, 20, or 25
bireysay = 1000 # 250, 500, 750, or 1000

simulasyon <- foreach(i=1:4,
             .packages = c("mirt", "doParallel"),
             .combine = rbind) %dopar% {
# Adım 1 madde parametrelerini ve veri setini üretme
adim1 <- veri_uretimi(maddesay =maddesay, bireysay =bireysay, seed=seed[i])
# Adım 2 üretilen veri seti üzerinden ketsirim yapma
adim2 <- kestirilen_par(adim1$cevaplar)
# adim 3 raporlama
hata(adim2, adim1$maddepar)
}

simulasyon

## library("dplyr")
## simulasyon %>%
## 
## 

library(tidyverse)
simulasyon_v1 <- simulasyon %>%
  group_by(parametreler) %>%
  summarise(bias = round(mean(bias),3),
            rmse = round(mean(rmse),3),
            korelasyon = round(mean(korelasyon),3)) %>%
  mutate(maddesay = maddesay,
         bireysay = bireysay) %>%
  as.data.frame()


simulasyon_v1

tekrar = 1;seed = sample.int(10000, 100);maddesay = c(10, 20 ,40);bireysay = c(250, 500, 1000)

# kumeler 
cl <- makeCluster(6);registerDoParallel(cl)

# İc ice foreachler
sonuc <- foreach(i=1:tekrar, 
                      .packages = c("mirt", "doParallel", "dplyr"),
                      .combine = rbind) %:%
  foreach(j=maddesay, 
          .packages = c("mirt", "doParallel", "dplyr"),
          .combine = rbind)  %:%
  foreach(k=bireysay, 
          .packages = c("mirt", "doParallel", "dplyr"),
          .combine = rbind)    %dopar% {
            adim1 <- veri_uretimi(maddesay=j, bireysay=k, seed=seed[i])
            adim2 <- kestirilen_par(adim1$cevaplar)
            adim3 <- hata(adim2, adim1$maddepar)
            adim3 %>%
              group_by(parametreler) %>%
              summarise(bias = round(mean(bias),3),
                        rmse = round(mean(rmse),3),
                        korelasyon = round(mean(korelasyon),3)) %>%
              mutate(maddesay = j, bireysay = k,
              ) %>%              as.data.frame()
          }

stopCluster(cl)



## 
##  write.table(adim3, "results.csv",
##             sep = ",",
##             col.names = FALSE,
##            row.names = FALSE,
##            append = TRUE) # Sonucları yazmak icin acik tutar
## 
## 

iterations = 4; seed = sample.int(10000, 100)
maddesay = 10 ;bireysay = 1000 #250, 500, 750, or 1000
library("doSNOW")
cl <- makeCluster(4);registerDoSNOW(cl)

pb <- txtProgressBar(max = iterations, style = 3) # Initiate progress bar
progress <- function(n) {setTxtProgressBar(pb, n)}
opts <- list(progress = progress)
sonuc <- foreach(i=1:iterations,
                      .packages = c("mirt", "doSNOW"),
                      .options.snow = opts, # see the additional line for doSNOW
                      .combine = rbind) %dopar% {
                        # Generate item parameters and data
                        adim1 <- veri_uretimi(maddesay  , bireysay, seed=seed[i])
                        # Estimate item parameters
                        adim2 <- kestirilen_par(adim1$cevaplar)
                        # Summarize results
                        hata(adim2, adim1$maddepar)
                      }
close(pb) # kapat progress bar
stopCluster(cl)


