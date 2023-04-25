for(i in 1:10) {
print(i)
}

for(i in 5:8) {
print(i)
}

for(i in 5:10){
print("Merhaba")
}

for(i in 1:10){
    cat(i," +  ", i ," = ", 2*i,  "\n")
}

(X <- cbind(a = 1:5, b=2:6))


Y <- array()
for(i in 1:nrow(X)) {
Y[i] <- X[i,1] + X[i,2]
}
Y

islem.kontrol <- array()
for(i in 1:10){
islem.kontrol[i] <- paste("Islem ", i, " tamamlandi", sep="")
}
islem.kontrol

set.seed(10)
x <- sample(1:10000,100)

sayac <- 0
for (val in x) {
  if(val %% 2 == 0){
    sayac = sayac+1
  }
}
print(sayac)

for(i in 1:3){
      if (i==2) cat("indeks cift sayidir:","\n")
      else cat(i,"\n")
}

for(i in 1:3){
  if (i==2) {
  cat("indeks degeri ikidir:",i,"\n") 
}else{cat("indeks degeri iki degildir","\n")}
    }

m2 <- matrix(0,nrow=5,ncol=5)
m2

for(j in 1:nrow(m2)){
for(i in 1:ncol(m2)){
        m2[i,j] <- i*j
}
}
m2

# n<-as.numeric(readline(prompt = "Kare matriste satir/sutun sayisi olarak kullanilmak uzere bir sayi yaziniz: "))
n<- 3
matris<-matrix(0,n,n)
for(satir in 1:n){
  for(sutun in 1:n){
        matris[satir,sutun]<- satir*sutun
}
}
if(nrow(matris)<=10){
  matris
}else{
  matris[1:10,1:10]
}

d <- data.frame(a = 1:5, b=2:6)
d


for(x in d) {
  cat("sutun toplamlari:", sum(x), "\n")
}

X <- cbind(1:10, 20:29)
X

for(i in 1:nrow(X)){
  cat(i, "satirdaki degerlerin carpimi",X[i,1] * X[i,2], "olarak hesaplanmistir.", "\n") 
}


for(i in 1:6){
  if(i==3){
    next
}
  print (i)}

for(i in 1:12){
  if(i==3){
    break
  }
  print (i)}

# donguler uzun zamanda calisir
set.seed(853)
y<-matrix(rnorm(1000000),nrow=1000)
z<-0*y
time1<-as.numeric(Sys.time())
#loop:
time2 <- system.time(
for(i in 1:1000){
  for(j in 1:1000){
      z[i,j]<-y[i,j]^2
}
})

time2


# ayni islemi dongusuz yapma
time3 <- system.time(z<-y^2)
time3


create_df <- function(veriseti=3, sutunsay=c(5,5,5),satirsay=c(20,30,40)){
        df_list <- list()
  for(i in 1:veriseti){
  df <- data.frame(matrix(0,ncol=sutunsay[i],nrow=satirsay[i]))

  for(j in 1:sutunsay[i]){
  df[,j] <- round(rnorm(satirsay[i],0,1),2)
  }
    df_list[[i]] <- df
    df_list
      writexl::write_xlsx(df_list[[i]],paste("veri",i,".xlsx", sep=""))

  }

}
create_df(3,c(5,5,8),c(20,30,40))


n <- 10
fib <- numeric(n)
fib[1] <- 1
fib[2] <- 1
for (i in 3:n)
{
fib[i] <- fib[i-1]+fib[i-2] 

}
fib


set.seed(1786)
ornek<-exp(matrix(rnorm(2000),nrow=100))
index1.temp<-sample(1:100,10)
index2.temp<-sample(1:20,10)
for(i in 1:10){
  ornek[index1.temp[i],index2.temp[i]]<--1
}
head(ornek,6)

ornek<-exp(matrix(rnorm(2000),nrow=100))
index1.temp<-sample(1:100,10)
index2.temp<-sample(1:20,10)
for(i in 1:10){
  ornek[index1.temp[i],index2.temp[i]]<--1
}

islem<-array()
for(i in 1:nrow(ornek)){
  if (all(ornek[i,]>=0)) islem[i]<- print(paste("Satir",i,"ortalamasi", mean(ornek[i,])))
  else if (sum(ornek[i,]<0)>3) islem[i]<- stop(paste("Satir", i ,"cok sayida negatif sayı icermektedir."))
  else if (sum(ornek[i,]<0)<=3) islem[i]<- print(paste("Satir", i ,"negatif sayi icermektedir."))
  
}
