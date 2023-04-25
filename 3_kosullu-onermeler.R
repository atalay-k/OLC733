
x <- 75
if(x>=65){
print("Başarılı") 
}

x <- 60
if(x>=65){
print("Başarılı") 
}

x <- 75 # Başarılı Durum
if(x>=65){
print("Başarılı")
}else{
print("Başarısız")
}

x <- 75 # Başarılı Durum
if(x>=90){
print("AA")
}else if(x>=80){
print("BA")
}else if(x>=70){
print("BB")
}else if(x>=65){
print("CB")
}else if(x>=60){
print("CC")
}else if(x>=50){
print("DD")
}else if(x>=30){
print("FD")
}else{
print("FF")
}

x <- 5
if(x!=0){
  paste(x,"'in carpmaya gore tersi 1/",x, sep="")
}


x <- 0
if(x!=0){
  paste(x,"'in carpmaya gore tersi 1/",x)
}else{ paste("1/",x," tanımsızdır.", sep="")
       }

x <- c(1,2,3,0,-4)
ifelse(x!=0,
  paste(x,"'in carpmaya gore tersi 1/",x ,sep=""),
  paste("1/",x," tanımsızdır.", sep=""))

x <- rnorm(1)
x

x <- rnorm(1)
x
if(x>1){
  print("sayi 1'den büyüktür.")
}else if(x<=1 & x>=-1){
 print ("sayi -1 ile +1 arasında")
}else{
   print ("sayi -1'den küçük")
}

x <- c(1,2,-3,4)
if(all(x>0)){
  
  print("tum sayilar 0'dan buyuktur")
  
} else{
  
  print("tum sayilar 0'dan buyuk degildir")
}

x <- c(1,2,-3,4)
if(any(x<0)){
  
  print("nesne en az bir negatif sayi icerir")
  
} else{
  
  print("nesne negatif sayi icermez")
}

x <- 2
if(x == 2) {
  goster3 <- "Dogru"  
  goster3b <- c(1,2,3)
  goster3c <- sample(1:1000,4)
} else  {
  goster3 <- "Yanlis"  
  goster3b <- c(3,2,1)
  goster3c <- 10000 + sample(1:1000,4)
}
goster3
goster3b
goster3c

x <- 20
ifelse(x>=65, "Başarılı", "Başarısız")

(x <- c(1,2,3,4,99,5))
ifelse(x==99, NA, x)


set.seed(41)
sayilar <- sample(50:90,27)
sayilar

ifelse(sayilar %% 2 == 0, "Cift Sayi", "Tek Sayi")


set.seed(987)
sayilar <- sample(-10:10,27,replace=TRUE)
sayilar

ifelse(sayilar == 0, "Sıfır", ifelse( sayilar <0, "Negatif", "Pozitif"))


vize <- c(60,70,80,90,55)
final <- c(45,65,70,50,80)
devam <- c(14,10,13,12,11)
