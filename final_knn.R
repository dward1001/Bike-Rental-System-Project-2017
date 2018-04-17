################################################

#### BIKE_B #### 
library(dplyr)
install.packages('DMwR')
library(DMwR)
library(class)
library(gmodels)
#library: dplyr,readxl,DMwR,class,gmodels
bike_B<-read.csv("bikey_26_B.csv")

ls(bike_B)
#독립변수
ls(bike_B)
IV<-c("a2030","a4050","busdist","bustotal","causal","suit","dens","fpop","male","school_park","subdist","subtotal","busrate","subrate","bikesum","bike_1")
IV<-c("a2030","busdist","causal","dens","fpop","male","school_park","subdist","busrate","subrate","bikesum")
IV<-c("dens","fpop","male","school_park","school","subdist","subtotal","bustotal","bikesum","bike_1")

IV2<-c("a2030","busdist","causal","dens","school","subdist","busrate","subrate","under20")
IV3<-c("a2030","busdist","causal","dens","school","subdist","busrate","subrate","under20")
IV4<-c("a2030","bustotal","causal","fpop","male","school_park","subtotal")
IV5<-c("busdist","busrate","dens","subdist","subrate","fpop","a2030","school_park")

str(bike_k)

IV<-c("a2030","busdist","causal","dens","fpop","male","school_park","subdist","busrate","subrate","bikesum")
IV<-c("school_park","subdist","subtotal","bustotal","bikesum","bike_1","fpop")

IV

  bike_k<-bike_B
  k <-readline('insert k please : ')
  bikeIV<-bike_k[IV]
  
  #bikeIV<-knnImputation(bikeIV)
  bike_k[IV]<-bikeIV[IV]
  normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}#표준화
  bikek_temp <- bike_k[order(runif(nrow(bike_k))),]
  bikek_n <- as.data.frame(lapply(bikek_temp[IV],normalize))
  
  
  
  
  
  ##정확도 
  
  #library("DMwR")
  #bikeIV<-knnImputation(bikeIV)
  bikek_train <- bikek_n[1:758,]
  bikek_test <- bikek_n[759:858,]
  
  bikek_train_labels1 <- bikek_temp[1:758,"totalhighlow"]
  bikek_test_labels1 <- bikek_temp[759:858,"totalhighlow"]
  bikek_train_labels2 <- bikek_temp[1:758,"totalRhighlow"]
  bikek_test_labels2 <- bikek_temp[759:858,"totalRhighlow"]
  bikek_train_labels3 <- bikek_temp[1:758,"totalQ"]
  bikek_test_labels3<- bikek_temp[759:858,"totalQ"]
  bikek_train_labels4 <- bikek_temp[1:758,"totalRQ"]
  bikek_test_labels4<- bikek_temp[759:858,"totalRQ"]
  bikek_train_labels5 <- bikek_temp[1:758,"totalQ3"]
  bikek_test_labels5<- bikek_temp[759:858,"totalQ3"]
  bikek_train_labels6 <- bikek_temp[1:758,"totalRQ3"]
  bikek_test_labels6<- bikek_temp[759:858,"totalRQ3"]
  
  #bikek_train_labels
  #library(class)

{
  HL = 0; RHL = 0; Q = 0; RQ = 0; Q3 = 0; RQ3 = 0;
  for(i in 1:10){   
  bikek_test_pred1 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels1, k)
  bikek_test_pred2 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels2, k)
  bikek_test_pred3 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels3, k)
  bikek_test_pred4 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels4, k)
  bikek_test_pred5 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels5, k)
  bikek_test_pred6 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels6, k)
  
  #install.packages("gmodels")
  #library(gmodels)
  
  
  highlow<-CrossTable(x=bikek_test_labels1, y=bikek_test_pred1, prop.chisq = FALSE)
  Rhighlow<-CrossTable(x=bikek_test_labels2, y=bikek_test_pred2, prop.chisq = FALSE)
  totalQ<-CrossTable(x=bikek_test_labels3, y=bikek_test_pred3, prop.chisq = FALSE)
  totalRQ<-CrossTable(x=bikek_test_labels4, y=bikek_test_pred4, prop.chisq = FALSE)
  totalQ3<-CrossTable(x=bikek_test_labels5, y=bikek_test_pred5, prop.chisq = FALSE)
  totalRQ3<-CrossTable(x=bikek_test_labels6, y=bikek_test_pred6, prop.chisq = FALSE)
  
  HL<-HL + highlow[[4]][[1,1]]+highlow[[4]][[2,2]]
  RHL<-RHL + Rhighlow[[4]][[1,1]]+highlow[[4]][[2,2]]
  Q<-Q + (totalQ[[4]][[1,1]]+totalQ[[4]][[4,4]])/(totalQ[[4]][[1,1]]+totalQ[[4]][[4,4]]+totalQ[[4]][[1,4]]+totalQ[[4]][[4,1]])
  RQ<-RQ + (totalRQ[[4]][[1,1]]+totalRQ[[4]][[4,4]])/(totalRQ[[4]][[1,1]]+totalRQ[[4]][[4,4]]+totalRQ[[4]][[1,4]]+totalRQ[[4]][[4,1]])
  Q3<-Q3 + (totalQ3[[4]][[1,1]]+totalQ3[[4]][[3,3]])/(totalQ3[[4]][[1,1]]+totalQ3[[4]][[3,3]]+totalQ3[[4]][[1,3]]+totalQ3[[4]][[3,1]])
  RQ3<- RQ3 + (totalRQ3[[4]][[1,1]]+totalRQ3[[4]][[3,3]])/(totalRQ3[[4]][[1,1]]+totalRQ3[[4]][[3,3]]+totalRQ3[[4]][[1,3]]+totalRQ3[[4]][[3,1]])
  }
  HL <- HL/10; RHL <- RHL/10; Q <- Q/10; RQ <- RQ/10; Q3 <- Q3/10; RQ3 <- RQ3/10;
  
  IV_name<- paste(IV,collapse=",")
  
  print(paste0("k=",k,"에서 total 상하 정확도는 ", round(HL*100,2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total rate 상하 정확도는 ",round(RHL*100.2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total 상 하위 25% 정확도는 ", round(Q*100,2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total rate 상 하위 25% 정확도는 ", round(RQ*100,2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total 상 하위 33% 정확도는 ", round(Q3*100,2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total rate 상 하위 33% 정확도는 ", round(RQ3*100,2),"% 입니다.  사용 변수 :", IV_name))
  
}




######################################################*



#### BIKE_A #### 
library(dplyr)
#library: dplyr,readxl,DMwR,class,gmodels
bike_A<-read.csv("bikey_26_A.csv")
#nrow(bike_A)

IV1<-c("a2030","a4050","busdist","bustotal","causal","suit","dens","fpop","male","school_park","subdist","subtotal","busrate","subrate","bikesum","bike_1")
IV<-c("a2030","busdist","causal","dens","fpop","male","school_park","subdist","busrate","subrate","bikesum","under20")

IV2<-c("a2030","busdist","causal","dens","school","subdist","busrate","subrate","under20")
IV3<-c("a2030","busdist","causal","dens","school","subdist","busrate","subrate","under20")
IV4<-c("a2030","bustotal","causal","fpop","male","school_park","subtotal")
IV5<-c("busdist","busrate","dens","subdist","subrate","fpop","a2030","school_park")

str(bike_k)

IV<-IV1


  
  
  bike_k<-bike_A
  k <-readline('insert k please : ')
  bikeIV<-bike_k[IV]
  
  #bikeIV<-knnImputation(bikeIV)
  bike_k[IV]<-bikeIV[IV]
  normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}#표준화
  bikek_temp <- bike_k[order(runif(nrow(bike_k))),]
  bikek_n <- as.data.frame(lapply(bikek_temp[IV],normalize))
  
  
  
  
  
  ##정확도 
  
  library("DMwR")
  #bikeIV<-knnImputation(bikeIV)
  bikek_train <- bikek_n[1:739,]
  bikek_test <- bikek_n[740:839,]
  
  bikek_train_labels1 <- bikek_temp[1:739,"totalhighlow"]
  bikek_test_labels1 <- bikek_temp[740:839,"totalhighlow"]
  bikek_train_labels2 <- bikek_temp[1:739,"totalRhighlow"]
  bikek_test_labels2 <- bikek_temp[740:839,"totalRhighlow"]
  bikek_train_labels3 <- bikek_temp[1:739,"totalQ"]
  bikek_test_labels3<- bikek_temp[740:839,"totalQ"]
  bikek_train_labels4 <- bikek_temp[1:739,"totalRQ"]
  bikek_test_labels4<- bikek_temp[740:839,"totalRQ"]
  bikek_train_labels5 <- bikek_temp[1:739,"totalQ3"]
  bikek_test_labels5<- bikek_temp[740:839,"totalQ3"]
  bikek_train_labels6 <- bikek_temp[1:739,"totalRQ3"]
  bikek_test_labels6<- bikek_temp[740:839,"totalRQ3"]
  
  #bikek_train_labels
  #library(class)
  
{
  HL = 0; RHL = 0; Q = 0; RQ = 0; Q3 = 0; RQ3 = 0;
  for(i in 1:10){  
  bikek_test_pred1 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels1, k)
  bikek_test_pred2 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels2, k)
  bikek_test_pred3 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels3, k)
  bikek_test_pred4 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels4, k)
  bikek_test_pred5 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels5, k)
  bikek_test_pred6 <- knn(train = bikek_train, test = bikek_test, cl=bikek_train_labels6, k)
  
  #install.packages("gmodels")
  #library(gmodels)
  
  
  highlow<-CrossTable(x=bikek_test_labels1, y=bikek_test_pred1, prop.chisq = FALSE)
  Rhighlow<-CrossTable(x=bikek_test_labels2, y=bikek_test_pred2, prop.chisq = FALSE)
  totalQ<-CrossTable(x=bikek_test_labels3, y=bikek_test_pred3, prop.chisq = FALSE)
  totalRQ<-CrossTable(x=bikek_test_labels4, y=bikek_test_pred4, prop.chisq = FALSE)
  totalQ3<-CrossTable(x=bikek_test_labels5, y=bikek_test_pred5, prop.chisq = FALSE)
  totalRQ3<-CrossTable(x=bikek_test_labels6, y=bikek_test_pred6, prop.chisq = FALSE)
  
  HL<-HL + highlow[[4]][[1,1]]+highlow[[4]][[2,2]]
  RHL<-RHL + Rhighlow[[4]][[1,1]]+highlow[[4]][[2,2]]
  Q<-Q + (totalQ[[4]][[1,1]]+totalQ[[4]][[4,4]])/(totalQ[[4]][[1,1]]+totalQ[[4]][[4,4]]+totalQ[[4]][[1,4]]+totalQ[[4]][[4,1]])
  RQ<-RQ + (totalRQ[[4]][[1,1]]+totalRQ[[4]][[4,4]])/(totalRQ[[4]][[1,1]]+totalRQ[[4]][[4,4]]+totalRQ[[4]][[1,4]]+totalRQ[[4]][[4,1]])
  Q3<-Q3 + (totalQ3[[4]][[1,1]]+totalQ3[[4]][[3,3]])/(totalQ3[[4]][[1,1]]+totalQ3[[4]][[3,3]]+totalQ3[[4]][[1,3]]+totalQ3[[4]][[3,1]])
  RQ3<-RQ3 + (totalRQ3[[4]][[1,1]]+totalRQ3[[4]][[3,3]])/(totalRQ3[[4]][[1,1]]+totalRQ3[[4]][[3,3]]+totalRQ3[[4]][[1,3]]+totalRQ3[[4]][[3,1]])
  }
  HL <- HL/10; RHL <- RHL/10; Q <- Q/10; RQ <- RQ/10; Q3 <- Q3/10; RQ3 <- RQ3/10;
  
  IV_name<- paste(IV,collapse=",")
  
  
  print(paste0("k=",k,"에서 total 상하 정확도는 ", round(HL*100,2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total rate 상하 정확도는 ",round(RHL*100.2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total 상 하위 25% 정확도는 ", round(Q*100,2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total rate 상 하위 25% 정확도는 ", round(RQ*100,2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total 상 하위 33% 정확도는 ", round(Q3*100,2),"% 입니다. 사용 변수 :", IV_name))
  print(paste0("k=",k,"에서 total rate 상 하위 33% 정확도는 ", round(RQ3*100,2),"% 입니다.  사용 변수 :", IV_name))
  
}
  