
#knn le-go HL & Quat
#correlation between the location and totalR(HL) -> 0.667
#correlation between the distance from transportations and totalR(HL) -> 0.57
bikey2 <- read.csv("bikey2_temp.csv", stringsAsFactors = FALSE)
table(bikey2$totalRhighlow)
bikey2$totalRhighlow <- factor(bikey2$totalRhighlow)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
bikey2_temp <- bikey2[order(runif(443)),]
#location(longitute, latitude $ divisioncode)
bikey2_n <- as.data.frame(lapply(bikey2_temp[1:3],normalize))
#distance(subway & bus)
#bikey2_n <- as.data.frame(lapply(bikey2_temp[6:7],normalize))
str(bikey2_n)

bikey2_train <- bikey2_n[1:350,]
bikey2_test <- bikey2_n[351:443,]

bikey2_train_labels <- bikey2_temp[1:350,4]
bikey2_test_labels <- bikey2_temp[351:443,4]
bikey2_train_labels

library(class)
bikey2_test_pred <- knn(train = bikey2_train, test = bikey2_test, cl=bikey2_train_labels, k=21)
bikey2_test_pred

library(gmodels)
CrossTable(x=bikey2_test_labels, y=bikey2_test_pred, prop.chisq = FALSE)

#correlation between the location and totalR(Q) -> 0.764
#correlation between the distance from transportations and totalR(Q) -> 0.653
bikey3 <- read.csv("bikey2_temp.csv", stringsAsFactors = FALSE)
table(bikey3$totalRQ)
bikey3$totalRQ <- factor(bikey3$totalRQ)

bikey3_temp <- bikey3[order(runif(222)),]
#location(longitute, latitude $ divisioncode)
bikey3_n <- as.data.frame(lapply(bikey3_temp[1:3],normalize))
#distance(subway & bus)
#bikey3_n <- as.data.frame(lapply(bikey3_temp[6:7],normalize))
str(bikey3_n)

bikey3_train <- bikey3_n[1:150,]
bikey3_test <- bikey3_n[151:222,]

bikey3_train_labels <- bikey3_temp[1:150,5]
bikey3_test_labels <- bikey3_temp[151:222,5]
bikey3_train_labels

library(class)
bikey3_test_pred <- knn(train = bikey3_train, test = bikey3_test, cl=bikey3_train_labels, k=14)
bikey3_test_pred

library(gmodels)
CrossTable(x=bikey3_test_labels, y=bikey3_test_pred, prop.chisq = FALSE)


#linear regression try try
bikey4 <- read.csv("bikey2_temp2.csv", stringsAsFactors = FALSE)

summary(bikey4$totalrate)
hist(bikey4$totalrate)

cor(bikey4[c("lat","long","divisioncode","subdist","busdis","totalrate")])
pairs(bikey4[c("lat","long","divisioncode","subdist","busdis","totalrate")])

install.packages("psych")
library(psych)
pairs.panels(bikey4[c("idnum","setnum","lat","long","divisioncode","subdist","busdis","totalrate")])

b_model <- lm(totalrate ~ ., data = bikey4)
install.packages("olsrr")
library(olsrr)
ols_all_subset(b_model)
k <- ols_best_subset(b_model)
k
plot(k)

reg <- function(y,x){
  x <- as.matrix(x)
  x <- cbind(Intercept=1,x)
  solve(t(x)%*%x) %*% t(x) %*% y
}
str(bikey4)

reg(y=bikey4$totalrate, x=bikey4[1:7])
reg(y=bikey4$total, x=bikey4[1:7])
pairs.panels(bikey4[c("idnum","setnum","lat","long","divisioncode","subdist","busdis","total")])
pairs.panels(bikey4[c("subdist","busdis","totalrate")])

bikey4_n <- read.csv("bikey4_temp.csv", stringsAsFactors = FALSE)
library(psych)
pairs.panels(bikey4_n[c("subdist","busdist","popdens","subtotal","bustotal","GUnum","total")])

b2_model <- lm(total ~ ., data = bikey4_n)
b3_model <- lm(total ~ busdist + bustotal + subdist + subtotal, data = bikey4_n)
reg(y=bikey4_n$total, x=bikey4_n[1:6])
b2_model
b3_model
summary(b2_model)
summary(b3_model)
pairs.panels(bikey4_n[c("busdist","bustotal","subdist","subtotal","total")])


#again knn with new data

bike_k <- read.csv("bike_knn.csv", stringsAsFactors = FALSE)
bike_k$totalhighlowA <- factor(bike_k$totalhighlowA)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

bikek_temp <- bike_k[order(runif(115)),]
bikek_n <- as.data.frame(lapply(bikek_temp[1:6],normalize))

bikek_train <- bikek_n[1:100,]
bikek_test <- bikek_n[101:115,]

bikek_train_labels <- bikek_temp[1:100,7]
bikek_test_labels <- bikek_temp[101:115,7]
#bikek_train_labels

library(class)
bikek_test_pred <- knn(train = bikek_train, test = bikek_test, 
                       cl=bikek_train_labels, k=10)
bikek_test_pred

library(gmodels)
CrossTable(x=bikek_test_labels, y=bikek_test_pred, prop.chisq = FALSE)


#linear regression
bike_l <- read.csv("bike_linear.csv", stringsAsFactors = FALSE)

summary(bike_l$total)
hist(bike_l$total)

cor(bike_l[c("subdist","busdist","GUnum","subtotal","bustotal","density","total")])
pairs(bike_l[c("subdist","busdist","GUnum","subtotal","bustotal","density","total")])
pairs(bike_l[c("subdist","busdist","total")])
pairs(bike_l[c("subtotal","bustotal","total")])

install.packages("psych")
library(psych)
pairs.panels(bike_l[c("subdist","busdist","GUnum","subtotal","bustotal","density","total")])

bl_model <- lm(total ~ subdist + subtotal + busdist + bustotal + density + GUnum, data = bike_l)
bl_model
summary(bl_model)

bike_l <- read.csv("bike_linear.csv", stringsAsFactors = FALSE)
bl2_model <- lm(totalrate ~ subdist + subtotal + 
                  busdist + bustotal + density, data = bike_l)
bl2_model
summary(bl2_model)

install.packages("olsrr")
library(olsrr)
ols_all_subset(bl_model)
k <- ols_best_subset(bl_model)
k
plot(k)

reg <- function(y,x){
  x <- as.matrix(x)
  x <- cbind(Intercept=1,x)
  solve(t(x)%*%x) %*% t(x) %*% y
}
str(bikey4)

reg(y=bike_l$total, x=bike_l[1:5])
reg(y=bike_l$totalrate, x=bike_l[1:5])
pairs.panels(bike_l[c("subdist","busdist","subtotal","bustotal","density","totalrate")])

bike_l <- read.csv("bike_linear.csv", stringsAsFactors = FALSE)
bikel_train <- bike_l[1:100,]
bikel_test <- bike_l[101:115,]

install.packages("rpart")
library(rpart)
#m.rpart <- rpart(total ~ busdist + bustotal + density, data = bike_l)
m.rpart <- rpart(totalrate ~ subdist + subtotal + busdist + bustotal + density, data = bike_l)
m.rpart

p.rpart <- predict(m.rpart, bikel_test)
summary(p.rpart)
summary(bikel_test$totalrate)
cor(p.rpart, bikel_test$totalrate)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
#rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)


#Decision Tree
bike_d <- read.csv("bike_DT.csv", stringsAsFactors = FALSE)
install.packages("caret")
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages('colorspace', dependencies = TRUE)
library("caret")
set.seed(115)
biked_train <- createDataPartition(y=bike_d$totalrate, p=1.0, list=FALSE)
bike_rand <- bike_d[order(runif(115)),]
train <- bike_rand[biked_train, ]

install.packages("party")
library(party)
partymod <- ctree(total ~ density + subdist + subtotal + busdist + bustotal, data=train)
plot(partymod, type="simple")
























