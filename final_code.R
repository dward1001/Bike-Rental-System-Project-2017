#linear regression with month8 data
bike1 <- read.csv("bikey_25_A.csv", stringsAsFactors = FALSE)
#summary(bike1$total)
#hist(bike1$total)

cor(bike1[c("subdist","busdist","subtotal","bustotal","dens","total")])
pairs(bike1[c("subdist","busdist","subtotal","bustotal","dens","total")])
#pairs(bike1[])
pairs(bike1[c("dens","a2030","a4050","female","male","fpop","school_park","total")])
pairs(bike1[c("bike_len", "bikesum","total")])
#pairs(bike1[c("subdist","busdist","total")])
#pairs(bike1[c("subtotal","bustotal","total")])

install.packages("psych")
library(psych)
pairs.panels(bike1[c("subdist","busdist","subtotal","bustotal","dens","total")])
pairs.panels(bike1[c("dens","a2030","a4050","female","male","fpop","school_park","total")])

b1_model <- lm(total ~ .,data = bike1)
b1_model
summary(b1_model)

b2_model <- lm(total ~ subdist+subtotal+bustotal+school, data=bike1)
b2_model
summary(b2_model)

install.packages("olsrr")
library(olsrr)
ols_all_subset(b1_model)
k <- ols_best_subset(b1_model)
k
plot(k)

reg <- function(y,x){
  x <- as.matrix(x)
  x <- cbind(Intercept=1,x)
  solve(t(x)%*%x, tol = 1e-42) %*% t(x) %*% y
}
str(bike1)

reg(y=bike1$total, x=bike1[1:7])
#reg(y=bike1$totalrate, x=bike1[1:5])
pairs.panels(bike1[c("subdist","busdist","subtotal","bustotal","density","totalrate")])

bike1 <- read.csv("bike_25_linear.csv", stringsAsFactors = FALSE)
bike1 <- bike1[order(runif(825)),]
bike1_train <- bike1[1:700,]
bike1_test <- bike1[701:825,]

install.packages("rpart")
library(rpart)
#m.rpart <- rpart(total ~ ., data = bike1)
#m.rpart <- rpart(total ~ subdist + subtotal + busdist + bustotal + dens + fpop + bike_len + bikesum, data = bike1)
m.rpart <- rpart(total ~ subdist + subtotal + bustotal + fpop + school_park + bike_len + bikesum, data = bike1)
m.rpart

p.rpart <- predict(m.rpart, bike1_test)
summary(p.rpart)
summary(bike1_test$total)
cor(p.rpart, bike1_test$total) #51%

#after deleting extreme values(total : 1-5, >8000)
m.rpart1 <- rpart(totalrate ~ subdist + subtotal + bustotal + fpop + school_park + bike_len + bikesum, data = bike1)
p.rpart1 <- predict(m.rpart1, bike1_test)
summary(p.rpart1)
summary(bike1_test$totalrate)
cor(p.rpart1, bike1_test$totalrate) #57%

#install.packages("rpart.plot")
#library(rpart.plot)
#rpart.plot(m.rpart, digits = 3)
