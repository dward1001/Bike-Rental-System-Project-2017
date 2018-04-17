bike1 <- read.csv("bikey_26_B.csv", stringsAsFactors = FALSE)
bike1 <- bike1[order(runif(858)),]
bike1_train <- bike1[1:700,]
bike1_test <- bike1[701:858,]

install.packages("rpart")
library(rpart)
#m.rpart <- rpart(total ~ ., data = bike1)
#m.rpart <- rpart(total ~ subdist + subtotal + busdist + bustotal + dens + fpop + bike_len + bikesum, data = bike1)
m.rpart <- rpart(total ~ subdist + subtotal + busdist + bustotal + dens + fpop + a2030 + a4050 + female + male + suit + casual + school + park + bike_len + bikesum, data = bike1)
m.rpart

p.rpart <- predict(m.rpart, bike1_test)
summary(p.rpart)
summary(bike1_test$total)
cor(p.rpart, bike1_test$total) #48%

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)


#after deleting extreme values(total : 1-5, >8000)
bike1 <- read.csv("bikey_26_B.csv", stringsAsFactors = FALSE)
bike1 <- bike1[order(runif(843)),]
bike1_train <- bike1[1:700,]
bike1_test <- bike1[701:843,]

m.rpart1 <- rpart(totalrate ~ subdist + subtotal + busdist + bustotal + dens + fpop + a2030 + a4050 + female + male + suit + casual + school + park + bike_len + bikesum, data = bike1)
p.rpart1 <- predict(m.rpart1, bike1_test)
summary(p.rpart1)
summary(bike1_test$totalrate)
cor(p.rpart1, bike1_test$totalrate) #54%

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart1, digits = 3)