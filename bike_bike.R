bike <- read.csv("bike.csv", stringsAsFactors = FALSE)
bike
str(bike)
summary(bike$rentrate)
summary(bike$returnrate)
hist(bike$rentrate)
hist(bike$returnrate)

set.seed(1000)
order(runif(443))
bike_rand <- bike[order(runif(443)),]
bike_rand[1,]
bike_rand[2,]

head(bike$idnum)
head(bike_rand$idnum)
str(bike_rand$구분)

bike_temp <- bike_rand[,3:4]
bike_temp <- cbind(bike_rand[10], bike_temp)
str(bike_temp)

bike_train <- bike_temp[1:350,]
bike_test <- bike_temp[351:443,]
str(bike_train$setnum)

bike_train$setnum <- as.factor(bike_train$setnum)
str(bike_train$setnum)

prop.table(table(bike_train$setnum))
prop.table(table(bike_test$setnum))

install.packages("class")
library(class)

sqrt(443)
bike_train_labels <- bike_temp[1:350,1]
bike_train_labels

test_pred <- knn(train=bike_train, test=bike_test,cl=bike_train_labels, k=10)
bike_test_labels <- bike_temp[351:443,1]

install.packages("gmodels")
library(gmodels)
CrossTable(x=bike_test_labels, y=test_pred, prop.chisq=FALSE)



#linear regression le-go
bike <- read.csv("bikey.csv", stringsAsFactors = FALSE)

summary(bike$totalrate)
hist(bike$totalrate)
table(bike$구분)

cor(bike[c("setnum","lat","long","subdist","totalrate")])
pairs(bike[c("idnum","lat","long","subdist","totalrate")])
pairs(bike[c("lat","long","subdist","rentrate","returnrate")])

install.packages("psych")
library(psych)
pairs.panels(bike[c("idnum","lat","long","subdist","total")])
pairs.panels(bike[c("lat","long","subdist","rent","return")])

#first : 0.1345
ins_model1 <- lm(total ~ idnum + lat + long + setnum, data = bike)
ins_model1
summary(ins_model1)

#second : 0.2298
ins_model2 <- lm(totalrate ~ idnum + lat + long + setnum, data = bike)
ins_model2
summary(ins_model2)

#third : 0.2068
ins_model3 <- lm(totalrate ~ idnum + setnum, data = bike)
ins_model3
summary(ins_model3)

#fourth : 0.2341
ins_model4 <- lm(totalrate ~ idnum + setnum + subdist, data = bike)
ins_model4
summary(ins_model4)

#fifth : 0.2563
ins_model5 <- lm(totalrate ~ setnum + idnum + subdist + lat + long, data = bike)
ins_model5
summary(ins_model5)

#sixth : 0.2566
ins_model6 <- lm(totalrate ~ setnum + idnum + subdist + lat*long, data = bike)
ins_model6
summary(ins_model6)



#decision_tree
bike2 <- read.csv("bikey_temp.csv", stringsAsFactors = FALSE)
install.packages("caret")
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages('colorspace', dependencies = TRUE)
library("caret")
set.seed(443)
bike2_train <- createDataPartition(y=bike2$total, p=1.0, list=FALSE)
bike_rand <- bike2[order(runif(443)),]
train <- bike_rand[bike2_train, ]

install.packages("party")
library(party)
partymod <- ctree(total~., data=train)
plot(partymod, type="simple")

bike3 <- read.csv("bikey_temp.csv", stringsAsFactors = FALSE)
bike3_train <- createDataPartition(y=bike3$totalrate, p=1.0, list=FALSE)
bike3_rand <- bike3[order(runif(443)),]
train3 <- bike3_rand[bike3_train, ]

partymod2 <- ctree(totalrate~., data=train3)
plot(partymod2, type="simple")