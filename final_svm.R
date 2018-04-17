###########################################
# 1. svm : totalhl
bikes1 <- read.csv("bikey_svm_totalhl.csv")
#str(bikes1)

bikes1_train <- bikes1[1:700,]
bikes1_test <- bikes1[701:839,]

#install.packages("kernlab")
library(kernlab)
bikes1_classifier <- ksvm(totalhighlow ~., data=bikes1_train, kernel="vanilladot")
bikes1_classifier #ERROR : 30%

bikes1_predictions <- predict(bikes1_classifier, bikes1_test)
#head(bikes1_predictions)
table(bikes1_predictions, bikes1_test$totalhighlow)

agreement1 <- bikes1_predictions == bikes1_test$totalhighlow
table(agreement1)
prop.table(table(agreement1)) #52.5%

# bikes1_classifier_rbf <- ksvm(totalhighlow ~ ., data=bikes1_train, kernel="rbfdot")
# bikes1_predictions_rbf <- predict(bikes1_classifier_rbf, bikes1_test)
# 
# agreement_rbf <- bikes1_predictions_rbf == bikes1_test$totalhighlow
# table(agreement_rbf)
# prop.table(table(agreement_rbf))
###########################################


###########################################
# 2. svm : totalQ
bikes2 <- read.csv("bikey_svm_totalQ.csv")
bikes2_train <- bikes2[1:700,]
bikes2_test <- bikes2[701:839,]

#install.packages("kernlab")
library(kernlab)
bikes2_classifier <- ksvm(totalQ ~., data=bikes2_train, kernel="vanilladot")
bikes2_classifier #ERROR : 55%

bikes2_predictions <- predict(bikes2_classifier, bikes2_test)
#head(bikes2_predictions)
table(bikes2_predictions, bikes2_test$totalQ)

agreement2 <- bikes2_predictions == bikes2_test$totalQ
table(agreement2)
prop.table(table(agreement2)) #22.3%
###########################################


###########################################
# 3. svm : totalQ3
bikes3 <- read.csv("bikey_svm_totalQ3.csv")
bikes3_train <- bikes3[1:700,]
bikes3_test <- bikes3[701:839,]

#install.packages("kernlab")
library(kernlab)
bikes3_classifier <- ksvm(totalQ3 ~., data=bikes3_train, kernel="vanilladot")
bikes3_classifier #ERROR : 49%

bikes3_predictions <- predict(bikes3_classifier, bikes3_test)
#head(bikes3_predictions)
table(bikes3_predictions, bikes3_test$totalQ3)

agreement3 <- bikes3_predictions == bikes3_test$totalQ3
table(agreement3)
prop.table(table(agreement3)) #32.3%
###########################################


###########################################
# 4.svm : totalRhl
bikes4 <- read.csv("bikey_svm_totalRhl.csv")
bikes4_train <- bikes4[1:700,]
bikes4_test <- bikes4[701:839,]

#install.packages("kernlab")
#library(kernlab)
bikes4_classifier <- ksvm(totalRhighlow ~., data=bikes4_train, kernel="vanilladot")
bikes4_classifier #ERROR : 29%

bikes4_predictions <- predict(bikes4_classifier, bikes4_test)
#head(bikes4_predictions)
table(bikes4_predictions, bikes4_test$totalRhighlow)

agreement4 <- bikes4_predictions == bikes4_test$totalRhighlow
table(agreement4)
prop.table(table(agreement4)) #51%
###########################################


###########################################
# 5. svm : totalRQ
bikes5 <- read.csv("bikey_svm_totalRQ.csv")
bikes5_train <- bikes5[1:700,]
bikes5_test <- bikes5[701:839,]

#install.packages("kernlab")
#library(kernlab)
bikes5_classifier <- ksvm(totalRQ ~., data=bikes5_train, kernel="vanilladot")
bikes5_classifier #ERROR : 57%

bikes5_predictions <- predict(bikes5_classifier, bikes5_test)
#head(bikes5_predictions)
table(bikes5_predictions, bikes5_test$totalRQ)

agreement5 <- bikes5_predictions == bikes5_test$totalRQ
table(agreement5)
prop.table(table(agreement5)) #26.6%
###########################################


###########################################
# 6. svm : totalRQ3
bikes6 <- read.csv("bikey_svm_totalRQ3.csv")
bikes6_train <- bikes6[1:700,]
bikes6_test <- bikes6[701:839,]

#install.packages("kernlab")
#library(kernlab)
bikes6_classifier <- ksvm(totalRQ3 ~., data=bikes6_train, kernel="vanilladot")
bikes6_classifier #ERROR : 46%

bikes6_predictions <- predict(bikes6_classifier, bikes6_test)
#head(bikes6_predictions)
table(bikes6_predictions, bikes6_test$totalRQ3)

agreement6 <- bikes6_predictions == bikes6_test$totalRQ3
table(agreement6)
prop.table(table(agreement6)) #31.6%
###########################################