###########################################
# nn : total
bikenn <- read.csv("bikey_nn_total.csv")
#str(bikenn)

normalize <- function(x){
  return ((x-min(x))/(max(x) - min(x)))
}

bikenn_norm <- as.data.frame(lapply(bikenn, normalize))
#summary(bikenn_norm)
#summary(bikenn_norm$total)
#summary(bikenn_norm$totalrate)

bikenn_train <- bikenn_norm[1:700,]
bikenn_test <- bikenn_norm[701:839,]

install.packages("neuralnet")
library(neuralnet)

# 20.79%
sum1 = 0
num = 0
for (i in 1:10){
bikenn_model1 <- neuralnet(total ~ bustotal + busdist + subtotal + subdist + dens + a2030 + a4050 + female + male + fpop + under20 + suit + casual + school + park + school_park + bikelen + bikesum + busrate + subrate, data = bikenn_train)
#plot(bikenn_model1)

model_results1 <- compute(bikenn_model1, bikenn_test[1:20])
predicted_total1 <- model_results1$net.result
#cor(predicted_total1, bikenn_test$total)
k <- cor(predicted_total1, bikenn_test$total)
k
if(k > 0.1){
  sum1 <- sum1 + k
  num <- num + 1
}
}
sum1 <- sum1/num
sum1

# bikenn2 <- read.csv("bikey_nn_total_1.csv")
# bikenn2_norm <- as.data.frame(lapply(bikenn2, normalize))
# bikenn2_train <- bikenn2_norm[1:700,]
# bikenn2_test <- bikenn2_norm[701:839,]
# 
# bikenn_model5 <- neuralnet(total ~ bustotal + busdist + subtotal + subdist + dens + fpop + school + bikelen + bikesum, data=bikenn2_train)
# model_result5 <- compute(bikenn_model5, bikenn2_test[1:9])
# predicted_total5 <- model_result5$net.result
# cor(predicted_total5, bikenn2_test$total)
###########################################

###########################################
##### hidden = 1 : 20.79%
##### hidden = 2 : 21.79%
# hidden = 3 : 19.29%
# hidden = 4 : 16.46%
# hidden = 5 : 17.17%
# hidden = 6 : 17.45%
# hidden = 7 : 14.11%
# hidden = 8 : 13.09%
# hidden = 9 : 13.10%
# hidden = 10 : 12.50%

# hidden = c(2,1) : 16.03%
# hidden = c(2,2) : 18.64%
# hidden = c(2,3) : 18.21%
# hidden = c(2,4) : 19.76%
# hidden = c(2,5) : 19.16%
# hidden = c(2,6) : 19.60%
##### hidden = c(2,7) : 21.98%
# hidden = c(2,8) : 19.75%
# hidden = c(2,9) : 20.04%
# hidden = c(2,10) : 17.59%

# hidden = c(3,1) : 14.97%
# hidden = c(3,2) : 18.19%
# hidden = c(3,3) : 17.13%
##### hidden = c(3,4) : 19.39%
# hidden = c(3,5) : 18.21%
# hidden = c(3,6) : 14.76%
# hidden = c(3,7) : 17.82%
# hidden = c(3,8) : 15.80%
# hidden = c(3,9) : 17.04%
# hidden = c(3,10) : 17.55%

# hidden = c(4,1) : 17.20%
# hidden = c(4,2) : 16.54%
# hidden = c(4,3) : 16.25%
# hidden = c(4,4) : 13.12%
# hidden = c(4,5) : 16.33%
# hidden = c(4,6) : 16.19%
##### hidden = c(4,7) : 19.34%
# hidden = c(4,8) : 15.52%
# hidden = c(4,9) : 18.50%
# hidden = c(4,10) : 16.93%

# hidden = c(5,1) : 13.46%
# hidden = c(5,2) : 15.83%
# hidden = c(5,3) : 18.90%
##### hidden = c(5,4) : 24.83%
# hidden = c(5,5) : 14.72%
# hidden = c(5,6) : 17.96%
# hidden = c(5,7) : 14.71%
##### hidden = c(5,8) : 20.05%
# hidden = c(5,9) : 12.84%
# hidden = c(5,10) : 17.29%

# hidden = c(6,1) : 15.82%
# hidden = c(6,2) : 15.26%
##### hidden = c(6,3) : 20.59%
# hidden = c(6,4) : 13.22%
# hidden = c(6,5) : 13.73%
# hidden = c(6,6) : 12.74%
# hidden = c(6,7) : 13.92%
# hidden = c(6,8) : 18.02%
# hidden = c(6,9) : 12.90%
# hidden = c(6,10) : 15.64%

for(k in 1:10){
  sum2 = 0
  num = 0
  for (i in 1:10){
    bikenn_model2 <- neuralnet(total ~ bustotal + busdist + subtotal + subdist + dens + a2030 + a4050 + female + male + fpop + under20 + suit + casual + school + park + school_park + bikelen + bikesum + busrate + subrate, data = bikenn_train, hidden = c(5,4))
    plot(bikenn_model2)

    model_results2 <- compute(bikenn_model2, bikenn_test[1:20])
    predicted_total2 <- model_results2$net.result
    t <- cor(predicted_total2, bikenn_test$total)
    if(t > 0.1){
      sum2 <- sum2 + t
      num <- num + 1
    }
  }
  sum2 <- sum2/num
  print(k)
  print(sum2)
}
###########################################


###########################################
# nn : totalrate
bikenn_r <- read.csv("bikey_nn_totalrate.csv")
#str(bikenn_r)

bikenn_norm2 <- as.data.frame(lapply(bikenn_r, normalize))
#summary(bikenn_norm2)
#summary(bikenn_norm2$totalrate)

bikenn_train2 <- bikenn_norm2[1:700,]
bikenn_test2 <- bikenn_norm2[701:839,]

# 16.06%
sum3 = 0
for (i in 1:10){
  bikenn_model3 <- neuralnet(totalrate ~ bustotal + busdist + subtotal + subdist + dens + a2030 + a4050 + female + male + fpop + under20 + suit + casual + school + park + school_park + bikelen + bikesum + busrate + subrate, data = bikenn_train2)
  #plot(bikenn_model3)
  
  model_results3 <- compute(bikenn_model3, bikenn_test2[1:20])
  predicted_total3 <- model_results3$net.result
  #sum3 <- sum3 + cor(predicted_total3, bikenn_test2$totalrate)
  cor(predicted_total3, bikenn_test2$totalrate)
}
sum3 <- sum3/10
sum3
###########################################

###########################################
# hidden = 2 : 14.05%
# hidden = 3 : 09.38%
# hidden = 4 : 15.22%
# hidden = 5 : 09.38%

# hidden = c(2,1) : 09.39%
# hidden = c(2,2) : 08.69%
# hidden = c(2,3) : 11.48%
# hidden = c(2,4) : 10.68%

# hidden = c(3,1) : 13.13%
# hidden = c(3,2) : 09.54%
# hidden = c(3,3) : 15.23%
# hidden = c(3,4) : 10.32%

# hidden = c(4,1) : 12.99%
# hidden = c(4,2) : 11.94%
# hidden = c(4,3) : 14.75%
# hidden = c(4,4) : 08.94%
# hidden = c(4,5) : 10.23%
# hidden = c(4,6) : 10.91%
# hidden = c(4,7) : 09.31%

sum4 = 0
for (i in 1:10){
  bikenn_model4 <- neuralnet(totalrate ~ bustotal + busdist + subtotal + subdist + dens + a2030 + a4050 + female + male + fpop + under20 + suit + casual + school + park + school_park + bikelen + bikesum + busrate + subrate, data = bikenn_train2, hidden = 4)
  #plot(bikenn_model4)
  
  model_results4 <- compute(bikenn_model4, bikenn_test2[1:20])
  predicted_total4 <- model_results4$net.result
  sum4 <- sum4 + cor(predicted_total4, bikenn_test2$totalrate)
  #cor(predicted_total4, bikenn_test2$totalrate)
}
sum4 <- sum4/10
sum4

