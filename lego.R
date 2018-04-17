bikey<-read.csv("bikey.csv")
bmonth8<-read.csv("bmonth8.csv")

install.packages("geosphere")
library(geosphere)
mat <- distm(bikey[,c('long','lat')], bmonth8[,c('long','lat')], fun=distVincentyEllipsoid)
mat <- as.data.frame(mat)
head(mat)
