########
names <- read.csv("bike_rent_201708.csv", stringsAsFactors = FALSE)
idsep<-strsplit(names$first, "[.]")

#idnum
idnum<-unlist(sapply(idsep,"[",1))
str(idnum)
#head(idnum)
#length(idnum)
#table(is.na(idnum))

#idname
idname<-unlist(sapply(idsep,"[",2))
str(idname)
table(is.na(idname))
idname[which(is.na(idname))] <- idnum[which(is.na(idname))]
which(is.na(idname))

names$first <- idname
names$first


idsep2<-strsplit(names$second, "[.]")
idnum2 <- unlist(sapply(idsep2,"[",1))
idname2<-unlist(sapply(idsep2,"[",2))
idname2[which(is.na(idname2))] <- idnum2[which(is.na(idname2))]
which(is.na(idname2))

names$second <- idname2
names$second

names$trans <- paste(idname,"/",idname2)
names$trans

write.csv(names, file="bike_rent_201708_new.csv")


install.packages("arules")
library(arules)
#prac <- read.csv("bike_rent_201708_new.csv", stringsAsFactors = FALSE)
prac <- read.transactions("bike_rent_201708_new.csv",sep='/',cols = c(1),rm.duplicates = FALSE, quote="")
summary(prac)
head(prac)
inspect(prac[1:20])
itemFrequencyPlot(prac, topN=20)

pracrules <- apriori(prac, parameter=list(support=0, confidence=0))
pracrules
summary(pracrules)
inspect(pracrules[1:10])




