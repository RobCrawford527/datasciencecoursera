## This is a markdown file

ozone <- read.csv("hw1_data.csv")

colnames(ozone)
ozone[1:2,]
nrow(ozone)
ozone[c(nrow(ozone)-1,nrow(ozone)),]
ozone[47,]
sum(is.na(ozone$Ozone))
mean(ozone$Ozone,na.rm=TRUE)

ozone_1 <- ozone[ozone$Ozone>31 & !is.na(ozone$Ozone),]
ozone_1
ozone_2 <- ozone_1[ozone_1$Temp>90 & !is.na(ozone_1$Temp),]
ozone_2
mean(ozone_2$Solar.R,na.rm=TRUE)

mean(ozone$Temp[ozone$Month==6])
max(ozone$Ozone[ozone$Month==5],na.rm=TRUE)





