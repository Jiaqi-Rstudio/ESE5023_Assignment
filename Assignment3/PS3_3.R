vp_Data <- read.delim("problem3.txt", head=TRUE)
PVgroup <- vp_Data$Pregnant.vegetarians
PNVgroup <- vp_Data$Pregnant.nonvegetarians
mean(vp_Data$Pregnant.vegetarians, na.rm=T)
mean(vp_Data$Pregnant.nonvegetarians, na.rm=T)
hist(PVgroup)
hist(PNVgroup)
boxplot(cbind(PVgroup, PNVgroup))
t.test(PVgroup, PNVgroup)
