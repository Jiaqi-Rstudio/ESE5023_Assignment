setwd("E:/课程/ESE5023_Assignments/Final Project")
getwd()
#Read the data
Total_Data <- read.csv("Data.csv", encoding="UTF-8")
names(Total_Data)
#install.packages("hydrogeo")
#install.packages("tidyr")
#install.packages("dplyr")
library(hydrogeo)
library(tidyr)
library(dplyr)
#Piper for total data
Piper_data <- Total_Data %>%
  select(X.U.FEFF.经度, 纬度, 样品野外编号, Ca2., Mg2., K., Na., Cl., SO42., HCO3., CO32.)
Piper_data <- na.omit(Piper_data)
Piper_l <- list( Ca = Piper_data$Ca2.,
           Mg = Piper_data$Mg2.,
           Na = Piper_data$Na.,
           K = Piper_data$K.,
           Cl = Piper_data$Cl.,
           SO4 = Piper_data$SO42.,
           CO3 =Piper_data$CO32.,
           HCO3 = Piper_data$HCO3.,
           IDs = Piper_data$样品野外编号)
Piper_d <- toPercent(Piper_l)
# check, should add up to 100%
z <- as.data.frame(Piper_d)
for(i in 1:length(z[[1]])) { print(sum(z[i,5:8])) }
for(i in 1:length(z[[1]])) { print(sum(z[i,1:4])) }
lp <- piper(Piper_d)
plot(lp, main="Piper-Hill Diagram of Groundawater")
#Piper for northern city
LYG_data <- Piper_data %>%
  mutate(city=substr(样品野外编号, 1,3)) %>%
  filter(city=="LYG")
Piper_LYG <- list( Ca = LYG_data$Ca2.,
                 Mg = LYG_data$Mg2.,
                 Na = LYG_data$Na.,
                 K = LYG_data$K.,
                 Cl = LYG_data$Cl.,
                 SO4 = LYG_data$SO42.,
                 CO3 =LYG_data$CO32.,
                 HCO3 = LYG_data$HCO3.,
                 IDs = LYG_data$样品野外编号)
Piper_LYG_d <- toPercent(Piper_LYG)
# check, should add up to 100%
LYG_z <- as.data.frame(Piper_LYG_d)
for(i in 1:length(LYG_z[[1]])) { print(sum(LYG_z[i,5:8])) }
for(i in 1:length(LYG_z[[1]])) { print(sum(LYG_z[i,1:4])) }
LYG_lp <- piper(Piper_LYG_d)
plot( LYG_lp, main="Piper-Hill Diagram of Groundawater in the Northern Area", cex=1.4, cex.axis=1.5)
#Piper for SOUthern city
NT_data <- Piper_data %>%
  mutate(city=substr(样品野外编号, 1,2)) %>%
  filter(city=="NT")
Piper_NT <- list( Ca = NT_data$Ca2.,
                   Mg = NT_data$Mg2.,
                   Na = NT_data$Na.,
                   K = NT_data$K.,
                   Cl = NT_data$Cl.,
                   SO4 = NT_data$SO42.,
                   CO3 =NT_data$CO32.,
                   HCO3 = NT_data$HCO3.,
                   IDs = NT_data$样品野外编号)
Piper_NT_d <- toPercent(Piper_NT)
# check, should add up to 100%
NT_z <- as.data.frame(Piper_NT_d)
for(i in 1:length(NT_z[[1]])) { print(sum(NT_z[i,5:8])) }
for(i in 1:length(NT_z[[1]])) { print(sum(NT_z[i,1:4])) }
NT_lp <- piper(Piper_NT_d)
plot( NT_lp, main="Piper-Hill Diagram of Groundawater in the Southern Area", cex=1.4, cex.axis=2)
#苏林分类
Sulin_data <- Piper_data %>% 
  select(X.U.FEFF.经度, 纬度, 样品野外编号, Mg2., Na., Cl., SO42.) %>%                           
  mutate(Meq_Na=Na./23*1, Meq_Cl=Cl./35*1, Meq_Mg = Mg2./24*2, Meq_SO4 = SO42./96*2) %>%
  mutate(Na_Cl = Meq_Na/Meq_Cl, diff_Na_Cl = Meq_Na-Meq_Cl, diff_Cl_Na = Meq_Cl-Meq_Na) %>%
  mutate(ratio1 = diff_Na_Cl/Meq_SO4, ratio2 = diff_Cl_Na/Meq_Mg) %>%
  mutate(type=ifelse(Na_Cl>1 & ratio1 <1,  "sodium sulfate", NA)) %>%
  mutate(type=ifelse(Na_Cl>1 & ratio1 >1,  "sodium bicarbonate", type)) %>%
  mutate(type=ifelse(Na_Cl<1 & ratio2 <1,  "magnesium chloride", type)) %>%
  mutate(type=ifelse(Na_Cl<1 & ratio2 >1,  "calcium chloride", type))
table(Sulin_data$type)
Sulin_type <- transform(as.data.frame(table(Sulin_data$type)),percentage_column=Freq/nrow(Sulin_data)*100)
names_sulin <- Sulin_type$Var1
pie(Sulin_type$percentage_column,labels=names_sulin, main="Sunlin Classification", col = c("purple", "violetred1", "green3", "cornsilk"), radius = 0.9,cex.main=2, cex=2)
pie(Sulin_type$percentage_column,labels=paste(names(table(Sulin_data$type)), "/", Sulin_type$percentage_column, "%", sep=""),cex.main=2,main="Sunlin Classification", col = c("purple", "violetred1", "green3", "cornsilk"), radius = 0.9)
Sulin_type1 <- Sulin_data %>%
  filter(type=="sodium sulfate")
Sulin_type2 <- Sulin_data %>%
  filter(type=="sodium bicarbonate")
Sulin_type3 <- Sulin_data %>%
  filter(type=="magnesium chloride")
Sulin_type4 <- Sulin_data %>%
  filter(type=="calcium chloride")

#water quality
#select variables
Quality_data <- Total_Data %>%
  select(X.U.FEFF.经度, 纬度, 样品野外编号, 色, 味, 嗅, 浑浊度, 肉眼可见物, PH值, 总硬度.CaCO3., TDS, SO42., Cl., Fe, Mn, Cu, Zn, Mo钼, Co, 挥发酚, 阴离子洗涤剂, NO3., 
          NO2., NH4., F., I., 氰化物, Hg, As砷, Se硒, Cd, Cr6., Pb, Ba钡, Ni, 大肠菌数, 菌落总数) %>%
  mutate(NO3._N = NO3./62*14)
Quality_data_new <- Quality_data %>%
  select_if(~ !any(is.na(.)))
str(Quality_data_new[1,])
#give ranks
Quality_factor <- Quality_data_new %>%
  #mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.>550, 10, NA)) %>%
  mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.<=550, 6, 10)) %>%
  mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.<=450, 3, 总硬度.CaCO3._Fi)) %>%
  mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.<=300, 1, 总硬度.CaCO3._Fi)) %>%
  mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.<=150, 0, 总硬度.CaCO3._Fi)) %>%
  #mutate(TDS_Fi=ifelse(TDS>2000, 10, NA)) %>%
  mutate(TDS_Fi=ifelse(TDS<= 2000, 6, 10)) %>%
  mutate(TDS_Fi=ifelse(TDS<= 1000, 3, TDS_Fi)) %>%
  mutate(TDS_Fi=ifelse(TDS<= 500, 1, TDS_Fi)) %>%
  mutate(TDS_Fi=ifelse(TDS<= 300, 0, TDS_Fi)) %>%
  mutate(SO42._Fi=ifelse(SO42.<= 350, 6, 10)) %>%
  mutate(SO42._Fi=ifelse(SO42.<= 250, 3, SO42._Fi)) %>%
  mutate(SO42._Fi=ifelse(SO42.<= 150, 1, SO42._Fi)) %>%
  mutate(SO42._Fi=ifelse(SO42.<= 50, 0, SO42._Fi)) %>%
  mutate(Cl._Fi=ifelse(Cl.<= 350, 6, 10)) %>%
  mutate(Cl._Fi=ifelse(Cl.<= 250, 3, Cl._Fi)) %>%
  mutate(Cl._Fi=ifelse(Cl.<= 150, 1, Cl._Fi)) %>%
  mutate(Cl._Fi=ifelse(Cl.<= 50, 0, Cl._Fi)) %>%
  mutate(Mn_Fi=ifelse(Mn<= 1.0, 6, 10)) %>%
  mutate(Mn_Fi=ifelse(Mn<= 0.1, 3, Mn_Fi)) %>%
  mutate(Mn_Fi=ifelse(Mn<= 0.05, 1, Mn_Fi)) %>%
  mutate(Mn_Fi=ifelse(Mn<= 0.05, 0, Mn_Fi)) %>%
  mutate(encoding="UTF-8", 挥发酚_Fi=ifelse(挥发酚<=0.01, 6, 10)) %>%
  mutate(encoding="UTF-8", 挥发酚_Fi=ifelse(挥发酚<=0.002, 3, 挥发酚_Fi)) %>%
  mutate(encoding="UTF-8", 挥发酚_Fi=ifelse(挥发酚<=0.001, 1, 挥发酚_Fi)) %>%
  mutate(encoding="UTF-8", 挥发酚_Fi=ifelse(挥发酚<=0.001, 0, 挥发酚_Fi)) %>%
  mutate(NO3._N_Fi=ifelse(NO3._N<= 30, 6, 10)) %>%
  mutate(NO3._N_Fi=ifelse(NO3._N<= 20, 3, NO3._N_Fi)) %>%
  mutate(NO3._N_Fi=ifelse(NO3._N<= 5, 1, NO3._N_Fi)) %>%
  mutate(NO3._N_Fi=ifelse(NO3._N<= 2, 0, NO3._N_Fi)) %>%
  mutate(NO2._Fi=ifelse(NO2.<= 0.1, 6, 10)) %>%
  mutate(NO2._Fi=ifelse(NO2.<= 0.02, 3, NO2._Fi)) %>%
  mutate(NO2._Fi=ifelse(NO2.<= 0.01, 1, NO2._Fi)) %>%
  mutate(NO2._Fi=ifelse(NO2.<= 0.001, 0, NO2._Fi)) %>%
  mutate(NH4._Fi=ifelse(NH4.<= 0.5, 6, 10)) %>%
  mutate(NH4._Fi=ifelse(NH4.<= 0.2, 3, NH4._Fi)) %>%
  mutate(NH4._Fi=ifelse(NH4.<= 0.02, 1, NH4._Fi)) %>%
  mutate(NH4._Fi=ifelse(NH4.<= 0.02, 0, NH4._Fi)) %>%
  mutate(encoding="UTF-8", 氰化物_Fi=ifelse(氰化物<=0.1, 6, 10)) %>%
  mutate(encoding="UTF-8", 氰化物_Fi=ifelse(氰化物<=0.05, 3, 氰化物_Fi)) %>%
  mutate(encoding="UTF-8", 氰化物_Fi=ifelse(氰化物<=0.01, 1, 氰化物_Fi)) %>%
  mutate(encoding="UTF-8", 氰化物_Fi=ifelse(氰化物<=0.001, 0, 氰化物_Fi)) %>%
  mutate(Hg_Fi=ifelse(Hg<= 0.001, 6, 10)) %>%
  mutate(Hg_Fi=ifelse(Hg<= 0.001, 3, Hg_Fi)) %>%
  mutate(Hg_Fi=ifelse(Hg<= 0.0005, 1, Hg_Fi)) %>%
  mutate(Hg_Fi=ifelse(Hg<= 0.00005, 0, Hg_Fi)) %>%
  mutate(encoding="UTF-8", As砷_Fi=ifelse(As砷<= 0.05, 6, 10)) %>%
  mutate(encoding="UTF-8", As砷_Fi=ifelse(As砷<= 0.05, 3, As砷_Fi)) %>%
  mutate(encoding="UTF-8", As砷_Fi=ifelse(As砷<= 0.01, 1, As砷_Fi)) %>%
  mutate(encoding="UTF-8", As砷_Fi=ifelse(As砷<= 0.005, 0, As砷_Fi)) %>%
  mutate(Cd_Fi=ifelse(Cd<= 0.01, 6, 10)) %>%
  mutate(Cd_Fi=ifelse(Cd<= 0.01, 3, Cd_Fi)) %>%
  mutate(Cd_Fi=ifelse(Cd<= 0.001, 1, Cd_Fi)) %>%
  mutate(Cd_Fi=ifelse(Cd<= 0.0001, 0, Cd_Fi)) %>%
  mutate(Pb_Fi=ifelse(Pb<= 0.1, 6, 10)) %>%
  mutate(Pb_Fi=ifelse(Pb<= 0.05, 3, Pb_Fi)) %>%
  mutate(Pb_Fi=ifelse(Pb<= 0.01, 1, Pb_Fi)) %>%
  mutate(Pb_Fi=ifelse(Pb<= 0.005, 0, Pb_Fi)) %>%
  select(X.U.FEFF.经度, 纬度, 总硬度.CaCO3._Fi, TDS_Fi, SO42._Fi, Cl._Fi, Mn_Fi, 挥发酚_Fi, NO3._N_Fi, NO2._Fi, NH4._Fi, 氰化物_Fi, Hg_Fi, As砷_Fi, Cd_Fi, Pb_Fi)
  #select(NO3._N, NO3._N_Fi)
#classify
Quality_class <- Quality_data_new %>%
  #mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.>550, 10, NA)) %>%
  mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.<=550, 4, 5)) %>%
  mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.<=450, 3, 总硬度.CaCO3._Fi)) %>%
  mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.<=300, 2, 总硬度.CaCO3._Fi)) %>%
  mutate(encoding="UTF-8", 总硬度.CaCO3._Fi=ifelse(总硬度.CaCO3.<=150, 1, 总硬度.CaCO3._Fi)) %>%
  #mutate(TDS_Fi=ifelse(TDS>2000, 10, NA)) %>%
  mutate(TDS_Fi=ifelse(TDS<= 2000, 4, 5)) %>%
  mutate(TDS_Fi=ifelse(TDS<= 1000, 3, TDS_Fi)) %>%
  mutate(TDS_Fi=ifelse(TDS<= 500, 2, TDS_Fi)) %>%
  mutate(TDS_Fi=ifelse(TDS<= 300, 1, TDS_Fi)) %>%
  mutate(SO42._Fi=ifelse(SO42.<= 350, 4, 5)) %>%
  mutate(SO42._Fi=ifelse(SO42.<= 250, 3, SO42._Fi)) %>%
  mutate(SO42._Fi=ifelse(SO42.<= 150, 2, SO42._Fi)) %>%
  mutate(SO42._Fi=ifelse(SO42.<= 50, 1, SO42._Fi)) %>%
  mutate(Cl._Fi=ifelse(Cl.<= 350, 4, 5)) %>%
  mutate(Cl._Fi=ifelse(Cl.<= 250, 3, Cl._Fi)) %>%
  mutate(Cl._Fi=ifelse(Cl.<= 150, 2, Cl._Fi)) %>%
  mutate(Cl._Fi=ifelse(Cl.<= 50, 1, Cl._Fi)) %>%
  mutate(Mn_Fi=ifelse(Mn<= 1.0, 4, 5)) %>%
  mutate(Mn_Fi=ifelse(Mn<= 0.1, 3, Mn_Fi)) %>%
  mutate(Mn_Fi=ifelse(Mn<= 0.05, 2, Mn_Fi)) %>%
  mutate(Mn_Fi=ifelse(Mn<= 0.05, 1, Mn_Fi)) %>%
  mutate(encoding="UTF-8", 挥发酚_Fi=ifelse(挥发酚<=0.01, 4, 5)) %>%
  mutate(encoding="UTF-8", 挥发酚_Fi=ifelse(挥发酚<=0.002, 3, 挥发酚_Fi)) %>%
  mutate(encoding="UTF-8", 挥发酚_Fi=ifelse(挥发酚<=0.001, 2, 挥发酚_Fi)) %>%
  mutate(encoding="UTF-8", 挥发酚_Fi=ifelse(挥发酚<=0.001, 1, 挥发酚_Fi)) %>%
  mutate(NO3._N_Fi=ifelse(NO3._N<= 30, 4, 5)) %>%
  mutate(NO3._N_Fi=ifelse(NO3._N<= 20, 3, NO3._N_Fi)) %>%
  mutate(NO3._N_Fi=ifelse(NO3._N<= 5, 2, NO3._N_Fi)) %>%
  mutate(NO3._N_Fi=ifelse(NO3._N<= 2, 1, NO3._N_Fi)) %>%
  mutate(NO2._Fi=ifelse(NO2.<= 0.1, 4, 5)) %>%
  mutate(NO2._Fi=ifelse(NO2.<= 0.02, 3, NO2._Fi)) %>%
  mutate(NO2._Fi=ifelse(NO2.<= 0.01, 2, NO2._Fi)) %>%
  mutate(NO2._Fi=ifelse(NO2.<= 0.001, 1, NO2._Fi)) %>%
  mutate(NH4._Fi=ifelse(NH4.<= 0.5, 4, 5)) %>%
  mutate(NH4._Fi=ifelse(NH4.<= 0.2, 3, NH4._Fi)) %>%
  mutate(NH4._Fi=ifelse(NH4.<= 0.02, 2, NH4._Fi)) %>%
  mutate(NH4._Fi=ifelse(NH4.<= 0.02, 1, NH4._Fi)) %>%
  mutate(encoding="UTF-8", 氰化物_Fi=ifelse(氰化物<=0.1, 4, 5)) %>%
  mutate(encoding="UTF-8", 氰化物_Fi=ifelse(氰化物<=0.05, 3, 氰化物_Fi)) %>%
  mutate(encoding="UTF-8", 氰化物_Fi=ifelse(氰化物<=0.01, 2, 氰化物_Fi)) %>%
  mutate(encoding="UTF-8", 氰化物_Fi=ifelse(氰化物<=0.001, 1, 氰化物_Fi)) %>%
  mutate(Hg_Fi=ifelse(Hg<= 0.001, 4, 5)) %>%
  mutate(Hg_Fi=ifelse(Hg<= 0.001, 3, Hg_Fi)) %>%
  mutate(Hg_Fi=ifelse(Hg<= 0.0005, 2, Hg_Fi)) %>%
  mutate(Hg_Fi=ifelse(Hg<= 0.00005, 1, Hg_Fi)) %>%
  mutate(encoding="UTF-8", As砷_Fi=ifelse(As砷<= 0.05, 4, 5)) %>%
  mutate(encoding="UTF-8", As砷_Fi=ifelse(As砷<= 0.05, 3, As砷_Fi)) %>%
  mutate(encoding="UTF-8", As砷_Fi=ifelse(As砷<= 0.01, 2, As砷_Fi)) %>%
  mutate(encoding="UTF-8", As砷_Fi=ifelse(As砷<= 0.005, 1, As砷_Fi)) %>%
  mutate(Cd_Fi=ifelse(Cd<= 0.01, 4, 5)) %>%
  mutate(Cd_Fi=ifelse(Cd<= 0.01, 3, Cd_Fi)) %>%
  mutate(Cd_Fi=ifelse(Cd<= 0.001, 2, Cd_Fi)) %>%
  mutate(Cd_Fi=ifelse(Cd<= 0.0001, 1, Cd_Fi)) %>%
  mutate(Pb_Fi=ifelse(Pb<= 0.1, 4, 5)) %>%
  mutate(Pb_Fi=ifelse(Pb<= 0.05, 3, Pb_Fi)) %>%
  mutate(Pb_Fi=ifelse(Pb<= 0.01, 2, Pb_Fi)) %>%
  mutate(Pb_Fi=ifelse(Pb<= 0.005, 1, Pb_Fi)) %>%
  select(X.U.FEFF.经度, 纬度, 总硬度.CaCO3._Fi, TDS_Fi, SO42._Fi, Cl._Fi, Mn_Fi, 挥发酚_Fi, NO3._N_Fi, NO2._Fi, NH4._Fi, 氰化物_Fi, Hg_Fi, As砷_Fi, Cd_Fi, Pb_Fi)
#select(NO3._N, NO3._N_Fi)



#frequency for each component
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}

for(j in 3:16) { 
  class <- do.call(cbind,lapply(Quality_class[j],tblFun))
  s <- colnames(Quality_class[j])
  colnames(class) <- c('Count', s)
  print(class)
  lals_pct <- paste(names(table(Quality_class[j])), "-", class[,2], "%", sep="")
  filename = paste("pie", j, ".jpeg", seq="")
  jpeg(file=filename)
  pie(class[,2],main=s,col = rainbow(10), radius = 0.9, labels=lals_pct)
  dev.off()
}


#calculate the rank
#install.packages("matrixStats")
library(matrixStats)
Quality_com_factor <- Quality_factor %>%
  mutate(mean_Fi=rowMeans(Quality_factor[,3:16]), max_Fi=rowMaxs(as.matrix(Quality_factor[,3:16]))) %>%
  mutate(com_F=sqrt(mean_Fi^2+max_Fi^2)) %>%
  mutate(encoding="UTF-8", result=ifelse(com_F<= 7.20, "较差", "极差")) %>%
  mutate(encoding="UTF-8", result=ifelse(com_F<= 4.25, "较好", result)) %>%
  mutate(encoding="UTF-8", result=ifelse(com_F<= 2.50, "良好", result)) %>%
  mutate(encoding="UTF-8", result=ifelse(com_F<= 0.80, "优良", result))

table(Quality_com_factor$result)
result_state <- transform(as.data.frame(table(Quality_com_factor$result)),percentage_column=Freq/nrow(Quality_com_factor)*100)
#rowMaxs(as.matrix(Quality_factor[,3:16]))
#rowMeans(Quality_factor[,3:16])
#apply(Quality_factor[3:16], 1, max)

#plot pie
names_pie <- result_state$Var1
#names_pie_data <- result_state$Freq
#names_pie_fre <- result_state$percentage_column
pie(result_state$percentage_column,labels=names_pie, main="Groundwater Quality", cex=1.5, cex.main=2, col = c("purple", "violetred1", "green3", "cornsilk"), radius = 0.9)
pie(result_state$percentage_column, main="Groundwater Quality", cex=1.5, cex.main=2, col = c("purple", "violetred1", "green3", "cornsilk"), radius = 0.9, labels = paste(names(table(Quality_com_factor$result)), "-", result_state$percentage_column, "%", sep=""))
#pie(result_state$percentage_column,labels=names_pie_data, col = c("purple", "violetred1", "green3", "cornsilk"), radius = 0.9)
#pie(result_state$percentage_column,labels=names_pie_fre, col = c("purple", "violetred1", "green3", "cornsilk"), radius = 0.9)

#plot
#install.packages("raster")
#install.packages("sp")
#install.packages("rgdal")
#install.packages("gstat")
#install.packages("maptools")
library(raster)
library(sp)
library(rgdal)
library(gstat)
library(maptools)
#select the area
#China_map <- readOGR("D://Work and study files/课程/ESE5023_Assignments/HW5/China_map", "bou2_4p") 
#plot(China_map)
#boundary <-  subset(China_map, NAME=="江苏省")
#plot(boundary)
#Bonudry
city <- readOGR("Boundary", "gadm36_CHN_2")
plot(city)
city_s1 <- subset(city, NAME_2=="Lianyungang")
plot(city_s1, add=T, col="red")
city_s2 <- subset(city, NAME_2=="Nantong")
plot(city_s2, add=T, col="green")
city_s3 <- subset(city, NAME_2=="Yancheng")
plot(city_s3, add=T, col="blue")
boundary1 <-  subset(city, NAME_2=="Lianyungang")
plot(boundary1)
boundary2 <-  subset(city, NAME_2=="Nantong")
plot(boundary2)
boundary3 <-  subset(city, NAME_2=="Yancheng")
plot(boundary3)
boundary <- rbind.SpatialPolygons(boundary1, boundary2, boundary3)
plot(boundary)
#city <- readOGR("Boundary", "边界")
#plot(city)
library(sp)
library(raster)
library(magrittr)
library(dplyr)


# 统一坐标系
#MyCRS <- CRS("+proj=aea +lat_1=25 +lat_2=50 +lon_0=105")
# 将温度数据转化为sp对象
#Quality_sp <- SpatialPointsDataFrame(coords = cbind(x = Quality_com_factor$X.U.FEFF.经度, y = Quality_com_factor$纬度),
                                                              #data = dplyr::select(Quality_com_factor, com_F),
#eatablish sp                                                               #proj4string = MyCRS)
Quality_sp <- SpatialPoints(Quality_com_factor[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Quality_sp <- SpatialPointsDataFrame(Quality_sp,Quality_com_factor)

#install.packages("tmap")
library(tmap)

tm_shape(shp = boundary) + 
    tm_polygons(col = "yellow", border.col = "cyan") + 
    tm_borders(lwd = 0.5) + 
    tm_shape(shp = Quality_sp) + 
    tm_dots(col = "magenta", size = 0.2, shape = 21)
library(gstat) # 内含插值函数
library(sp)    # 内含spsample函数
library(raster)

# 创建空栅格，n表示栅格数量
## 首先生成空栅格函数
grd_empty <- function(water_sp, n) {
    grd <- as.data.frame(spsample(water_sp, "regular", n=n))
    names(grd) <- c("X", "Y")
    coordinates(grd) <- c("X", "Y")
    gridded(grd) <- TRUE
    fullgrid(grd) <- TRUE
    proj4string(grd) <- proj4string(water_sp)
    return(grd)
  }

## 调用函数
grd_TEM <- grd_empty(water_sp = Quality_sp, n = 40000) # 栅格总数量


# idw插值(指定幂次为2， 即idp = 2)创建raster
Quality_idw <- gstat::idw(formula = com_F ~ 1, # 反距离加权插值
                                            locations = Quality_sp,
                                             newdata = grd_TEM,
                                             idp = 2) # 幂次为2
Quality_raster <- raster(Quality_idw) # 栅格化
Quality_mask <- mask(Quality_raster, boundary) # 筛选在boundary范围内的栅格
#rm(TEM_idw, TEM_raster)

#library(tmap)

tmap_Quality <- function(Water_data = Quality_mask) {
    tm_shape(shp = Water_data) + 
        tm_raster(n=5, palette = "-PuOr", legend.reverse = TRUE, # 负色板
                                 auto.palette.mapping = FALSE, 
                  title="Water Quality") + 
    tm_legend(legend.text.size=1.2,  legend.title.size=2) +
        tm_shape(shp = boundary) +  
        tm_borders(col = "black", lwd = 0.5) +
        tm_legend(legend.outside = TRUE)
  }

tmap_Quality() 

#plot sulin type spatial
Sulin_data_sp <- SpatialPoints(Sulin_data[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Sulin_data_sp <- SpatialPointsDataFrame(Sulin_data_sp,Sulin_data)
Sulin_type1_sp <- SpatialPoints(Sulin_type1[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Sulin_type1_sp <- SpatialPointsDataFrame(Sulin_type1_sp,Sulin_type1)
Sulin_type2_sp <- SpatialPoints(Sulin_type2[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Sulin_type2_sp <- SpatialPointsDataFrame(Sulin_type2_sp,Sulin_type2)
Sulin_type3_sp <- SpatialPoints(Sulin_type3[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Sulin_type3_sp <- SpatialPointsDataFrame(Sulin_type3_sp,Sulin_type3)
Sulin_type4_sp <- SpatialPoints(Sulin_type4[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Sulin_type4_sp <- SpatialPointsDataFrame(Sulin_type4_sp,Sulin_type4)
#(shp = boundary) + 
 # tm_polygons(col = "white", border.col = "black") + 
  #tm_shape(shp = Sulin_type1_sp) + 
 # tm_dots(col = "red", size = 0.2, shape = 21) +
 # tm_shape(shp = Sulin_type2_sp) + 
 # tm_dots(col = "yellow", size = 0.2, shape = 21)+
#  tm_shape(shp = Sulin_type3_sp) + 
#  tm_dots(col = "blue", size = 0.2, shape = 21) +
#  tm_shape(shp = Sulin_type4_sp) + 
#  tm_dots(col = "green", size = 0.2, shape = 21)

tm_shape(shp = boundary) + 
  tm_polygons(col = "white", border.col = "black") + 
  tm_shape(shp = Sulin_data_sp) + 
  tm_dots(size = 0.2, col= "type", shape= 21, legend.show = TRUE, legend.shape.show = TRUE,shapes.legend=22, scale=1.5) +
  tm_legend(legend.text.size=1.2,  legend.title.size=2)
tm_shape(shp = boundary) + 
  tm_polygons(col = "white", border.col = "black") + 
  tm_shape(shp = Sulin_type1_sp) + 
  tm_dots(size = 0.2, col= "red", shape="type", legend.show = TRUE, legend.shape.show = TRUE,shapes.legend=22, scale=1.5)+
  tm_legend(legend.text.size=1.2,  legend.title.size=2)
tm_shape(shp = boundary) + 
  tm_polygons(col = "white", border.col = "black") + 
  tm_shape(shp = Sulin_type2_sp) + 
  tm_dots(size = 0.2, col= "yellow", shape="type", legend.show = TRUE, legend.shape.show = TRUE,shapes.legend=22, scale=1.5)+
  tm_legend(legend.text.size=1.2,  legend.title.size=2)
tm_shape(shp = boundary) + 
  tm_polygons(col = "white", border.col = "black") + 
  tm_shape(shp = Sulin_type3_sp) + 
  tm_dots(size = 0.2, col= "blue", shape="type", legend.show = TRUE, legend.shape.show = TRUE,shapes.legend=22, scale=1.5)+
  tm_legend(legend.text.size=1.2,  legend.title.size=2)
tm_shape(shp = boundary) + 
  tm_polygons(col = "white", border.col = "black") + 
  tm_shape(shp = Sulin_type4_sp) + 
  tm_dots(size = 0.2, col= "green", shape="type", legend.show = TRUE, legend.shape.show = TRUE,shapes.legend=22, scale=1.5)+
  tm_legend(legend.text.size=1.2,  legend.title.size=2)
#plot class of factor
Quality_class_chara <- Quality_class
for (k in 3:16) {
  for (w in 1:644) {
     Quality_class_chara[w,k]=as.character(Quality_class_chara[w,k])
  }
}
str(Quality_class_chara[1,])
Quality_class_sp <- SpatialPoints(Quality_class_chara[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Quality_class_sp <- SpatialPointsDataFrame(Quality_class_sp,Quality_class_chara)
for (k in 3:16) {
  names_col = colnames(Quality_class_chara[k])
  plot_Fi <- tm_shape(shp = boundary) + 
    tm_polygons(col = "white", border.col = "black") + 
    tm_shape(shp = Quality_class_sp) + 
    tm_dots(size = 0.2, col= names_col, shape= 21, legend.show = TRUE, legend.shape.show = TRUE,shapes.legend=22, scale=1.5) +
    tm_legend(legend.text.size=1.2,  legend.title.size=2)
  filename = paste("Fi", k, ".jpeg", seq="")
   jpeg(file=filename)
   print(plot_Fi)
   dev.off()
}






