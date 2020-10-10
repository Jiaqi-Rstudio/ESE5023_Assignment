# Read the csv file 
Airport_Data <- read.csv(file = "2281305.csv", header = T) 
names(Airport_Data) 
BaoAn_V<-Airport_Data$VIS 

Obs_Time<-Airport_Data$DATE 

BaoAn_V_value <- substr(BaoAn_V,1,6) 
BaoAn_V_flag <- substr(BaoAn_V,12,12) 
BaoAn_V_value2 <- as.numeric(BaoAn_V_value) 
BaoAn_V_flag2 <- as.numeric(BaoAn_V_flag) 

BaoAn_V_value2[which(BaoAn_V_value2 == 999999)]  <- NA 
BaoAn_V_value2[!which(BaoAn_V_flag2 == 1)]  <- NA 
BaoAn_V_value3 <- BaoAn_V_value2 * 0.001 

Obs_Time_Date <- substr(Obs_Time,1,10) 
Obs_Time_Hour <- substr(Obs_Time,12,13) 
Obs_Time2 <- as.Date(Obs_Time) 
plot(Obs_Time2,BaoAn_V_value3, lwd=0.5,type="l",col="blue") 


BaoAn_V_flag3 <- BaoAn_V_flag2[which(BaoAn_V_flag2 == 1)] 
result_5 <- table(BaoAn_V_flag3[which(BaoAn_V_value3 < 5)]) 
result_10 <- table(BaoAn_V_flag3[which(BaoAn_V_value3 < 10)]) - result_5 
result_15 <- table(BaoAn_V_flag3[which(BaoAn_V_value3 < 15)]) - result_10 
result_20 <- table(BaoAn_V_flag3[which(BaoAn_V_value3 < 20)]) - result_15 
result_25 <- table(BaoAn_V_flag3[which(BaoAn_V_value3 < 25)]) - result_20 
result_30 <- table(BaoAn_V_flag3[which(BaoAn_V_value3 < 30)]) - result_25 
result_30plus <- table(BaoAn_V_flag3[which(BaoAn_V_value3 >= 30)]) 
results <- c(result_5,result_10,result_15,result_20,result_25,result_30,result_30plus) 
plot(c(1,2,3,4,5,6,7),results*0.01, lwd=0.5,type="l",col="blue")