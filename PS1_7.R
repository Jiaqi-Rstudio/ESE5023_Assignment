# Read the Precipitation csv file 
Precipitation_Data <- read.csv(file = "VQC00670480.csv", header = T) 
names(Precipitation_Data) 
Pre_V<-Precipitation_Data$DlySum 
head(Pre_V) 
tail(Pre_V) 
typeof(Pre_V) 

Obs_Time<-Precipitation_Data$DATE 
head(Obs_Time) 
tail(Obs_Time) 
typeof(Obs_Time) 

#Clean the data, make the missing data NA; retain the data after 2016 to plot 
Pre_Vnum <- as.numeric(Pre_V) 
Pre_V_flag <- Precipitation_Data$DlySumQF 
Pre_Vnum[which(Pre_V_flag == "M")]  <- NA 
Pre_V_cal <- Pre_Vnum[which(Obs_Time > "2016-12-31")] 
Obs_Time2  <- Obs_Time[which(Obs_Time > "2016-12-31")] 
Obs_Time_cal <- as.Date(Obs_Time2) 

#Plot the time series of the daily sum precipitation after 2016. 
plot(Obs_Time_cal,Pre_V_cal, lwd=0.5,type="l",col="blue") 

#statistical checks with the daily sum precipitation after 2016. 
sum_cal <- sum(Pre_V_cal,na.rm=T) 
mean_cal <- mean(Pre_V_cal,na.rm=T) 
median_cal <- median(Pre_V_cal,na.rm=T) 
min_cal <- min(Pre_V_cal,na.rm=T) 
max_cal <- max(Pre_V_cal,na.rm=T) 
min_date <- Obs_Time_cal[which.min(Pre_V_cal)] 
max_date <- Obs_Time_cal[which.max(Pre_V_cal)] 

print(c(sum_cal,mean_cal,median_cal,min_cal,max_cal)) 
print(c(min_date,max_date)) 