install.packages("forecast")
library(lubridate)
library(forecast)
Baoan_Data <- read.csv("2281305.csv", head= TRUE)
names(Baoan_Data)
head(Baoan_Data)
Met_Data <- Baoan_Data %>%
  mutate(temp=substr(TMP, 1,5), temp_quality=substr(TMP, 7,7)) %>%
  mutate(temp_new = as.numeric(temp), temp_quality_new = as.logical(as.numeric(temp_quality)))
# Handel missing values
Met_Data$temp_new[which(Met_Data$temp_new==9999)]<- NA
for(i in 1:length(Met_Data$temp_new)){
  if( is.na(Met_Data$temp_new[i])){
    Met_Data$temp_new[i] <- mean(Met_Data$temp_new[(i-2):(i+2)],na.rm=T )
  }
}
Met_Data$temp_quality_new[!which(Met_Data$temp_quality_new)]       <- NA
Met_Data1 <- Met_Data %>%
  mutate(temp_new=temp_new*0.1)
#2.1 Construct a time series of monthly-averaged temperature from 2010 Jan. to 2020 Aug.
montly_mean <- c()
for (i in 2010:2020) {
  Thisyear_mean <- Met_Data1 %>%
    select(DATE, temp_new, temp_quality_new) %>%
    mutate(month=substr(DATE, 1, 7)) %>%
    mutate(year=substr(DATE,1,4)) %>%
    mutate(year2=as.numeric(year)) %>%
    filter(year2==i) %>%
    group_by(month) %>%
    summarise(mean=mean(temp_new, na.rm = T)) 
  montly_mean <- rbind(montly_mean, Thisyear_mean) 
}

Monthly_mean1 <- montly_mean %>%
  filter(month <= "2020-08" & month >= "2010-01")
# Apply the ts() function
TEMP_mean <- ts(Monthly_mean1$mean, start=c(2010,1), frequency=12)

plot(TEMP_mean, type="l")

#2.2 Decompose the time series into trend, seasonality, and error parts. Check whether the error part follows a white noise distribution.
TEMP_mean_components <- decompose(TEMP_mean)
plot(TEMP_mean_components)
# Plot hist
hist(TEMP_mean_components$random, prob=TRUE)
# Add pdf
curve(dnorm(x, mean=mean(TEMP_mean_components$random,na.rm=T),
            sd=sd(TEMP_mean_components$random,na.rm=T)),
      add=TRUE, col="red")

# Plot hist
hist(TEMP_mean_components$trend, prob=TRUE)
# Add pdf
curve(dnorm(x, mean=mean(TEMP_mean_components$trend,na.rm=T),
            sd=sd(TEMP_mean_components$trend,na.rm=T)),
      add=TRUE, col="red")

# Plot hist
hist(TEMP_mean_components$seasonal, prob=TRUE)
# Add pdf
curve(dnorm(x, mean=mean(TEMP_mean_components$seasonal,na.rm=T),
            sd=sd(TEMP_mean_components$seasonal,na.rm=T)),
      add=TRUE, col="red") 

# Plot hist
hist(TEMP_mean_components$x, prob=TRUE)
# Add pdf
curve(dnorm(x, mean=mean(TEMP_mean_components$x,na.rm=T),
            sd=sd(TEMP_mean_components$x,na.rm=T)),
      add=TRUE, col="red") 


#2.3 Fit an ARIMA(p,d,q) model to the time series. Describe the fitting process in details in your report.
TEMP_mean_log <- log(TEMP_mean)
plot(TEMP_mean_log, type="l")
TEMP_mean_log_components <- decompose(TEMP_mean_log)
plot(TEMP_mean_log_components)



TEMP_mean_log_d1 <- diff(TEMP_mean_log)
plot(TEMP_mean_log_d1)
hist(TEMP_mean_log_d1)

# Check acf and pacf
acf(TEMP_mean_log_d1)
pacf(TEMP_mean_log_d1)

model <- auto.arima(TEMP_mean_log)
model


#2.4 Predict monthly-averaged temperatures in 2020 Sep. and Oct. with the ARIMA model from 2.3. The predictions will be evaluated against actual observations in those two months.
# Number of days to predict
days_forecast  <- 10
# Number of include in the plot
days_in_plot   <- 20
# Make predictions using the forecast() function
forecast_10days <- forecast(model, days_forecast)
# Plot
plot(forecast(model, days_forecast), include=days_in_plot, 
     xlab="Time", ylab="log(global cases)",type="o",lwd=2) 

# Sep 20
day_forward <- (2020-2010)*12+9-((2020-2010)*12+8) #(year-start_year)*12+month-((end_year-start_year)*12+end_month)
exp(forecast_10days$mean[day_forward])
exp(forecast_10days$lower[day_forward,1])
exp(forecast_10days$upper[day_forward,1])

# Oct 20
day_forward <- (2020-2010)*12+10-((2020-2010)*12+8)
exp(forecast_10days$mean[day_forward])
exp(forecast_10days$lower[day_forward,1])
exp(forecast_10days$upper[day_forward,1])





#青助教解释一下，数据是否需要进行前处理的原因，我都做了结果，但是感觉结果差别不大，是不需要做吗？？
model1 <- auto.arima(TEMP_mean)
model1

# Number of days to predict
days_forecast  <- 10
# Number of include in the plot
days_in_plot   <- 20
# Make predictions using the forecast() function
forecast_10days <- forecast(model1, days_forecast)
# Plot
plot(forecast(model1, days_forecast), include=days_in_plot, 
     xlab="Time", ylab="global cases",type="o",lwd=2) 

# Sep 20
day_forward <- (2020-2010)*12+9-((2020-2010)*12+8) #(year-start_year)*12+month-((end_year-start_year)*12+end_month)
forecast_10days$mean[day_forward]
forecast_10days$lower[day_forward,1]
forecast_10days$upper[day_forward,1]

# Oct 20
day_forward <- (2020-2010)*12+10-((2020-2010)*12+8)
forecast_10days$mean[day_forward]
forecast_10days$lower[day_forward,1]
forecast_10days$upper[day_forward,1]

# good work
