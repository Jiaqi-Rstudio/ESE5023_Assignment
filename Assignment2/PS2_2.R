Met_Data       <- read.csv(file = "2281305.csv", header = T)
names(Met_Data)
SZ_wind <- as_tibble(Met_Data)
SZ_wind
montly_mean <- c()
  for (i in 2010:2020) {
  Thisyear_mean <- SZ_wind %>%
    select(DATE, WND) %>%
      mutate(wind_speed=substr(WND, 9, 12)) %>%
      mutate(wind_speed2=as.numeric(wind_speed)) %>%
      mutate(month=substr(DATE,1,7)) %>%
    #mutate(month2=as.numeric(month)) %>%
      mutate(year=substr(DATE,1,4)) %>%
      mutate(year2=as.numeric(year)) %>%
      filter(year2==i) %>%
      group_by(month) %>%
      summarise(mean=mean(wind_speed2, na.rm = T)) 
    montly_mean <- rbind(montly_mean, Thisyear_mean) #help from TA Ming YANG
  }
montly_mean %>%
  ggplot(aes(x=month, y=mean))+
  geom_point() 




