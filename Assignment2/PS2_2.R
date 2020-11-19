Met_Data       <- read.csv(file = "2281305.csv", header = T)
names(Met_Data)
SZ_wind <- as_tibble(Met_Data)
SZ_wind
montly_mean <- c()
  for (i in 2010:2020) {
   # @ MingYANG noticed:
   # semms awkward for using "for" to get the annual data
   # (1)"filter(substr(DATE,1)>2010 && substr(DATE,1)<2020)" can achieve the same effect
   # (2)these data are not qualified, so you should use filter to select the qualified data:
   # for example:
   # "filter(substr(WND,1,3)!="999")"   for avoiding missing data of direction angle
   # "filter(substr(WND,9,12)!="9999")"   for avoiding missing data of speed rate
   # filter(substr(WND,5,7)=="1,N")"    for avoiding unqualified data in direction and type
   # "filter(substr(WND,14,14)=="1")"   for avoiding unqualified data in speed
   # (3) SCALING FACTOR of speed data is 10, so the original data should multiply 0.1 to get the real wind speed
   # the end
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




