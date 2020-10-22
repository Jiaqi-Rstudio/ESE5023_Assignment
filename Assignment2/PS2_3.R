Precipitation_Data <- read.csv(file = "VQC00670480.csv", header = T) 
names(Precipitation_Data) 
Pre_Data <- as_tibble(Precipitation_Data)
#Plot the time series of a certain variable.
select(Pre_Data, DATE, DlySum, DlySumQF) %>%
  filter(DATE > "2016-12-31") %>%
  mutate(DlySum_new=ifelse(DlySumQF=="M", NA, DlySum)) %>%
  ggplot(aes(x=DATE, y=DlySum_new)) + 
  geom_point()
#Conduct at least 5 simple statistical checks with the variable, and report your findings.
select(Pre_Data, DATE, DlySum, DlySumQF) %>%
  mutate(Year=substr(DATE,1,4)) %>%
  filter(DlySumQF!="M") %>%
  group_by(Year) %>%
  summarize(Annual_mean=mean(DlySum), Annual_sd=sd(DlySum), 
            Annual_min=min(DlySum),Annual_max=max(DlySum),
            Annual_se=sd(DlySum)/sqrt(n()))

