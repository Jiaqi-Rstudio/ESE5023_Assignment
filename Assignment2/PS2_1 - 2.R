#1.1 
Data_E <- read.delim("signif.txt",head=TRUE)
Sig_Eqs <- as_tibble(Data_E)
Sig_Eqs
#1.2
select(Sig_Eqs, YEAR, COUNTRY, DEATHS)  %>%
  group_by(COUNTRY) %>%
  summarise(countrytotal=sum(DEATHS, na.rm = T)) %>%
  arrange(desc(countrytotal))

#1.3
select(Sig_Eqs, YEAR, EQ_PRIMARY) %>%
  filter(EQ_PRIMARY >= 6.0) %>% 
  ggplot(aes(x=YEAR, y=EQ_PRIMARY)) + 
  geom_point()

select(Sig_Eqs, YEAR, EQ_PRIMARY) %>%
  filter(EQ_PRIMARY >= 6.0) %>% 
  group_by(YEAR) %>%
  count() %>%
  ggplot(aes(x=YEAR, y=n)) + 
  geom_point()

NUM <- Sig_Eqs%>%
select(YEAR, EQ_PRIMARY) %>%
  filter(EQ_PRIMARY >= 6.0) %>% 
  group_by(YEAR) %>%
  count()
names(NUM)

#1.4
country_total <- Sig_Eqs %>%
  group_by(COUNTRY) %>%
  count()
 # summarise(countrytotal=sum(DEATHS, na.rm = T)) 
country_max <- Sig_Eqs %>%
  select(YEAR, MONTH, DAY, COUNTRY,EQ_PRIMARY) %>%
  mutate(DATE=paste(YEAR,MONTH,DAY,sep = "_")) %>%
  select(DATE, COUNTRY, EQ_PRIMARY) %>%
  group_by(COUNTRY) %>%
  summarize(magmax=max(EQ_PRIMARY,na.rm = T))
country_date <- Sig_Eqs %>%
  select(YEAR, MONTH, DAY, COUNTRY,EQ_PRIMARY) %>%
  mutate(DATE=paste(YEAR,MONTH,DAY,sep = "_")) %>%
  select(DATE, COUNTRY, EQ_PRIMARY)
#total_C <- country_total$countrytotal
#max_magnitude <- country_max$magmax
CountEq_LargestEq <- function(states){
 # total_C <- country_total %>%
   # filter(COUNTRY==states) 
  max_magnitude <- country_max %>%
    filter(COUNTRY==states) 
  max_magnitude2 <- max_magnitude$magmax
  max_date <- country_date %>%
    group_by(COUNTRY) %>%
    mutate(count= n()) %>%
    filter(EQ_PRIMARY==max_magnitude2 & COUNTRY==states)
  #print("The total number of earthquakes since 2150 B.C.is ")
 # print(total_C$n)
 # print("the date of the largest earthquake ever happened in this country is ")
 # print(max_date$DATE)
max_date
}
CountEq_LargestEq("CHINA")

country <- country_total$COUNTRY
Result <- c()
  for (i in country) {
    max <-  CountEq_LargestEq(i)
   Result <- rbind(Result, max)
  }

Order <- Result %>%
  arrange(desc(count))
  





