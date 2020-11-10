#1.1 
Data_E <- read.delim("signif.txt",head=TRUE)
Sig_Eqs <- as_tibble(Data_E)
Sig_Eqs
#1.2
select(Sig_Eqs, YEAR, COUNTRY, DEATHS)  %>%
  group_by(COUNTRY) %>%
  summarise(countrytotal=sum(DEATHS, na.rm = T)) %>%
  arrange(desc(countrytotal))
# @MingYANG recommended:
# there are some differences with TOTEL_DEATHS and DEATHS
# using "select" for calculating is ok,but you should print the top 10 countries 
# adding "TOPTEN<-" in the beginning of line 6 to save the selected data and
# using "print(TOPTEN[1:10,])" for printing the top 10 countries
# the end

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
# good work

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
  total_C <- country_total %>%
    filter(COUNTRY==states) 
  max_magnitude <- country_max %>%
    filter(COUNTRY==states) 
  max_magnitude2 <- max_magnitude$magmax
  max_date <- country_date %>%
    filter(EQ_PRIMARY==max_magnitude2 & COUNTRY==states)
  print("The total number of earthquakes since 2150 B.C.is ")
  print(total_C$n)
  print("the date of the largest earthquake ever happened in this country is ")
  print(max_date$DATE)
}
CountEq_LargestEq("CHINA")
 
# @MingYANG noticed
# Search the biggest earthquake for each country, you could take the following code as a reference:
# country<-unique(Sig_Eqs$COUNTRY)
# EQ_number<-c()
# EQ_country<-c()
# EQmax_date<-c()
# for(i in country){
#   a<-as.numeric(CountEq_LargestEq(i)[1])
#   b<-i
#   c<-as.character(CountEq_LargestEq(i)[2])
#   EQ_number <-c(EQ_number,a)
#   EQ_country<-c(EQ_country,b)
#   EQmax_date<-c(EQmax_date,c)
# }
# df1<-data.frame(EQ_country,EQ_number ,EQmax_date)
# tbl_new<-as_tibble(df1)
# tbl_new %>% 
#   arrange(desc(EQ_number))
