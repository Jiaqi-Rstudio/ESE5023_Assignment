# Load libraries
library(dplyr)
library(ggplot2)
library(gapminder)
#precipitaion data from 6/1/2017 to 2/29/2020 in Tongguan Country
install.packages("readxl")
library(readxl)
TG_rain <- read_excel("tongguan.xls", sheet="rain")
head(TG_rain)
#Boxplot of precipitaion in Tongguan from 2018 to 2019
TG_rain %>%
  filter(date >= "2018-01-01" & date < "2020-01-01") %>%
  mutate(year=substr(date, 1,4)) %>% 
  ggplot(aes(x=year, y=precipitation,fill=year)) +
  geom_boxplot(alpha=0.7) +
  labs(title="Boxplot of precipitaion in Tongguan from 2018 to 2019", x="Year", y="Precipitation/m", fill="Year") +
  theme_bw() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=20), 
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.key.size = unit(2,'cm'))
ggsave("boxplot.png") 

#Time series of precipitaion in Tongguan from 2018 to 2019
# Apply the ts() function
Pre <- ts(TG_rain$precipitation, start=c(2018-01-01), frequency=12)

# Quick plot
plot(Pre, type="l")

TG_rain %>%
  filter(date >= "2018-01-01" & date < "2020-01-01") %>%
  ggplot(aes(x=date, y=precipitation))+
  geom_line(color = "red", size=0.5) +
  labs(title="Time sereies of precipitaion in Tongguan from 2018 to 2019", x="Date", y="Precipitation/m") +
  theme_bw() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=20), 
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20))
ggsave("time series.png") 

#Histogram of precipitaion in Tongguan from 2018 to 2019
TG_rain %>%
  filter(date >= "2018-01-01" & date < "2020-01-01") %>%
  ggplot(aes(x=precipitation))+
  geom_histogram(bins=12, color="blue", fill="blue", alpha=0.9) +
  labs(title="Histogram of precipitaion in Tongguan from 2018 to 2019", x="Precipitation/m", y="") +
  theme_bw() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=20), 
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20))
ggsave("histogram.png") 

#Scatter plot of precipitaion in Tongguan from 2018 to 2019
TG_rain %>%
  filter(date >= "2018-01-01" & date < "2020-01-01") %>%
  ggplot(aes(x=date, y=precipitation)) +
  geom_point(color="blue") + 
  labs(title="Scatter plot of precipitaion in Tongguan from 2018 to 2019", x="Date", y="Precipitation/m") +
  theme_bw() +
  theme(panel.grid.major =element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=20), 
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20))
ggsave("Scatter plot.png") 

#Image Plot
install.packages("fields")
install.packages("maps")
install.packages("RNetCDF")
library(fields); 
library(maps); 
library(RNetCDF)

# Open the NetCDF file
ex.nc     <- open.nc("precip.mon.ltm.nc")
# Print the variables and attributes
print.nc(ex.nc)

# Read the variables
# Lat
Lat       <- var.get.nc(ex.nc, "lat")
# Lon
Lon       <- var.get.nc(ex.nc, "lon")
# Long Term Mean Average Monthly Rate of Precipitation mm/day
precip_T     <- var.get.nc(ex.nc, "precip") 

# Close the NetCDF file
close.nc(ex.nc)

# Original Lat is in decreasing order, we need to reverse it
Lat <- rev(Lat)

# Data transformation of precip_T_Jan
precip_T_Jan <- array(NA,dim=c(length(Lon), length(Lat)))
for(row in 1:length(Lat)){
  precip_T_Jan[,row] <- precip_T[, (length(Lat)+1-row),1 ]
}

image.plot(Lon, Lat,precip_T_Jan)

# Set margins on bottom, left, top, right
par(mar=c(4.5,3,2,1))

# Plot
image.plot(Lon, Lat, precip_T_Jan,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Rate of Precipitation mm/day",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("Long Term (1981-2010) Mean Average Monthly Rate of Precipitation in Jan."),
      cex.main=1,font.main=2)

# Add map
map('world',add=T,lwd=0.75,col="black")

# Add a box
box(lwd=2)

# Set the png format
png("Precip_T.png", width=8.5, height=6, units="in", res=400) 

# Set margins on bottom, left, top, right
par(mar=c(4.5,3,2,1))

# Plot
image.plot(Lon, Lat, precip_T_Jan,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Rate of Precipitation mm/day",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("Long Term (1981-2010) Mean Average Monthly Rate of Precipitation in Jan."),
      cex.main=1,font.main=2)

# Add map
map('world',add=T,lwd=0.75,col="black")

# Add a box
box(lwd=2)

# Close the png file to save the file
dev.off()
