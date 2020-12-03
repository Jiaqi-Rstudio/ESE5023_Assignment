install.packages("sp")
install.packages("rgdal")
install.packages("sf")
install.packages("raster")
library("sp")
library("rgdal")
library("sf")
library("raster")
library(fields); 
library(maps); 
library(RNetCDF)
setwd("E:/¿Î³Ì/ESE5023_Assignments/HW5")
getwd()
#1.1 Download the following data sets and load them in R
# Read Solar radiation tiff file(NOv)
Srad_Nov <- raster("wc2.1_2.5m_srad_11.tif")
# Look at the raster attributes
Srad_Nov
# Read Precipitation tiff file(NOv)
Prec_Nov <- raster("wc2.1_2.5m_prec_11.tif")
# Look at the raster attributes
Prec_Nov
# Read Wind speed tiff file(NOv)
Wind_Nov <- raster("wc2.1_2.5m_wind_11.tif")
# Look at the raster attributes
Wind_Nov
#1.2 Plot the above data sets over China. You should make three plots, each should contain its own legend.
China_map <- readOGR("E://¿Î³Ì/ESE5023_Assignments/HW5/China_map", "bou2_4p") 
plot(China_map)
#plot Solar radiation in Nov.
plot(Srad_Nov, main="Solar radiation in Nov.")
Srad_Nov_crop <- crop(Srad_Nov, China_map)
Srad_Nov_china <- Srad_Nov_crop %>%
  mask(China_map, na.rm=TRUE)
plot(Srad_Nov_china, main="Solar radiation in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="kJ/m2/day",cex=1.25))
plot(China_map, add=T)
#plot Precipitation in Nov.
plot(Prec_Nov, main="Precipitation in Nov.")
Prec_Nov_crop <- crop(Prec_Nov, China_map)
Prec_Nov_china <- Prec_Nov_crop %>%
  mask(China_map, na.rm=TRUE)
plot(Prec_Nov_china, main="Precipitation in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="mm",cex=1.25))
plot(China_map, add=T)
#plot Wind speed in Nov.
plot(Wind_Nov, main="Wind speed in Nov.")
Wind_Nov_crop <- crop(Wind_Nov, China_map)
Wind_Nov_china <- Wind_Nov_crop %>%
  mask(China_map, na.rm=TRUE)
plot(Wind_Nov_china, main="Wind speed in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="m/s",cex=1.25))
plot(China_map, add=T)
contour(Wind_Nov_china, add=T, col="red", nlevels=4)
#1.3 First, let¡¯s search for regions with relatively high wind speed to build wind farms. 
#Define a reasonable wind speed as the threshold, and describe your favorite spots.
#Discuss with Yue Hou
plot(Wind_Nov_china, main="Wind speed in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="m/s",cex=1.25))
contour(Wind_Nov_china, add=T, col="blue", levels = seq(0, 8, by=4), labcex=1)
plot(Wind_Nov_china, main="Wind speed in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="m/s",cex=1.25))
plot(China_map, add=T)
contour(Wind_Nov_china, add=T, col="blue", levels = seq(0, 8, by=4), labcex=1)
#1.4 Second, let¡¯s search for regions with relatively high solar radiation and low precipitation as potential locations of photovoltaics (PV) farms. 
#Describe your favorite spots of PV farms.
#Discuss with Yue Hou
plot(Srad_Nov_china, main="Solar radiation in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="kJ/m2/day",cex=1.25))
contour(Srad_Nov_china, add=T, col="red", levels = seq(4000, 16000, by=4000), labcex=1)
plot(Srad_Nov_china, main="Solar radiation in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="kJ/m2/day",cex=1.25))
plot(China_map, add=T)
contour(Srad_Nov_china, add=T, col="red", levels = seq(4000, 16000, by=4000), labcex=1)
plot(Prec_Nov_china, main="Precipitation in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="mm",cex=1.25))
contour(Prec_Nov_china, add=T, col="red", levels = seq(0, 300, by=10), labcex=1)
plot(Prec_Nov_china, main="Precipitation in Nov.",xlab="longitude", ylab="latitude", cex.lab=1.5, cex.main=2, legend.args=list(text="mm",cex=1.25))
plot(China_map, add=T)
contour(Prec_Nov_china, add=T, col="red", levels = seq(0, 300, by=10), labcex=1)
