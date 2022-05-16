# create plots for climate data from selected simU points throughout Nepal
#
# Author: Luisa Gensch

#simU points chosen are 5899 (Jhapa- Eastern Terai), 4970 (Taplejung- Eastern Mountain), 5872 (Ilam- Eastern Hill)
#393 (Darchula- Western Mountain), 1200 (Dadeldhura- Western Hill), 1662 (Kanchanpur- Western Terai)
#3577 (Tanahu- Middle Hill), 3947 (Sindhupalchok- Middle Mountain), 3986 (Chitwan- Middle Terai)
# ------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ncdf4)
library(chron)
library(gdata)
library(sp)
library(geosphere)
library(RANN)
library(lattice)
library(rgdal)


#check daily files against weather station data
#--------------------------------------------------------------------------------------------------------
tmax <- read.csv("C:/Users/u244061/Desktop/Thang/weatherstation/Nepal station data/tmax.csv")
tmin <- read.csv("C:/Users/u244061/Desktop/Thang/weatherstation/Nepal station data/tmin.csv")

rain <- read.csv("C:/Users/u244061/Desktop/Thang/weatherstation/Nepal station data/rain.csv")
rh <- read.csv("C:/Users/u244061/Desktop/Thang/weatherstation/Nepal station data/rh.csv")

#Kailali: simU point 857
subset_tmax <- tmax[1:6572,]
subset_tmin <- tmin[1:6572,]
subset_rain <- rain[1:6572,]
subset_rh <- rh[1:6572,]


a <- cbind(subset_tmax[,"year"], subset_tmax[,"month"], 
           subset_tmax[,"day"],
           sprintf("%04.2f",0), 
           sprintf("%04.2f",subset_tmax[,"max_temp"]),
           sprintf("%04.2f",subset_tmin[,"min_temp"]),
           sprintf("%04.2f",subset_rain[,"rainfall_sum"]),
           sprintf("%04.2f",subset_rh[,"rh"]/100),
           sprintf("%04.2f",0))

write.fwf(a, file = "857.dly",rownames=FALSE,colnames=FALSE, width=c(6,4,4,6,6,6,6,6,6), eol="\n", sep="")




#Taplejung 4796

subset_tmax <- subset(tmax, district == "Taplejung", select = c("gsid","index_no","station","district","year","month","day","max_temp"))
subset_tmin <- subset(tmin, district == "Taplejung", select = c("gsid","index_no","station","district","year","month","day","min_temp"))
subset_rain <- subset(rain, district == "Taplejung", select = c("gsid","index_no","station","district","year","month","days","rainfall_sum"))
subset_rh <- subset(rh, district == "Taplejung", select = c("gsid","index_no","station","district","year","month","days","rh"))


b <- merge(subset_tmax,subset_tmin, by=c("year","month","day"),all= TRUE) # NA's match
b[is.na(b)] <- 0
b <-b[c("year","month","day","gsid.x","index_no.x","station.x","district.x","max_temp","min_temp")]
colnames(subset_rain)[7]<-paste("day") 
b <- merge(subset_rain,b, by=c("year","month","day"),all= TRUE) # NA's match
b[is.na(b)] <- 0
b <-b[c("year","month","day","gsid.x","index_no.x","station.x","district.x","rainfall_sum","max_temp","min_temp")]
a <- cbind(b[,"year"], b[,"month"], 
           b[,"day"],
           sprintf("%04.2f",0), 
           sprintf("%04.2f",b[,"max_temp"]),
           sprintf("%04.2f",b[,"min_temp"]),
           sprintf("%04.2f",b[,"rainfall_sum"]),
           sprintf("%04.2f",0),
           sprintf("%04.2f",0))
write.fwf(a, file = "4796.dly",rownames=FALSE,colnames=FALSE, width=c(6,4,4,6,6,6,6,6,6), eol="\n", sep="")

#Gorkha 1422

subset_tmax <- subset(tmax, district == "Gorkha", select = c("gsid","index_no","station","district","year","month","day","max_temp"))
subset_tmin <- subset(tmin, district == "Gorkha", select = c("gsid","index_no","station","district","year","month","day","min_temp"))
subset_rain <- subset(rain, district == "Gorkha", select = c("gsid","index_no","station","district","year","month","days","rainfall_sum"))
subset_rh <- subset(rh, district == "Gorkha", select = c("gsid","index_no","station","district","year","month","days","rh"))


b <- merge(subset_tmax,subset_tmin, by=c("year","month","day"),all= TRUE) # NA's match
b[is.na(b)] <- 0
b <-b[c("year","month","day","gsid.x","index_no.x","station.x","district.x","max_temp","min_temp")]
colnames(subset_rain)[7]<-paste("day") 
b <- merge(subset_rain,b, by=c("year","month","day"),all= TRUE) # NA's match
b[is.na(b)] <- 0
b <-b[c("year","month","day","gsid.x","index_no.x","station.x","district.x","rainfall_sum","max_temp","min_temp")]
a <- cbind(b[,"year"], b[,"month"], 
           b[,"day"],
           sprintf("%04.2f",0), 
           sprintf("%04.2f",b[,"max_temp"]),
           sprintf("%04.2f",b[,"min_temp"]),
           sprintf("%04.2f",b[,"rainfall_sum"]),
           sprintf("%04.2f",0),
           sprintf("%04.2f",0))
write.fwf(a, file = "1422.dly",rownames=FALSE,colnames=FALSE, width=c(6,4,4,6,6,6,6,6,6), eol="\n", sep="")

#Rupandehi 4938

tmax <- read.csv("C:/Users/u244061/Desktop/Thang/weatherstation/Nepal station data/climatedata/tem_max (1).csv")
tmin <- read.csv("C:/Users/u244061/Desktop/Thang/weatherstation/Nepal station data/climatedata/tem_ min (1).csv")

rain <- read.csv("C:/Users/u244061/Desktop/Thang/weatherstation/Nepal station data/climatedata/rain (1).csv")


subset_tmax <- subset(tmax, district == "Rupandehi", select = c("gsid","index_no","station","district","year","month","day","max_temp"))
subset_tmin <- subset(tmin, district == "Rupandehi", select = c("gsid","index_no","station","district","year","month","day","min_temp"))
subset_rain <- subset(rain, district == "Rupandehi", select = c("gsid","index_no","station","district","year","month","days","rainfall_sum"))



b <- merge(subset_tmax,subset_tmin, by=c("year","month","day"),all= TRUE) # NA's match
b[is.na(b)] <- 0
b <-b[c("year","month","day","gsid.x","index_no.x","station.x","district.x","max_temp","min_temp")]
colnames(subset_rain)[7]<-paste("day") 
b <- merge(subset_rain,b, by=c("year","month","day"),all= TRUE) # NA's match
b[is.na(b)] <- 0
b <-b[c("year","month","day","gsid.x","index_no.x","station.x","district.x","rainfall_sum","max_temp","min_temp")]
a <- cbind(b[,"year"], b[,"month"], 
           b[,"day"],
           sprintf("%04.2f",0), 
           sprintf("%04.2f",b[,"max_temp"]),
           sprintf("%04.2f",b[,"min_temp"]),
           sprintf("%04.2f",b[,"rainfall_sum"]),
           sprintf("%04.2f",0),
           sprintf("%04.2f",0))
write.fwf(a, file = "4938.dly",rownames=FALSE,colnames=FALSE, width=c(6,4,4,6,6,6,6,6,6), eol="\n", sep="")

