
#### libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)

#### data directory ----
#sapflow and sensor data parent directory
dirData <- "K:/Environmental_Studies/hkropp/Data/campus/buckthorn/sapflux"

#sapflow download date for file
sversion <- "07_16_2021"


#### read in data ----
sapRaw <- read.csv(paste0(dirData,"/campbell/",sversion,"/Sapflow_TableDT.dat"),
                    header=FALSE,skip=4,na.strings=c("NAN"))
#remove unused sensor locations
datSap <- sapRaw[,1:18]
#rename columns
colnames(datSap ) <- c("date","record",paste0("dT",seq(1,16)))
#parse date
datSap$dateF <- ymd_hms(datSap$date)
datSap$year <- year(datSap$dateF)
datSap$doy <- yday(datSap$dateF)
datSap$hour <- hour(datSap$dateF)+(minute(datSap$dateF)/60)
datSap$DD <- datSap$doy + (datSap$hour/24)



#### initial plots ----

sensors <- read.csv("K://Environmental_Studies/hkropp/Data/campus/buckthorn/sapflux/sensors_meta.csv")

#add sapwood to sensors
#ash allometry from Zeima Kassahun, Heidi J. Renninger 2021 Ag & Forest Met
sensors$sd.cm <- ifelse(sensors$Type == "Ash", #if sensors is ash
                        -36.33 + (44.28*(1-exp(-0.1306*sensors$DBH.cm))),#allometry
                        1)#if buckthorn fill place with 1 cm placeholder until allometry is fully measured

#organize data for easier calculations
tabledt <- datSap


dtAll <- data.frame(date= rep(tabledt$date, times = 16), 
                    doy = rep(tabledt$doy, times = 16),
                    hour = rep(tabledt$hour, times = 16),
                    DD = rep(tabledt$DD, times = 16),
                    sensor = rep(seq(1,16), each = nrow(tabledt)), 
                    dT = c(tabledt[,3],
                           tabledt[,4],
                           tabledt[,5],
                           tabledt[,6],
                           tabledt[,7],
                           tabledt[,8],
                           tabledt[,9],
                           tabledt[,10],
                           tabledt[,11],
                           tabledt[,12],
                           tabledt[,13],
                           tabledt[,14],
                           tabledt[,15],
                           tabledt[,16],
                           tabledt[,17],
                           tabledt[,18]))



#join sensor info into table dt
#make a doy that contains the same night
#so new day actually starts at 5 am not midnight
dtAll$doy5 <- ifelse(dtAll$hour < 5, dtAll$doy-1,dtAll$doy)

night <- dtAll[dtAll$hour < 5|dtAll$hour >= 22,]

#filter night so maximum in day and sensor is provided
maxnight <- night %>% 
  group_by(sensor, doy5) %>%
  filter(dT == max(dT),na.rm=TRUE)
#remove duplicate maximums that occur for longer than 15 min
maxnight <- maxnight[!duplicated(maxnight$dT),]

ggplot(maxnight, aes(doy5,dT, color=sensor))+
  geom_point()
#isolate max and join back into table
maxJoin <- data.frame(sensor=maxnight$sensor, 
                      doy5=maxnight$doy5,
                      maxDT = maxnight$dT)

#join backinto tabledt
dtCalct1 <- left_join(dtAll, maxJoin, by=c("sensor","doy5"))
#join sensor info
dtCalc <- left_join(dtCalct1 , sensors, by=c("sensor"="SensorID"))

#from clearwater

#sap velocity mm s-1 (v)
#v = 0.119*k^1.231
#flow is F (L s-1) = v* A (m2, sapwood area)

#K= (dTmax - dT)/dT if sensor is fully within sapwood

#otherwise correction is:
#dt sap = (dT - b* Dtmax)/a

#a = proportion of probe in sapwood and b=1-a

dtCalc$a <- ifelse(dtCalc$sd.cm >= 3,1,
                   dtCalc$sd.cm/3)

dtCalc$b <- 1 - dtCalc$a 

dtCalc$dTCor <- (dtCalc$dT - (dtCalc$b * dtCalc$maxDT))/dtCalc$a
dtCalc$K <- (dtCalc$maxDT - dtCalc$dTCor)/dtCalc$dTCor
dtCalc$velo <- 0.119*(dtCalc$K^1.231)


################
#To do

#need sapwood area allometry from paper
#calculate sapflow in volume per time
#filter out days with voltage regulator down
#filter outliers that exceed 
dtCalc <- dtCalc[ dtCalc$velo <0.5,]

ggplot(dtCalc[dtCalc$sensor ==1,], aes(x=DD,y=velo))+ 
  geom_point()

ggplot(dtCalc[dtCalc$sensor ==2,], aes(x=DD,y=velo))+ 
  geom_point()

ggplot(dtCalc[dtCalc$sensor ==3,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==4,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==5,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==6,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==7,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==8,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==9,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==10,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==11,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==12,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==13,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==14,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==15,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==16,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
