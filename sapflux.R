###############################################
### Sapflow calculations for bucktorn       ###
### removal experiment. The following data  ###
### frames of interest are outlined below   ###
###############################################
###############################################
### ash.tree & buckthorn.tree:              ###  
### raw measurements and calculations       ###
###############################################
### ash.Flow & buckthorn.Flow:              ###
### L s-1 & m-2 flow rate at each hour      ###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################
###############################################
###############################################
### buckthorn.L.day & ash.L.day             ###
### total water use in L per day  (& m-2)   ###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################






#### libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)


##############################
#### data directory #### ----
userNumber <- 1
#sapflow and sensor data parent directory
dirData <- c("G:/My Drive/research/projects/campus/buckthorn/sapflux",#windows office
             "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/campus/buckthorn/sapflux") # teaching mac
dirWeather <- c("G:/My Drive/research/projects/Data/campus_weather/METER/",
                "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/Data/campus_weather/METER/")

#sapflow download date for file
sversion <- "09_24_2021"

##############################
#### read in data ###### ----
#dT sapflow
sapRaw <- read.csv(paste0(dirData[userNumber],"/campbell/",sversion,"/Sapflow_TableDT.dat"),
                    header=FALSE,skip=4,na.strings=c("NAN"))
#column names
sapInfo <- read.csv(paste0(dirData[userNumber],"/campbell/",sversion,"/Sapflow_TableTC.dat"),
                   header=FALSE,skip=4,na.strings=c("NAN"))

#green ash allometrry
greenwood <- read.csv(paste0(dirData[userNumber],"/green ash olson paper measurements.csv"))
#sapwood allometry
buckthornSW <- read.csv(paste0(dirData[userNumber],"/buckthorn_allometry_info.csv"))
#SLA
buckthornSLA <- read.csv(paste0(dirData[userNumber],"/leaf area.csv"))
#buchthorn dbh and leaf allom
buckthornLA <- read.csv(paste0(dirData[userNumber],"/buckthorn_leaf_allom.csv"))
#list of buckthorn removed with dbh
buckthornRemove <- read.csv(paste0(dirData[userNumber],"/buckthorn_dbh.csv"))
#read in sensor info

sensors <- read.csv(paste0(dirData[userNumber],"/sensors_meta.csv"))


##############################
#### organize sap flow ----
heaterv <- data.frame(date =  ymd_hms(sapInfo[,1]),
                      ht1 = sapInfo[,165],
                      ht2 = sapInfo[,166])
heaterv$year <- year(heaterv$date)
heaterv$doy <- yday(heaterv$date)
heaterv$hour <- hour(heaterv$date)+(minute(heaterv$date)/60)

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

##############################
#### sapwood depth Allometry ----


# check sapwood depth for sensors


# buckthorn sapwood allometry
# dbh vs sapwood 
# plot(buckthornSW$DBH.cm, buckthornSW$Sapwood.mm/10, pch=19)
# linear regression sap
bsap.lm <- lm(buckthornSW$Sapwood.mm/10 ~ buckthornSW$DBH.cm )
summary(bsap.lm)

bsap.calc <- mean( buckthornSW$Sapwood.mm/10)


#dbh vs bark thickness
# plot(buckthornSW$DBH.cm, buckthornSW$bark.mm, pch=19)
# bbark.lm <- lm(buckthornSW$bark.mm/10 ~ buckthornSW$DBH.cm )
# summary(bbark.lm)
# bark relationship not significant
# assume mean
bbark.calc <- mean( buckthornSW$bark.mm/10)


# ash sapwood depth allometry from Zeima Kassahun, Heidi J. Renninger 2021 Ag & Forest Met
sensors$sd.cm <- ifelse(sensors$Type == "Ash", #if sensors is ash
                        -36.33 + (44.28*(1-exp(-0.1306*sensors$DBH.cm))),#allometry
                        bsap.calc)#if buckthorn fill place with 1 cm placeholder until allometry is fully measured




##############################
#### dT to v calcs ##### ----



# organize data for easier calculations
tabledtF <- datSap



dtAll1 <- data.frame(date= rep(tabledtF$date, times = 16), 
                    doy = rep(tabledtF$doy, times = 16),
                    hourD = rep(tabledtF$hour, times = 16),
                    DD = rep(tabledtF$DD, times = 16),
                    sensor = rep(seq(1,16), each = nrow(tabledtF)), 
                    dT = c(tabledtF[,3],
                           tabledtF[,4],
                           tabledtF[,5],
                           tabledtF[,6],
                           tabledtF[,7],
                           tabledtF[,8],
                           tabledtF[,9],
                           tabledtF[,10],
                           tabledtF[,11],
                           tabledtF[,12],
                           tabledtF[,13],
                           tabledtF[,14],
                           tabledtF[,15],
                           tabledtF[,16],
                           tabledtF[,17],
                           tabledtF[,18]))
# calculate hourly average
dtAll1$hour <- floor(dtAll1$hourD)

dT_hour <- na.omit(dtAll1) %>%
  group_by(doy, hour, sensor) %>%
  summarise(mean.dT = mean(dT),
            n.Dt = length(dT))

dT_hour$dT <- dT_hour$mean.dT

# filter out unreliable data due to voltage regulator issues


#### QC filter 1    #   
#filter out days when voltage regulator was unreliable
#either too variable or heaters turned off at any point
# filter out based on day given the amount of missing data
dtAll <- dT_hour %>%
  filter(doy > 191)


#################
#check for dt outliers
 quantile(dtAll$mean.dT, prob=seq(0,1,by=0.001), na.rm=TRUE)


#join sensor info into table dt
#make a doy that contains the same night
#so new day actually starts at 5 am not midnight


night <- dtAll %>%
  filter(hour < 5)

#filter night so maximum in day and sensor is provided
maxnight1 <- night %>% 
  group_by(sensor, doy) %>%
  filter(dT == max(dT),na.rm=TRUE)

#remove duplicate maximums that occur for longer than 15 min
#just take earliest measurement
maxnight <- maxnight1  %>% 
  group_by(sensor, doy) %>%
  filter(hour == min(hour),na.rm=TRUE)

ggplot(maxnight, aes(doy,dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

# isolate max and join back into table
maxJoin <- data.frame(sensor=maxnight$sensor, 
                      doy=maxnight$doy,
                      maxDT = maxnight$dT)

# join backinto tabledt
dtCalct1 <- left_join(dtAll, maxJoin, by=c("sensor","doy"))
# join sensor info
dtCalc <- left_join(dtCalct1 , sensors, by=c("sensor"="SensorID"))

# get mean hourly dT
#summary table
#flow L s every hour by tree

# from clearwater

#sap velocity m s-1 (v)
#v = 0.000119*k^1.231
#flow is F (L s-1) = v* A (m2, sapwood area)

#K= (dTmax - dT)/dT if sensor is fully within sapwood

# otherwise correction is:
# dt sap = (dT - b* Dtmax)/a

# a = proportion of probe in sapwood and b=1-a

dtCalc$a <- ifelse(dtCalc$sd.cm >= 3,1,
                   dtCalc$sd.cm/3)

dtCalc$b <- 1 - dtCalc$a 

dtCalc$dTCor <- (dtCalc$dT - (dtCalc$b * dtCalc$maxDT))/dtCalc$a
dtCalc$K <- (dtCalc$maxDT - dtCalc$dTCor)/dtCalc$dTCor
dtCalc$velo <- 0.000119*(dtCalc$K^1.231)

ggplot(dtCalc %>% filter(sensor==10), aes(doy + (hour/24),velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

#### Quantile filter: some days look like rainfall may have 
### caused issues resulting in spikes in sap flow. Apply quanitile filter for sensors 7 & 10
### 98% for sensor 10. sensor 7 was very prone to outliers on rainy days so went with 97
quantile(dtCalc$velo[dtCalc$sensor == 7], prob=seq(0,1,by=0.01), na.rm=TRUE)
quantile(dtCalc$velo[dtCalc$sensor == 10], prob=seq(0,1,by=0.01), na.rm=TRUE)

dtCalc$FlagQA <- ifelse(dtCalc$velo >= 1e-04 & dtCalc$sensor == 7,1,
                        ifelse(dtCalc$velo >= 4.5e-05 & dtCalc$sensor == 10,1,
                               ifelse(dtCalc$velo >= 1e-04 & dtCalc$sensor == 16,1, 0)))
                        
dtCalcF <- dtCalc %>% 
  filter(FlagQA == 0)

ggplot(dtCalcF  , aes(doy + (hour/24),velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()


# separate species types
ash <- dtCalcF %>%
  filter(Type == "Ash")
buckthorn <- dtCalcF %>%
  filter(Type == "buckthorn")


ggplot(buckthorn , aes(doy + (hour/24),velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()
ggplot(ash , aes(doy + (hour/24),velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

##############################
#### N & S radial check   ----

# compare N & S sensors for ash
sens3 <- data.frame(doy = ash$doy[ash$sensor == 3],
                    hour = ash$hour[ash$sensor == 3],
                    veloN = ash$velo[ash$sensor == 3])
                     
sens4 <- data.frame(doy = ash$doy[ash$sensor == 4],
                    hour = ash$hour[ash$sensor == 4],
                    veloS = ash$velo[ash$sensor == 4])

treeD1 <- inner_join(sens3,sens4, by=c("doy", "hour"))

#compare N & S sensors for ash
sens12 <- data.frame(doy = ash$doy[ash$sensor == 12],
                     hour = ash$hour[ash$sensor == 12],
                    veloN = ash$velo[ash$sensor == 12])

sens11 <- data.frame(doy = ash$doy[ash$sensor == 11],
                     hour = ash$hour[ash$sensor == 11],
                    veloS = ash$velo[ash$sensor == 11])

treeD2 <- inner_join(sens12,sens11,  by=c("doy", "hour"))

sens15 <- data.frame(doy = ash$doy[ash$sensor == 15],
                     hour = ash$hour[ash$sensor == 15],
                     veloN = ash$velo[ash$sensor == 15])

sens16 <- data.frame(doy = ash$doy[ash$sensor == 16],
                     hour = ash$hour[ash$sensor == 16],
                     veloS = ash$velo[ash$sensor == 16])

treeD3 <- inner_join(sens15,sens16, by=c("doy", "hour"))

treeDir <- rbind(treeD1,treeD2,treeD3)


#check relationship
azim.rel <- lm(treeDir$veloS ~ treeDir$veloN)
 summary(azim.rel)

ggplot(treeDir, aes(veloN,veloS))+
  geom_point()+
  geom_abline()

#regression does not differ significantly from S=0 + 1*N

# check buckthorn


sens7 <- data.frame(doy = buckthorn$doy[buckthorn$sensor == 7],
                    hour = buckthorn$hour[buckthorn$sensor == 7],
                    veloN = buckthorn$velo[buckthorn$sensor == 7])

sens9 <- data.frame(doy = buckthorn$doy[buckthorn$sensor == 9],
                    hour = buckthorn$hour[buckthorn$sensor == 9],
                    veloS = buckthorn$velo[buckthorn$sensor == 9])

treeB1 <- inner_join(sens3,sens4,by=c("doy", "hour"))

sens8 <- data.frame(doy = buckthorn$doy[buckthorn$sensor == 8],
                    hour = buckthorn$hour[buckthorn$sensor == 8],
                     veloN = buckthorn$velo[buckthorn$sensor == 8])

sens10 <- data.frame(doy = buckthorn$doy[buckthorn$sensor == 10],
                     hour = buckthorn$hour[buckthorn$sensor == 10],
                     veloS = buckthorn$velo[buckthorn$sensor == 10])

treeB2 <- inner_join(sens8,sens10, by=c("doy", "hour"))

treeBDir <- rbind(treeB1,treeB2)

treeBDir <- treeBDir %>%
  filter(veloN <= 0.00007)

azimB.rel <- lm(treeBDir$veloS ~ treeBDir$veloN)
 summary(azimB.rel)
 
plot(treeBDir$veloN, treeBDir$veloS)
abline(azimB.rel)
abline(0,1, col="red")


#regression has  a lot of noise 
# and variation around the 1:1 line. The slope coefficient
# does not explain enough variability to warrent a difference
# from the 1:1 line.

#use N for final data
ash.tree <- ash %>%
  filter(Direction == "N")

buckthorn.tree <- buckthorn %>%
  filter(Direction == "N")




##############################
#### canopy leaf allometry   ----


#Ash allometry from literature
greenwood$sap.area <- greenwood$Sapwood.Volume..ft.3./greenwood$Total.Height..feet
#30.48 cm in 1 foot
greenwood$sap.area.cm <- 30.48*30.48*greenwood$sap.area 
greenwood$dbh.cm <- (greenwood$DBH..inches.*2.54)
greenwood$treeArea <- ((greenwood$dbh.cm /2)^2)*pi

#plot(greenwood$dbh.cm,greenwood$sap.area.cm)

#saparea.reg <- lm(greenwood$sap.area.cm ~ greenwood$dbh.cm)
#summary(saparea.reg)


#sap cm2 = -9.6 + 8.854*DBH cm


##############################
#### Canopy calculations   ----

## sapwood arrea

# ash tree
ash.tree$sap.areacm2 <- -9.6 + 8.854*ash.tree$DBH.cm
#convert sap area to m2
ash.tree$sap.aream2 <- 0.0001*ash.tree$sap.areacm2

# buckthorn

#calculate heartwood
buckthorn.tree$Htwd <- buckthorn.tree$DBH.cm - (bbark.calc*2) - (bsap.calc*2)



#calculate sapwood area

buckthorn.tree$sap.areacm2 <- (pi*(((bsap.calc/2)+(buckthorn.tree$Htwd/2))^2))-(pi*((buckthorn.tree$Htwd/2)^2))
buckthorn.tree$sap.aream2 <-  0.0001*buckthorn.tree$sap.areacm2
## tree leaf area
#meadows paper
#LA (m2) = -66.185 +  6.579*DBH in cm
ash.tree$LA.m2 <- -66.185 +  6.579*ash.tree$DBH.cm

# buckthorn

#Leaf Area/ leaf Mass (cm2 / g)
buckthorn.SLA <- mean(buckthornSLA$area.cm2/buckthornSLA$weight.g)
buckthorn.SLA.m2kg <- (buckthorn.SLA*1000)*(1/100)*(1/100)


# use Mascaro allometry

#estimate leaf area in m2
buckthorn.tree$LA.m2 <- (0.0287 *buckthorn.tree$DBH.cm ^1.6046)*buckthorn.SLA.m2kg


##############################
#### Flow calculations   ----

#flow rate according to clearwater
#F(L s-1) =  v(m s-1)* A (m2)

ash.tree$Flow.m3.s <- ash.tree$velo * ash.tree$sap.aream2

buckthorn.tree$Flow.m3.s <- buckthorn.tree$velo * buckthorn.tree$sap.aream2

#convert to L per secton

ash.tree$Flow.L.s <- ash.tree$Flow.m3.s * 1000

ggplot(ash.tree, aes(doy + (hour/24), Flow.L.s, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

buckthorn.tree$Flow.L.s <- buckthorn.tree$Flow.m3.s * 1000

ggplot(buckthorn.tree, aes(doy + (hour/24), Flow.L.s, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

#normalize by canopy leaf area
ash.tree$Flow.L.m2.s <- ash.tree$Flow.L.s /ash.tree$LA.m2 

buckthorn.tree$Flow.L.m2.s <- buckthorn.tree$Flow.L.s /buckthorn.tree$LA.m2 

#summarize total per day for each tree
#remove NA
ash.treeNN <- ash.tree %>%
  filter(is.na(Flow.L.s)==FALSE)


#summarize total per day for each tree
#remove NA
buckthorn.treeNN <- buckthorn.tree %>%
  filter(is.na(Flow.L.s)==FALSE)


##############################
#### Summary tables    ----
 
# summarize hourly data by treatment           

buckthorn.hour <- buckthorn.treeNN %>%
  group_by(doy, hour, Removal) %>%
  summarise(mh.L.s = mean(Flow.L.s),
            sdh.L.s=sd(Flow.L.s), 
            nh.L.s=length(Flow.L.s),
            mh.L.m2.s = mean(Flow.L.m2.s),
            sdh.L.m2.s=sd(Flow.L.m2.s), 
            nh.L.m2.s=length(Flow.L.m2.s))%>%
  filter(nh.L.s >=3)

ggplot(buckthorn.hour, aes(doy + (hour/24), mh.L.m2.s, color=Removal))+
  geom_point()+
  geom_line()  

ash.hour <- ash.treeNN %>%
  group_by(doy, hour, Removal) %>%
  summarise(mh.L.s = mean(Flow.L.s),
            sdh.L.s=sd(Flow.L.s), 
            nh.L.s=length(Flow.L.s),
            mh.L.m2.s = mean(Flow.L.m2.s),
            sdh.L.m2.s=sd(Flow.L.m2.s), 
            nh.L.m2.s=length(Flow.L.m2.s))%>%
  filter(nh.L.s >=3)

ggplot(ash.hour, aes(doy + (hour/24), mh.L.m2.s, color=Removal))+
  geom_point()+
  geom_line() 

ggplot(ash.hour %>% filter(doy>208 & doy <220), aes(doy + (hour/24), mh.L.m2.s, color=Removal))+
  geom_point()+
  geom_line()  


#total liters per day used by each tree per day
ash.L.sens <- ash.treeNN %>%
  group_by(doy, Removal, TreeID) %>%
  summarise(L.day1 = sum(Flow.L.s*60*60 ), # sum up L per hour
            n.day1=length(Flow.L.s),
            L.day1.m2 = sum(Flow.L.m2.s*60*60 )) %>%
  filter(n.day1 >= 23)

buckthorn.L.sens <- buckthorn.treeNN %>%
  group_by(doy, Removal, TreeID) %>%
  summarise(L.day1 = sum(Flow.L.s*60*60 ), # sum up L per hour
            n.day1=length(Flow.L.s),
            L.day1.m2 = sum(Flow.L.m2.s*60*60 )) %>%
  filter(n.day1 >= 23)
  


# average daily transpiration by species and plot
ash.L.day <- ash.L.sens %>%
  group_by(doy, Removal) %>%
  summarise(L.day = mean(L.day1),
            n.day = length(L.day1),
            sd.day = sd(L.day1),
            L.m2.day = mean(L.day1.m2),
            sd.m2.day = sd(L.day1.m2)) %>%
  filter(n.day >=3)

ggplot(ash.L.day, aes(doy, L.m2.day, color=Removal))+
  geom_point()+
  geom_line()

buckthorn.L.day <- buckthorn.L.sens %>%
  group_by(doy, Removal) %>%
  summarise(L.day = mean(L.day1),
            n.day = length(L.day1),
            sd.day = sd(L.day1),
            L.m2.day = mean(L.day1.m2),
            sd.m2.day = sd(L.day1.m2)) %>%
  filter(n.day >=3)

ggplot(buckthorn.L.day, aes(doy, L.day))+
  geom_point()+
  geom_line()


rm(list=setdiff(ls(), c("ash.hour","buckthorn.hour", 
                        "buckthorn.L.day", "ash.L.day",  
                        "weather")))
                        
