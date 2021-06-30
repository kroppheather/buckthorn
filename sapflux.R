
#### libraries ----
library(lubridate)
library(ggplot2)

#### data directory ----
#sapflow and sensor data parent directory
dirData <- "K:/Environmental_Studies/hkropp/Data/campus/buckthorn/sapflux"

#sapflow download date for file
sversion <- "06_29_2021"


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
plotFrame <- datSap[datSap$record > 106 ,]


ggplot(plotFrame, aes(dateF, dT16))+
  geom_path()
