###############################################
###############################################
###############################################
### Visualization for AGU poster 2021       ###
###############################################
###############################################
###############################################
### Uses calculations from sapflux.R to get ###
### the data frames described below:        ###
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
### L s-1 flow rate at each 15 min increment###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################
### ash.Flow.m2 & buckthorn.Flow.m2         ###
### L m-2 s-1 flow rater per m2 of canopy   ###
### at each 15 min increment                ###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################
### buckthorn.L.m2.day & ash.L.m2.day       ###
### total water use in L per day per m2 of  ###
### canopy leaf area                        ###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################
### buckthorn.L.day & ash.L.day             ###
### total water use in L per day per tree   ###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################
### Uses calculations from weather.R to get ###
### the data frames described below:        ###
###############################################
### meterTable & meterMeta:                 ###
### 15 min obs of weather                   ###
###############################################
### control & removal & weather             ###
### observations from TMS 4 at each location###
###############################################
### TMSbind                                 ###
### all TMS in same data frame              ###
###############################################

source("c:/Users/hkropp/Documents/GitHub/buckthorn/sapflux.r")
source("c:/Users/hkropp/Documents/GitHub/buckthorn/weather.r")
source("c:/Users/hkropp/Documents/GitHub/buckthorn/spatial_process.r")
##############################
#### output directory ### ----

outDir <- "E:/Google Drive/research/projects/campus/buckthorn/AGU21/figures"


##############################
#### daily T per unit leaf ----


range(ash.L.m2.day$mean)
range(buckthorn.L.m2.day$mean)

range(ash.L.m2.day$doy)
range(buckthorn.L.m2.day$doy)

plot(c(0,0), c(0,0), ylim=c(0,0.5),
     xlim=c(165,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")
points(ash.L.m2.day$doy[ash.L.m2.day$Removal == "C"],
  ash.L.m2.day$mean[ash.L.m2.day$Removal == "C"],
  pch=19, col=rgb(0,114,178,155,maxColorValue=255),
  type="b")
points(ash.L.m2.day$doy[ash.L.m2.day$Removal == "R"],
       ash.L.m2.day$mean[ash.L.m2.day$Removal == "R"],
       pch=19, col=rgb(213,94,0,155,maxColorValue=255),
       type="b")

points(buckthorn.L.m2.day$doy,
       buckthorn.L.m2.day$mean,
       pch=19, col=rgb(0,158,115,155,maxColorValue=255),
       type="b")

axis(1, seq(170,270, by=10))
axis(2, seq(0,0.5, by=0.1), las=2)


##############################
#### flow per unit leaf ----

ash.Fsub <- ash.Flow.m2[ash.Flow.m2$doy >= 229 &ash.Flow.m2$doy <= 240, ]
ash.Fsub$ml.m2.s <- ash.Fsub$mean *1000

range(ash.Flow.m2$mean)*1000
range(buckthorn.Flow.m2$mean)

plot(ash.Fsub$DD[ash.Fsub$Removal == "C"],
     ash.Fsub$ml.m2.s[ash.Fsub$Removal == "C"],
     pch=19, col=rgb(0,114,178,155,maxColorValue=255), type="b",
     ylim=c(0,0.01))
points(ash.Fsub$DD[ash.Fsub$Removal == "R"],
       ash.Fsub$ml.m2.s[ash.Fsub$Removal == "R"],
       pch=19, col=rgb(213,94,0,155,maxColorValue=255), type="b")

##############################
#### met with flow per unit leaf ----

meterSub <- na.omit(meterTable[meterTable$doy  >= 170 & meterTable$doy <= 270 & meterTable$year == 2021, ])

plot(meterSub$DD, meterSub$SolRad,
     pch=19, col=rgb(0,114,178,155,maxColorValue=255), type="b")
plot(meterSub$DD, meterSub$VPD,
     pch=19, col=rgb(0,114,178,155,maxColorValue=255), type="b")

plot(meterSub$DD, meterSub$Precip,
     pch=19, col=rgb(0,114,178,155,maxColorValue=255), type="b")
