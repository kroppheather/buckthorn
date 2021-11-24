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
#### Standard plot argument ----

#plot colors

pt.cols <- c(rgb(0,114,178,155,maxColorValue=255), #control ash
             rgb(213,94,0,155,maxColorValue=255), #removal ash
             rgb(0,158,115,155,maxColorValue=255)) #buckthon control

wd <- 16*2.54
hd <- 4*2.54

#point cex
pt.c <- 4
#line thickness
ln.w <- 3

#line thickness for line only
lln.w <- 4
#tick lwd
tlw <- 3
#axis tick label size
alc <- 2.5
#  axis label size
llc <- 2.5
#legend size
lg.c <- 2



##############################
#### daily T per unit leaf ----

ash.L.m2.dayS <- ash.L.m2.day[ash.L.m2.day$doy >= 191, ]
buckthorn.L.m2.dayS <- buckthorn.L.m2.day[buckthorn.L.m2.day$doy >= 191, ]


png(paste0(outDir,"/Tday.png"), width = 20, height = 5, units = "in", res=300)
par(mai=c(1.5,3,0.5,0.5))
plot(c(0,0), c(0,0), ylim=c(0,0.5),
     xlim=c(190,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")
points(ash.L.m2.dayS$doy[ash.L.m2.dayS$Removal == "C"],
       ash.L.m2.dayS$mean[ash.L.m2.dayS$Removal == "C"],
       pch=19, col=pt.cols[1],
       type="b", cex=pt.c, lwd=ln.w)
arrows(ash.L.m2.dayS$doy[ash.L.m2.dayS$Removal == "C"],
       ash.L.m2.dayS$mean[ash.L.m2.dayS$Removal == "C"]-ash.L.m2.dayS$se[ash.L.m2.dayS$Removal == "C"] ,
       ash.L.m2.dayS$doy[ash.L.m2.dayS$Removal == "C"],
       ash.L.m2.dayS$mean[ash.L.m2.dayS$Removal == "C"]+ash.L.m2.dayS$se[ash.L.m2.dayS$Removal == "C"],
       code=0, lwd=ln.w, 
       col=pt.cols[1])

points(ash.L.m2.dayS$doy[ash.L.m2.dayS$Removal == "R"],
       ash.L.m2.dayS$mean[ash.L.m2.dayS$Removal == "R"],
       pch=19, col=pt.cols[2],
       type="b", cex=pt.c, lwd=ln.w)
arrows(ash.L.m2.dayS$doy[ash.L.m2.dayS$Removal == "R"],
       ash.L.m2.dayS$mean[ash.L.m2.dayS$Removal == "R"]-ash.L.m2.dayS$se[ash.L.m2.dayS$Removal == "R"] ,
       ash.L.m2.dayS$doy[ash.L.m2.dayS$Removal == "R"],
       ash.L.m2.dayS$mean[ash.L.m2.dayS$Removal == "R"]+ash.L.m2.dayS$se[ash.L.m2.dayS$Removal == "R"],
       code=0, lwd=ln.w, 
       col=pt.cols[2])

points(buckthorn.L.m2.dayS$doy,
       buckthorn.L.m2.dayS$mean,
       pch=19, col=pt.cols[3],
       type="b", cex=pt.c, lwd=ln.w)

arrows(buckthorn.L.m2.dayS$doy,
       buckthorn.L.m2.dayS$mean-buckthorn.L.m2.dayS$se,
       buckthorn.L.m2.dayS$doy,
       buckthorn.L.m2.dayS$mean+buckthorn.L.m2.dayS$se,
       code=0, lwd=ln.w, 
       col=pt.cols[3])

legend("topright",
       c("Ash control",
         "Ash removal",
         "Buckthorn control"),
       col=pt.cols,
       pch=19, lwd=ln.w,
       cex=lg.c, pt.cex=pt.c, bty="n", horiz=TRUE)

axis(1, seq(190,270, by=10), rep(" ", length(seq(190,270, by=10))), cex.axis=ax.c, lwd.ticks=tlw)
axis(2, seq(0,0.5, by=0.1), rep(" ", length(seq(0,0.5, by=0.1))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(190,270, by=10), at= seq(190,270, by=10), side=1, line=2, cex=alc)
mtext( seq(0,0.5, by=0.1), at= seq(0,0.5, by=0.1), side=2, line=2, cex=alc, las=2)
mtext(expression(paste("Canopy transpiration ")), side=2, line=9, cex=llc)
mtext(expression(paste("(L m"^"-2","day"^"-1",")")), side=2, line=6, cex=llc)
mtext("Day of year", side=1, line=6, cex=llc)
dev.off()





##############################
#### leaf area histogram   ----


#is 60 cm tree valid? Double check with Aaron and students

buckthornRemovals <- buckthornRemoval[buckthornRemoval$DBH..cm. < 60,]

hist(buckthornRemovals$LAft.M2, breaks=seq(0,240,by=20))

#exclude LA from large tree because unsure


mean(buckthorn.L.m2.day$mean)*sum(buckthornRemovals$LAft.M2)


test <- inner_join(controlT, removalT, by="estD")

tempD <- test$Tm6.x - test$Tm6.y
t.test(tempD, mu=0)
#negative difference = removal is warmer than control
moistD <- test$SM.cor.x - test$SM.cor.y
t.test(moistD, mu=0)
#positive difference = removal has less moisture than control



##############################
#### soil moisture       ----



png(paste0(outDir,"/soil_moist.png"), width = 20, height = 5, units = "in", res=300)
par(mai=c(1.5,3,0.5,0.5))
plot(c(0,0), c(0,0), ylim=c(0.3,0.6),
     xlim=c(190,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")

points(TMSsub$DD[TMSsub$location == "control" & TMSsub$estD <= "2021-07-16 11:15:00"],
       TMSsub$SM.cor[TMSsub$location == "control"& TMSsub$estD <= "2021-07-16 11:15:00"],
       pch=19, col=pt.cols[1],
       type="l", lwd=lln.w)

points(TMSsub$DD[TMSsub$location == "control" & TMSsub$estD >= "2021-08-03 10:15:00"],
       TMSsub$SM.cor[TMSsub$location == "control"& TMSsub$estD >= "2021-08-03 10:15:00"],
       pch=19, col=pt.cols[1],
       type="l", lwd=lln.w)

points(TMSsub$DD[TMSsub$location == "removal"& TMSsub$estD <= "2021-07-16 11:15:00"],
       TMSsub$SM.cor[TMSsub$location == "removal"& TMSsub$estD <= "2021-07-16 11:15:00"],
       pch=19, col=pt.cols[2],
       type="l", lwd=lln.w)

points(TMSsub$DD[TMSsub$location == "removal" & TMSsub$estD >= "2021-08-03 10:15:00"],
       TMSsub$SM.cor[TMSsub$location == "removal"& TMSsub$estD >= "2021-08-03 10:15:00"],
       pch=19, col=pt.cols[2],
       type="l", lwd=lln.w)

legend("topright",
       c("Control",
         "Removal"),
       col=pt.cols[1:2],
        lwd=ln.w,
       cex=lg.c, pt.cex=pt.c, bty="n", horiz=TRUE)
axis(1, seq(190,270, by=10), rep(" ", length(seq(190,270, by=10))), cex.axis=ax.c, lwd.ticks=tlw)
axis(2, seq(0.3,0.5, by=0.1), rep(" ", length(seq(0.3,0.5, by=0.1))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(190,270, by=10), at= seq(190,270, by=10), side=1, line=2, cex=alc)
mtext( seq(0.3,0.5, by=0.1), at= seq(0.3,0.5, by=0.1), side=2, line=2, cex=alc, las=2)
mtext(expression(paste("Soil moisture ")), side=2, line=9, cex=llc)
mtext(expression(paste("(m"^"3","m"^"-3",")")), side=2, line=6, cex=llc)
mtext("Day of year", side=1, line=6, cex=llc)

dev.off()

##############################
#### soil temperature       ----

png(paste0(outDir,"/soil_temperature.png"), width = 20, height = 5, units = "in", res=300)
par(mai=c(1.5,3,0.5,0.5))
plot(c(0,0), c(0,0), ylim=c(15,25),
     xlim=c(190,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")

points(TMSsub$DD[TMSsub$location == "control" & TMSsub$estD <= "2021-07-16 11:15:00"],
       TMSsub$Tm6[TMSsub$location == "control"& TMSsub$estD <= "2021-07-16 11:15:00"],
       pch=19, col=pt.cols[1],
       type="l", lwd=lln.w)

points(TMSsub$DD[TMSsub$location == "control" & TMSsub$estD >= "2021-08-03 10:15:00"],
       TMSsub$Tm6[TMSsub$location == "control"& TMSsub$estD >= "2021-08-03 10:15:00"],
       pch=19, col=pt.cols[1],
       type="l", lwd=lln.w)

points(TMSsub$DD[TMSsub$location == "removal"& TMSsub$estD <= "2021-07-16 11:15:00"],
       TMSsub$Tm6[TMSsub$location == "removal"& TMSsub$estD <= "2021-07-16 11:15:00"],
       pch=19, col=pt.cols[2],
       type="l", lwd=lln.w)

points(TMSsub$DD[TMSsub$location == "removal" & TMSsub$estD >= "2021-08-03 10:15:00"],
       TMSsub$Tm6[TMSsub$location == "removal"& TMSsub$estD >= "2021-08-03 10:15:00"],
       pch=19, col=pt.cols[2],
       type="l", lwd=lln.w)

legend("topright",
       c("Control",
         "Removal"),
       col=pt.cols[1:2],
       lwd=ln.w,
       cex=lg.c, pt.cex=pt.c, bty="n", horiz=TRUE)
axis(1, seq(190,270, by=10), rep(" ", length(seq(190,270, by=10))), cex.axis=ax.c, lwd.ticks=tlw)
axis(2, seq(15,25, by=2), rep(" ", length(seq(15,25, by=2))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(190,270, by=10), at= seq(190,270, by=10), side=1, line=2, cex=alc)
mtext( seq(15,25, by=2), at= seq(15,25, by=2), side=2, line=2, cex=alc, las=2)
mtext(expression(paste("Soil temperature ")), side=2, line=9, cex=llc)
mtext(expression(paste("(",degree,"C)")), side=2, line=6, cex=llc)
mtext("Day of year", side=1, line=6, cex=llc)

dev.off()






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
