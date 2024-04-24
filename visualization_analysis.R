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

userNumber <- 1
dirData <- c("G:/My Drive/research/projects/campus/buckthorn/sapflux")#windows office
#SLA
buckthornSLA <- read.csv(paste0(dirData[userNumber],"/leaf area.csv"))
#list of buckthorn removed with dbh
buckthornRemove <- read.csv(paste0(dirData[userNumber],"/buckthorn_dbh.csv"))
##############################
#### output directory ### ----

outDir <- "G:/My Drive/research/projects/buckthorn/figures"

##############################
#### Analysis ### ----

# filter T for days when errors are reduced
ash.L.day$species <- rep("Ash", nrow(ash.L.day))
buckthorn.L.day$species <- rep("Buckthorn", nrow(buckthorn.L.day))

Tday_all <- rbind(ash.L.day, buckthorn.L.day)
Tday_all$expID <- paste(Tday_all$species, Tday_all$Removal)
Tday_w <- left_join(Tday_all, meteoDaily, by="doy")

# filter out low VPD and high precip days
Tday <- Tday_w %>%
  filter(TotPrecip_cm < 0.1 & maxVPD > 0.6 ) 

ggplot(Tday, aes(doy, L.m2.day, color=expID))+
         geom_point()+
  geom_line()

ggplot(Tday, aes(aveVPD, L.m2.day, color=expID))+
  geom_point()

ggplot(Tday, aes(maxVPD, L.m2.day, color=expID))+
  geom_point()

# removal completed by 181 doy

##############################
#### Standard plot argument ----

#plot colors

pt.cols <- c(rgb(0,114,178,155,maxColorValue=255), #control ash
             rgb(213,94,0,155,maxColorValue=255), #removal ash
             rgb(0,158,115,155,maxColorValue=255)) #buckthon control

pt.cols2 <- c(rgb(0,114,178,50,maxColorValue=255), #control ash
             rgb(213,94,0,50,maxColorValue=255), #removal ash
             rgb(0,158,115,50,maxColorValue=255)) #buckthon control

pt.cols3 <- c(rgb(0,114,178,maxColorValue=255), #control ash
              rgb(213,94,0,maxColorValue=255), #removal ash
              rgb(0,158,115,maxColorValue=255)) #buckthon control

pt.cols4 <- c(rgb(0,114,178,190,maxColorValue=255), #control ash
              rgb(213,94,0,190,maxColorValue=255), #removal ash
              rgb(0,158,115,190,maxColorValue=255)) #buckthon control

wd <- 16*2.54
hd <- 4*2.54

#point cex
pt.c <- 3
#line thickness
ln.w <- 2

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
#axis size
ax.c <- 2
#text size
tcx <- 2.5
#panel letter label size
plc <- 4


##############################
#### weather ----
hd <- 5
wd <- 10


png(paste0(outDir,"/metero.png"), width = 15, height = 20, units = "in", res=300)
layout(matrix(c(1,2,3), ncol=1), width=lcm(rep(wd*2.54,1)),
       height=lcm(rep(c(hd)*2.54,3)))

par(mai=c(0.3,0.3,0.3,0.3))

plot(c(0,0), c(0,0), ylim=c(10,26),
     xlim=c(190,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")
points(meteoDaily$doy,meteoDaily$airT,
       pch=19, col="black",
       type="b", cex=pt.c, lwd=ln.w)

axis(1, seq(190,270, by=10), rep(" ", length(seq(190,270, by=10))), cex.axis=ax.c, lwd.ticks=tlw)
axis(2, seq(10,25, by=5), rep(" ", length(seq(10,25, by=5))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(10,25, by=5), at= seq(10,25, by=5), side=2, line=3, cex=alc, las=2)
mtext("Air temperature", side=2, line=13, cex=llc)
mtext(expression(paste("(", degree,"C )")), side=2, line=9, cex=llc)
text(192, 25, "a", cex=plc)

par(mai=c(0.3,0.3,0.3,0.3))

plot(c(0,0), c(0,0), ylim=c(0,5),
     xlim=c(190,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")
for(i in 1:nrow(meteoDaily)){
  polygon(c(meteoDaily$doy[i]-0.25,meteoDaily$doy[i]-0.25, 
            meteoDaily$doy[i]+0.25,meteoDaily$doy[i]+0.25),
          c(0, meteoDaily$TotPrecip_cm[i], meteoDaily$TotPrecip_cm[i],0),
          border=NA, col="#91B6C6") 
}

axis(1, seq(190,270, by=10), rep(" ", length(seq(190,270, by=10))), cex.axis=ax.c, lwd.ticks=tlw)
  
axis(2, seq(0,5, by=1), rep(" ", length(seq(0,5, by=1))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(0,5, by=1), at= seq(0,5, by=1), side=2, line=3, cex=alc, las=2)
mtext("Precipitation", side=2, line=13, cex=llc)
mtext("(cm)", side=2, line=9, cex=llc)
text(192, 4.85, "b", cex=plc)


par(mai=c(0.3,0.3,0.3,0.3))

plot(c(0,0), c(0,0), ylim=c(0,2.2),
     xlim=c(190,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")

points(meteoDaily$doy,meteoDaily$aveVPD,
       pch=19, col="black",
       type="b", cex=pt.c, lwd=ln.w)

points(meteoDaily$doy,meteoDaily$maxVPD,
       pch=19, col="#737B7F",
       type="b", cex=pt.c, lwd=ln.w)


axis(1, seq(190,270, by=10), rep(" ", length(seq(190,270, by=10))), cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(190,270, by=10), at= seq(190,270, by=10), side=1, line=3, cex=alc)
mtext("Day of year", side=1, line=6, cex=llc)


axis(2, seq(0,2, by=0.5), rep(" ", length(seq(0,2, by=0.5))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( c("0","0.5","1.0","1.5","2.0"), at= seq(0,2, by=0.5), side=2, line=3, cex=alc, las=2)
mtext("Vapor Pressure Deficit ", side=2, line=13, cex=llc)
mtext("(KPa)", side=2, line=9, cex=llc)
text(192, 1.9, "c", cex=plc)
legend("topright", c("average", "maximum"),
       pch=19, lwd=ln.w, col=c("black", "#737B7F"), bty="n", cex=3)

dev.off()

##############################
#### daily T per unit leaf ----

ash.L.m2.dayS <- ash.L.m2.day[ash.L.m2.day$doy >= 191, ]
buckthorn.L.m2.dayS <- buckthorn.L.m2.day[buckthorn.L.m2.day$doy >= 191, ]


png(paste0(outDir,"/Tday.png"), width = 20, height = 7, units = "in", res=300)
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
mtext("Day of year", side=1, line=4, cex=llc)
dev.off()





##############################
#### leaf area histogram   ----
#### leaf area allometry ----

buckthorn.SLA <- mean(buckthornSLA$area.cm2/buckthornSLA$weight.g)
#LA (m2) = -66.185 +  6.579*DBH in cm
ash.tree$LA.m2 <- -66.185 +  6.579*ash.tree$DBH.cm
plot(seq(1,60), -66.185 +  (6.579*seq(1,60))) 

#estimate leaf area in m2
buckthorn.tree$LA.m2 <- exp(-1.058 + (1.828*log(buckthorn.tree$DBH.cm)))
plot(seq(1,65), exp(-1.058 + (1.828*log(seq(1,65)))) )
buckthornRemoval <- buckthornRemove[grepl("Removal",buckthornRemove$Plot..Control.Removal.Neither.) == TRUE,]

# Mascaro and Schnitzer have a calculation from data that includes larger trees
# smaller trees have similar values to our allometry
buckthornRemoval$LAft.M2 <- (((((0.0287 *buckthornRemoval$DBH..cm.^1.6046)))*1000)*buckthorn.SLA)*0.0001


#is 60 cm tree valid? Double check with Aaron and students

buckthornRemovals <- buckthornRemoval[buckthornRemoval$DBH..cm. < 60,]

histL <- hist(buckthornRemovals$LAft.M2, breaks=seq(0,240,by=3))
sum(buckthornRemovals$LAft.M2)

#exclude LA from large tree because unsure

mean(buckthorn.L.m2.day$mean)
mean(buckthorn.L.m2.day$mean)*sum(buckthornRemovals$LAft.M2)

range(buckthornRemovals$LAft.M2)


png(paste0(outDir,"/leaf_area.png"), width = 8, height = 7, units = "in", res=300)
par(mai=c(1.5,1.5,0.5,0.5))

plot(c(0,0), c(0,0), ylim=c(0,10),
     xlim=c(0,241),
     axes=FALSE, xlab=" ",type="n",
     ylab= " ", xaxs = "i", yaxs="i")
for(i in 1:length(histL$mids)){
  polygon(c(histL$mids[i]-1.5,histL$mids[i]-1.5,histL$mids[i]+1.5,histL$mids[i]+1.5),
          c(0,histL$counts[i],histL$counts[i],0),
          col=pt.cols[3],
          border="white")
  
}

axis(1, seq(0,240, by=40), rep(" ", length(seq(0,240, by=40))), cex.axis=ax.c, lwd.ticks=tlw)
axis(2, seq(0,10, by=2), rep(" ", length(seq(0,10, by=2))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(0,240, by=40), at= seq(0,240, by=40), side=1, line=2, cex=alc)
mtext( seq(0,10, by=2), at= seq(0,10, by=2), side=2, line=2, cex=alc, las=2)
mtext(expression(paste("Buckthorn canopy leaf area removed ")), side=1, line=4.5, cex=llc)
mtext(expression(paste("(m"^"2",")")), side=1, line=7, cex=llc)
mtext("Frequency", side=2, line=4.5, cex=llc)
dev.off()



##############################
#### soil moisture       ----



png(paste0(outDir,"/soil_moist.png"), width = 20, height = 7, units = "in", res=300)
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

png(paste0(outDir,"/soil_temperature.png"), width = 20, height = 7, units = "in", res=300)
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
mtext("Day of year", side=1, line=4, cex=llc)

dev.off()




##############################
#### soil stats ----- 
test <- inner_join(controlT, removalT, by="estD")

tempD <- test$Tm6.x - test$Tm6.y
t.test(tempD, mu=0)
#negative difference = removal is warmer than control
moistD <- test$SM.cor.x - test$SM.cor.y
t.test(moistD, mu=0)
#positive difference = removal has less moisture than control



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
