library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(mapview)

Ddir <- "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M"
DdirGC <- "E:/Google Drive/GIS/drone/campus/mapping/P4M_GCP"

#read in study boundaries
removal <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/GPS/correct_boundaries/poly_removal.shp"),32618)
control <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/GPS/correct_boundaries/poly_controls.shp"),32618)

#define broader forest boundary for cropping
extentB <- extent(466520, 466610, 4767390, 4767480)
#read in raster data


fl0503 <- stack(paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_green.tif"),
                paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red.tif"),
                paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_nir.tif"))

fl0503c <- crop(fl0503, extentB)
plotRGB(fl0503c, r=3,g=2,b=1, scale=0.5,stretch="lin")
viewRGB(fl0503c, r=3,g=2,b=1)

fl0503G <- stack(paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_blue.tif"),
                paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_green.tif"),
                paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red.tif"),
                paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red edge.tif"),
                paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_nir.tif"))

fl0503Gc <- crop(fl0503G, extentB)
plotRGB(fl0503Gc, r=5,g=3,b=2, scale=0.2)
viewRGB(fl0503c, r=5,g=3,b=2,quantiles = c(0,1))
$#compare to gcp
fl0503G <- stack(paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_blue.tif"),
                paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_green.tif"),
                paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red.tif"),
                paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red edge.tif"),
                paste0(DdirGC, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_nir.tif"))

fl0503Gc <- crop(fl0503G, extentB)
plotRGB(fl0503Gc, r=3,g=2,b=1, scale=0.5,stretch="lin")
viewRGB(fl0503Gc, r=3,g=2,b=1)+
  viewRGB(fl0503c, r=3,g=2,b=1)


fl0519 <- stack(paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_green.tif"),
                paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_red.tif"),
                paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_nir.tif"))

fl0519c <- crop(fl0519, extentB)
#plotRGB(fl0519c, r=3,g=2,b=1, scale=0.5,stretch="lin")

fl0607 <- stack(paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_green.tif"),
                paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_red.tif"),
                paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_nir.tif"))

fl0607c <- crop(fl0607, extentB)

#plotRGB(fl0607c , r=3,g=2,b=1, scale=0.5,stretch="lin")


fl0610 <- stack(paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_green.tif"),
                paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_red.tif"),
                paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_nir.tif"))

fl0610c <- crop(fl0610, extentB)

#plotRGB(fl0610c , r=3,g=2,b=1, scale=0.5,stretch="lin")

fl0618 <- stack(paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_green.tif"),
                paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_red.tif"),
                paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_nir.tif"))

fl0618c <- crop(fl0618, extentB)

#plotRGB(fl0618c , r=3,g=2,b=1, scale=0.5,stretch="lin")

fl0625 <- stack(paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_green.tif"),
                paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_red.tif"),
                paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_nir.tif"))

fl0625c <- crop(fl0625, extentB)

#plotRGB(fl0625c , r=3,g=2,b=1, scale=0.5,stretch="lin")



fl0701 <- stack(paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_green.tif"),
                paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_red.tif"),
                paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_nir.tif"))

fl0701c <- crop(fl0701, extentB)

#plotRGB(fl0701c , r=3,g=2,b=1, scale=0.5,stretch="lin")

fl0712 <- stack(paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_green.tif"),
                paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_red.tif"),
                paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_nir.tif"))

fl0712c <- crop(fl0712, extentB)

#plotRGB(fl0712c , r=3,g=2,b=1, scale=0.5,stretch="lin")


fl0719 <- stack(paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_green.tif"),
                paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_red.tif"),
                paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_nir.tif"))

fl0719c <- crop(fl0719, extentB)

#plotRGB(fl0719c , r=3,g=2,b=1, scale=0.5,stretch="lin")


fl0726 <- stack(paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_green.tif"),
                paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_red.tif"),
                paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_nir.tif"))

fl0726c <- crop(fl0726, extentB)

#plotRGB(fl0726c , r=3,g=2,b=1, scale=0.5,stretch="lin")



#calculate NDVI 

AllList <- list(fl0503c, fl0519c,fl0607c,fl0610c,fl0618c,fl0625c,fl0701c ,
                fl0712c, fl0719c,fl0726c)
DOY <- c(123,139,158,161,169,175,182,193,200,207)

#calculate
NDVIList <- list()
for(i in 1:length(DOY)){
  NDVIList[[i]] <- (AllList[[i]][[5]]-AllList[[i]][[3]])/(AllList[[i]][[5]]+AllList[[i]][[3]])
}

#resample so all maps match
#also change resolution so consistent
#make slightly courser at 10 cm since slightly over 5 cm is highest resolution
#set up common raster extent  
blankR <- raster(extent(NDVIList[[1]]), crs = crs(NDVIList[[1]]), resolution = .1)
NDVIR <- list()
for(i in 1:length(DOY)){
  NDVIR[[i]] <- resample(NDVIList[[i]], blankR)

}


NDVIs <- stack(NDVIR)
names(NDVIs) <- DOY
plot(NDVIs)
#before removal

plot(NDVIs[[4]], main = names(NDVIs[[4]]))
#look at changes from pre- removal to 7/1

change0701.0610 <- NDVIs[[7]] - NDVIs[[4]] 
plot(change0701.0610)
plot(removal$geometry, add=TRUE)


change0712.0610 <- NDVIs[[8]] - NDVIs[[4]] 
plot(change0712.0610)
plot(removal$geometry, add=TRUE)


change0726.0610 <- NDVIs[[10]] - NDVIs[[4]] 
plot(change0726.0610)
plot(removal$geometry, add=TRUE)


#examine difference between control and removal
#remove z
removeXY <- st_zm(removal)
#convert to sp
removeSP <- as_Spatial(removeXY )

#remove z
controlXY <- st_zm(control)
#convert to sp
controlSP <- as_Spatial(controlXY )
test <- extract(NDVIs[[1]],controlSP )
test2 <- c(test[[1]],test[[2]],test[[3]])


controlN <-numeric()
controltemp <- list()
removalN <- numeric()
ndviDF <- list()

for(i in 1:nlayers(NDVIs)){
  removalN <-  extract(NDVIs[[i]],removeSP )[[1]]
  
  
  controltemp <- extract(NDVIs[[i]],controlSP )
  controlN <- c(controltemp[[1]],controltemp[[2]],controltemp[[3]])
  
  ndviDF[[i]] <- data.frame(doy = c(rep(DOY[i], length( removalN)),
                                    rep(DOY[i], length( controlN))),
                            exp = c(rep("remove", length(removalN)),
                                   rep("control", length(controlN))),
                            
                            ndvi =  c(removalN,controlN))
  
}


ndviAll <- rbind(ndviDF[[1]],ndviDF[[2]],ndviDF[[3]],
                 ndviDF[[4]],ndviDF[[5]],ndviDF[[6]],
                 ndviDF[[7]],ndviDF[[8]],ndviDF[[9]],
                 ndviDF[[10]] )


NDVIdoy <- ndviAll %>%
  group_by(doy, exp) %>%
  summarise(mean = mean(ndvi,na.rm=TRUE),median= quantile(ndvi, prob=0.5,na.rm=TRUE),sd=sd(ndvi), n=length(ndvi))


ggplot(ndviAll, aes(as.factor(doy),ndvi,fill=exp))+
  geom_violin()+
  geom_boxplot(width=0.1, color="grey", alpha=0.2)


ggplot(NDVIdoy, aes(doy,mean,col=exp))+
  geom_point()

