library(raster)
library(sf)
library(dplyr)


Ddir <- "K:/Environmental_Studies/hkropp/GIS/drone/campus/mapping/P4M"


##############################
#### Study boundaries ----
userNumber <- 1
dirData <- c("E:/Google Drive/research/projects/campus/buckthorn/sapflux",#windows office
             "/Users/hkropp/Google Drive/research/projects/campus/buckthorn/sapflux")
#SLA
buckthornSLA <- read.csv(paste0(dirData[userNumber],"/leaf area.csv"))
#buchthorn dbh and leaf allom
buckthornLA <- read.csv(paste0(dirData[userNumber],"/buckthorn_leaf_allom.csv"))
#list of buckthorn removed with dbh
buckthornRemove <- read.csv(paste0(dirData[userNumber],"/buckthorn_dbh.csv"))

##############################
#### Study boundaries ----

# read in study boundaries
# and reproject
removal <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/GPS/correct_boundaries/poly_removal.shp"),32618)
control <- st_transform(st_read("K:/Environmental_Studies/hkropp/GIS/GPS/correct_boundaries/poly_controls.shp"),32618)

##############################
#### Read in drone data ----

#define broader forest boundary for cropping
extentB <- extent(466520, 466610, 4767390, 4767480)
#read in raster data


fl0503 <- stack(paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_green.tif"),
                paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red.tif"),
                paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/05_03_21_buckthorn/05_03_21_buckthorn_transparent_reflectance_nir.tif"))

fl0503c <- crop(fl0503, extentB)


fl0519 <- stack(paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_green.tif"),
                paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_red.tif"),
                paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/05_19_21_buckthorn/05_19_21_transparent_reflectance_nir.tif"))

fl0519c <- crop(fl0519, extentB)


fl0607 <- stack(paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_green.tif"),
                paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_red.tif"),
                paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/06_07_21_buckthorn/June_7_transparent_reflectance_nir.tif"))

fl0607c <- crop(fl0607, extentB)


fl0610 <- stack(paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_green.tif"),
                paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_red.tif"),
                paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/06_10_21_buckthorn/06_10_transparent_reflectance_nir.tif"))

fl0610c <- crop(fl0610, extentB)


fl0618 <- stack(paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_green.tif"),
                paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_red.tif"),
                paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/06_18_21_buckthorn_p1/06_18_21_buckthorn_p1_rerun_transparent_reflectance_nir.tif"))

fl0618c <- crop(fl0618, extentB)


fl0625 <- stack(paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_green.tif"),
                paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_red.tif"),
                paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/06_25_21_buckthorn_p1/6_25_21_buckthorn_part1_transparent_reflectance_nir.tif"))

fl0625c <- crop(fl0625, extentB)

fl0701 <- stack(paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_green.tif"),
                paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_red.tif"),
                paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/07_01_21_buckthorn_p1/flight_07_01_21_buckthorn_p1_transparent_reflectance_nir.tif"))

fl0701c <- crop(fl0701, extentB)


fl0712 <- stack(paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_green.tif"),
                paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_red.tif"),
                paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/07_12_21_buckthorn_p1/07_12_21_buckthorn_p1_transparent_reflectance_nir.tif"))

fl0712c <- crop(fl0712, extentB)


fl0719 <- stack(paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_green.tif"),
                paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_red.tif"),
                paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/07_19_21_buckthorn_p2/07_19_21_buckthorn_p2_transparent_reflectance_nir.tif"))

fl0719c <- crop(fl0719, extentB)

fl0726 <- stack(paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_green.tif"),
                paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_red.tif"),
                paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/07_26_21_buckthorn_p1/07_26_21_buckthorn_p1_transparent_reflectance_nir.tif"))

fl0726c <- crop(fl0726, extentB)


fl0904 <- stack(paste0(Ddir, "/09_04_buckthorn_p1/09_04_21_buckthorn_transparent_reflectance_blue.tif"),
                paste0(Ddir, "/09_04_buckthorn_p1/09_04_21_buckthorn_transparent_reflectance_green.tif"),
                paste0(Ddir, "/09_04_buckthorn_p1/09_04_21_buckthorn_transparent_reflectance_red.tif"),
                paste0(Ddir, "/09_04_buckthorn_p1/09_04_21_buckthorn_transparent_reflectance_red edge.tif"),
                paste0(Ddir, "/09_04_buckthorn_p1/09_04_21_buckthorn_transparent_reflectance_nir.tif"))

fl0904c <- crop(fl0904, extentB)

##############################
#### NDVI calculation ----

#calculate NDVI 

AllList <- list(fl0503c, fl0519c,fl0607c,fl0610c,fl0618c,fl0625c,fl0701c ,
                fl0712c, fl0719c,fl0726c, fl0904c )
DOY <- c(123,139,158,161,169,175,182,193,200,207,247)

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


##############################
#### Extract NDVI ----


#examine difference between control and removal
#remove z
removeXY <- st_zm(removal)
#convert to sp
removeSP <- as_Spatial(removeXY )

#remove z
controlXY <- st_zm(control)
#convert to sp
controlSP <- as_Spatial(controlXY )



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
                 ndviDF[[10]],ndviDF[[11]] )


NDVIdoy <- ndviAll %>%
  group_by(doy, exp) %>%
  summarise(mean = mean(ndvi,na.rm=TRUE),median= quantile(ndvi, prob=0.5,na.rm=TRUE),sd=sd(ndvi), n=length(ndvi))

##############################
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



