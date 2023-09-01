

library(dplyr)


Ddir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/GIS/drone/campus/mapping/P4M"


##############################
#### Study boundaries ----
userNumber <- 2
dirData <- c("E:/Google Drive/research/projects/campus/buckthorn/sapflux",#windows office
             "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/campus/buckthorn/sapflux")
#SLA
buckthornSLA <- read.csv(paste0(dirData[userNumber],"/leaf area.csv"))
#buchthorn dbh and leaf allom
buckthornLA <- read.csv(paste0(dirData[userNumber],"/buckthorn_leaf_allom.csv"))
#list of buckthorn removed with dbh
buckthornRemove <- read.csv(paste0(dirData[userNumber],"/buckthorn_dbh.csv"))


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



