#-----------------------------------------------------------------------------------------------------------------
# This script matches IPHC stat areas with the stat6 management areas. The end result is a dataframe that includes 
# the iphc area in which the centroid of a stat6 area fell.

# Author: Jordan Watson
# Date: 4/2/2016
#-----------------------------------------------------------------------

library(sp)
library(rgdal)
library(rgeos)
library(dplyr)

#-----------------------------------------------------------------------
# Read in IPHC shapefile
iphc <- readOGR(dsn="Shapefiles", layer = "GOA_Den")
# Plot it
plot(iphc);axis(1);axis(2)
# Figure out projection
iphc@proj4string


#  Now repeat for the groundfish areas shapefile
stat6 <- readOGR(dsn="Shapefiles", layer = "pvg_stat_2001")
stat6@proj4string

# Transform the stat6 file into lat-long with the same CRS as the iphc file
stat6 <- spTransform(stat6, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(stat6);axis(1);axis(2)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
##### Code Chunk for fixing plots - not mandatory but helpful for checking accuracy

# I want to overlay the two plots on each other to make sure they are matching, but this stupid dateline thing is annoying.
# Convert longitudes > 0 to be negative
# iphc is a SpatialPolygonsDataFrame, which is an S4 object containing lists of SpatialPolygons objects which are lists.
# First identify the number of unique polygons. This number will be the top level of our nested objects
level1 <- length(unique(iphc@polygons))
for(i in 1:level1){
  # Determine how many sets of polygon coordinates lie within each level of the main polygons
  # Loop through each of these 
  level2 <- length(unique(iphc@polygons[[i]]@Polygons))
  for(j in 1:level2){
    # We can now access each set of longitudes, from which we subtract 360
    iphc@polygons[[i]]@Polygons[[j]]@coords[,1][iphc@polygons[[i]]@Polygons[[j]]@coords[,1]>0] <- iphc@polygons[[i]]@Polygons[[j]]@coords[,1] - 360
  }
}
# We are close. Unfortunately, the shapefile bounds are still set to the original coordinates
plot(iphc);axis(1);axis(2)
# Adjust the bounding box extent to something for reasonable
iphc@bbox[3] <- -120
# Much better.
plot(iphc);axis(1);axis(2)

#writeOGR(iphc, dsn="Shapefiles", layer="iphc_new", driver="ESRI Shapefile")


level1 <- length(unique(stat6@polygons))
for(i in 1:level1){
  level2 <- length(unique(stat6@polygons[[i]]@Polygons))
  for(j in 1:level2){
    stat6@polygons[[i]]@Polygons[[j]]@coords[,1][stat6@polygons[[i]]@Polygons[[j]]@coords[,1]>0] <- stat6@polygons[[i]]@Polygons[[j]]@coords[,1] - 360
  }
}
rm(i,j,level1,level2)
plot(stat6);axis(1);axis(2)

stat6@bbox[3] <- -120
#writeOGR(stat6, dsn="Shapefiles", layer="stat6_new", driver="ESRI Shapefile")

plot(stat6,col="grey")
plot(iphc,add=TRUE,col="#FF303050")
# That looks good.

## End plotting code chunk
#-----------------------------------------------------------------------

# Identify overlaps by finding the centroid of each stat area.
# Find the centroid of each stat6 area
stat6.center <- gCentroid(stat6[,1],byid=TRUE)
#  Create a column that includes the stat area names
stat6.center$statarea <- levels(stat6@data$STAT_AREA)
#  Make sure this worked
plot(stat6.center,col="red",pch=16)
plot(stat6,add=TRUE)

#  Create a data frame where a stat6 area is matched with the iphc area in which its centroid fell.
temp <- over(stat6.center,iphc)$STAT_AREA
my.match <- data.frame(stat6=levels(stat6.center$statarea),iphc=temp) %>% filter(!is.na(iphc))
rm(temp)
write.csv(my.match,"myMatch.csv")

#  Depending on how you're doing the matching, you may prefer to find the stat area in which the centroid
#  of an iphc area fell.
iphc.center <- gCentroid(iphc,byid=TRUE)
#  Create a column that includes the stat area names
iphc.center$statarea <- as.character(levels(factor(iphc@data$STAT_AREA)))

#  Create a data frame where a stat6 area is matched with the iphc area in which its centroid fell.
temp <- over(iphc.center,stat6)$STAT_AREA
my.match2 <- data.frame(iphc=levels(iphc.center$statarea),stat6=temp) %>% filter(!is.na(stat6))
write.csv(my.match2,"myMatch2.csv")
