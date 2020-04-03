###################
# Activity 6 Script
# Tyler Pantle
# 3/27/20
###################

# install packages to be used, then set to comment
install.packages(c("raster","sp","rgdal","rgeos","plyr"))

# load all packages to environment
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

setwd("/Users/TylerPantle/Documents/DataSci")

# read in shapefiles using readOGR in rgdal
# start withh 1966 data
g1966 <- readOGR("/Users/TylerPantle/Documents/DataSci/a06/GNPglaciers/GNPglaciers_1966.shp")

# when you make a plot of spatial data, your packages automatically get called
plot(g1966, col="black", axes = TRUE)

# display data structure with 'str'
str(g1966)

# display projection information
g1966@proj4string
### if you read in data from different projections, then the coordinate values will be different and some data won't show up

# read in the rest of the shapefiles
g1998 <- readOGR("/Users/TylerPantle/Documents/DataSci/a06/GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("/Users/TylerPantle/Documents/DataSci/a06/GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("/Users/TylerPantle/Documents/DataSci/a06/GNPglaciers/GNPglaciers_2015.shp")

# display data structure
str(g2015)
# data stores all accompanying info/measurements for each spatial object
head(g2015@data)
# polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]
# for vector objects, find projection info in an object called proj4string
g1966@proj4string

# make a map of the 1966 glaciers and view them by name
spplot(g1966, "GLACNAME")

# check that the 2015 glacier names don't match the rest of the shapefiles
# compare 2015 to 1966 for reference
g1966@data$GLACNAME
g2015@data$GLACNAME

# fix glacier names so that they are consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

# read in rgb imagery from landsat (Sept 5, 2014)
redL <- raster("/Users/TylerPantle/Documents/DataSci/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/TylerPantle/Documents/DataSci/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/TylerPantle/Documents/DataSci/a06/glacier_09_05_14/l08_blue.tif")

# check coordinate system of the raster .tif files to make sure it's the same as what we're using
redL@crs
# they're the same so we can plot them together without having to reproject

# make a brick that stacks all layers
# a BRICK is a series of rasters with the same extent and resolution
rgbL <- brick(redL, greenL, blueL)

# plot with color
# show axes for reference
# add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
# add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

# use EXT to zoom in 
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

# set up years to read in
ndviYear <- seq(2003,2016)

# read all NDVIfiles into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("/Users/TylerPantle/Documents/DataSci/a06/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

# use STR function to look at a single raster function from the first year (2003)
str(NDVIraster[[1]])

### QUESTION 2 ###
# look at projection
NDVIraster[[1]]@crs
# this reveals a Lambert Azimuthal Equal Area projection

# plot NDVI
plot(NDVIraster[[1]])

### QUESTION 3 ###
# plot NDVI with glacier polygons
plot(NDVIraster[[1]])
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)
# plot 2003 NDVI next to 1966 glacier extent map
par(mfrow=c(1,2))
plot(NDVIraster[[1]])
plotRGB(rgbL, stretch="lin", axes=TRUE)
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

# reproject the glaciers
# use the NDVI projection
# spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

### QUESTION 4 ###
# make a map with max NDVI and glaciers in 2015
plot(NDVIraster[[13]])
plot(g2015p, col=NA, add=TRUE, border=TRUE)

# calculate area for all polygons
# add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

# use JOIN from plyr to join all data together into a table not associated with the shapefile
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

# make a plot for the area of each glacier using the following table:
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")
for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
} 

### QUESTION 5 ###
# calculate percentage change in area between 1966 and 2015
1.00 - (sum(g2015p@data$Area2015))/(sum(g1966p@data$Area1966))
# make an spplot of the glaciers in 2015, showing each glacier's % change
change <- numeric(0)
for(i in 1:nrow(g2015p@data)){
  change[i] <- (1.00 - ((g2015p@data$Area2015[i])/(g1966p@data$Area1966[i])))
}

g2015p@data$pct_change <- change
spplot(g2015p,"pct_change")
# I tried for hours to figure out spplot but I couldn’t make it work
# I made a standard plot instead that shows 
plot(c(1966,2015), 
     c(0,1.00 - ((gAll$a2015m.sq[1])/(gAll$a1966m.sq[1]))),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,1),
     ylab="Percent of Glacier Lost",
     xlab="Year")
for(i in 2:39){
  points(c(1966,2015), 
         c(0,1.00 - ((gAll$a2015m.sq[i])/(gAll$a1966m.sq[i]))),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
} 


## gDifference doesn't work on Mac
# diffPoly <- gDifference(g1966p, g2015p)
# plot(diffPoly)

# plot diffPoly with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
# plot(diffPoly,col="black", border=NA,add=TRUE)
# **no diffpoly bc gDiff didn't work

### QUESTION 6 ###
# find the glacier with the largest % loss
for(i in 1:nrow(g2015p@data)){
  print((1.00 - ((g2015p@data$Area2015[i])/(g1966p@data$Area1966[i]))))
}
# make a map displaying glacial extent for all years for the glacier with highest % loss
plotRGB(rgbL, ext=c(253800,293800,5397000,5457000), stretch="lin", axes=TRUE)
plot(g1966@data$Area1966[g1966@data$GLACNAME=="Boulder Glacier"], col="tan3", border=NA, add=TRUE)
plot(g1998@data$Area1998[g1998@data$GLACNAME=="Boulder Glacier"], col="royalblue3", add=TRUE, border=NA)
plot(g2005@data$Area2005[g2005@data$GLACNAME=="Boulder Glacier"], col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015@data$Area2015[g2015@data$GLACNAME=="Boulder Glacier"], col="tomato3", add=TRUE, border=NA)

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
# for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  # NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  # meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
#}
# **no diffPoly**

#plot(ndviYear, meanDiff, type="b",
#     xlab= "Year",
#     ylab="Average NDVI (unitless)",
#     pch=19)
# **no diffPoly

# designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
# set up lm function to apply to every cell where x is the value of a cell
# need to first skip NA values (like lakes)
# if NA is missing in first raster, it is missing in all so tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
# apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
# plot the change in NDVI
plot(NDVIfit, axes=FALSE)

# buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units
# convert vector to raster
# then use zonal statistics
# convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

### QUESTION 8 ###
# rasterize glaciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
# subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

# get stats for rate of veg change in area around rasterized buffer
meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

### QUESTION 9 ###
# add the mean change in NDVI per year into the 2015 glacier polygons
meanNDVIchange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones[[2:6]],#raster with zones
                    "mean")#function to apply
head(meanChange)

par(mfrow=c(1,2))
plot(glacZones)
plot(g2015p)


### QUESTION 11 ###
g2015p@data$NDVIcol <- ifelse(g2015p@data$NDVImean<0.4,"blue","red")
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)

max_list <- list()
for(i in 1:nrow(NDVIstack@layers)){
  max_list[[i]] <- mean(NDVIstack@layers[[i]]@data@max)
}



