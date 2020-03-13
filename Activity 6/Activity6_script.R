###################
# Activity 6 Script
# Tyler Pantle
# 3/27/20
###################

# install packages to be used, then set to comment
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

# load all packages to environment
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

# read in shapefiles using readOGR in rgdal
# start withh 1966 data
g1966 <- readOGR("Z:\\Data\\activities\\a06\\GNPglaciers\\GNPglaciers_1966.shp")

# when you make a plot of spatial data, your packages automatically get called
plot(g1966, col="black", axes = TRUE)

# display data structure with 'str'
str(g1966)

# display projection information
g1966@proj4string
### if you read in data from different projections, then the coordinate values will be different and some data won't show up

