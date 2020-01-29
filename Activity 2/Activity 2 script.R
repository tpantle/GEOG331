###################
# Activity 2 Script
# Tyler Pantle
###################

### Vectors ###

# make a vector of tree heights in meters
heights <- c(30,41,20,22)
# convert to cm
heights_cm <- heights*100
heights_cm
# look at first tree height
# square brackets subset an object
heights[1]
# look at second and third
heights[2:3]

### Matrices ###

# Matrices are multiple columns of vectors

# use 'help(matrix)' to get more info on the matrix function
help(matrix)
# set up a matrix with 2 columns and fill in by rows
# first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
# Fill by columns
Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
# NOTE: square bracket labels are always formatted [ROW,COLUMN]
# use this notation to refer to specific spots in a matrix
Mat.bycol[1,2]
# by leaving a spot blank in the notation, it'll reder to all items in that slot
# all values in row 1
Mat.bycol[1,]
# all values in column 2
Mat.bycol[,2]

### Dataframes ###

# read in weather station data file
datW <- read.csv("Z:\\Students\\tpantle\\Data\\activities\\a02\\2011124.csv")

# change date format from a factor so that it's more useful
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
View(datW)

# find all unique site names
levels(datW$NAME)

# look at the mean max temp for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
# this returns an 'NA' value because there is missing data in this data set
# there is an argument in mean ('na.rm=TRUE') that ignores NAs in the calculation
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

# calculate avg daily temp
# this will be halfway between the min and max temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

# above method is not efficient so we can use the aggregate function instead
# get means across all sites
# the 'by' function is a list of one or more variables to index over
# 'FUN' indicates the function we want to use
# then add any specific arguments using a comma and adding them after the function
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
averageTemp

# now change the column names to be more meaningful
# note: "MAAT" is a common abbreviation for Mean Annual Air Temp"
colnames(averageTemp) <- c("Name","MAAT")
averageTemp

# convert names to numbers
datW$siteN <- as.numeric(datW$NAME)

# make a histogram for the first site in our levels
# 'main=' is the title name argument so paste the actual name of the factor, not the numeric index
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average Daily Temperature (Degrees C)",
     ylab = "Relative Frequency",
     col = "grey50",
     border = "white")
