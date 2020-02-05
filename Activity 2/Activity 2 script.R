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

# create a date column by reformatting the date to only include years
# and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

##### Question 2: create example vectors
a <- c(150,215,360,485,599) #numeric vector
b <- c("plotA", "plotB", "plotC", "plotD", "plotE") #character vector
c <- as.integer(a) #integer vector
d <- factor(c("plotA", "plotB", "plotC", "plotD", "plotE"))

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

# histogram for Aberdeen (first site in our levels)
# main = title name argument
# paste the actual name of the factor not the numeric index
par(mfrow=c(2,2))
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# add mean line with red (tomato3) color
# and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
# add standard deviation line below the mean with red (tomato3) color
# and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

##### Question 4
# Histogram for Livermore

hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="blue",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# Histogram for Mandan Experiment Station
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="dark green",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# Histogram for Mormon Flat
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="black",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# name the Aberdeen histogram for later reference
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

# SEQ function generates a sequence of numbers that we can use to plot the normal dist across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)

# DNORM function will produce the probability density based on a mean and standard deviation

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# create a density that is scaled to fit in the plot (since ranges are different)
##### This is helpful for putting multiple things on the same plot!!
##### It means the max value of the plot is always the same between the two datasets on the plot
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

# POINTS function adds points or lines to a graph  
# first two arguments are the x coordinates and the y coordinates

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# Using probability to think about the occurance of different air temps

help(dnorm)
# pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
# pnorm with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
# subtract pnorm with 0 from pnorm with 5 to get area between 0-5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# pnorm of 20 gives all probability (area of the curve) below 20 
# subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# qnorm function returns the value associated with a probability
# this is the value in which all values at or below the value equal that probability
# examine what unusually high temps in Aberdeen start at
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

##### Question 6
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
pnorm(22.51026,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(18.51026,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

##### Question 7
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation",
     ylab = "Relative Frequency",
     col = "grey50",
     border = "white")

##### Question 8
sumSite <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum", na.rm=TRUE)
sumSite

# Histogram of annual precipitation in Livermore
hist(sumSite$x[sumSite$Group.1 == "LIVERMORE, CA US"],
     freq=FALSE,
     main = "LIVERMORE, CA US",
     xlab = "Annual Precipitation",
     ylab = "Relative Frequency",
     col = "grey50",
     border = "white")

##### Question 9
meanPRCP <- aggregate(sumSite$x, by=list(sumSite$Group.1), FUN="mean", na.rm=TRUE)


                 