###################
# Activity 3 Script
# Tyler Pantle
# 2/21/20
###################

## NOTES FROM CLASS ON PACKAGES ##
# Installing Packages: Step 1
# install lubridate package, then change it to a comment
## install.packages(c("lubridate"))
# step 1 only has to be done...
# ...any time you're on a new computer, updating r, you have to re-install your packages

# Installing Packages: Step 2
# step 2 has to be done every time!
library(lubridate)
# using the 'library' code attaches the lubridate function

## START OF ACTIVITY 3 CODE ##

# Create a function 
# The names of the arguments for your function will be in parentheses
# Everything in curly brackets will be run each time the function is run
assert <- function(statement,err.message){
  # 'if' evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message) # if the statement is false, it prints the error message...if it's true, it prints nothing
  }
  
}

# check how the statement works
# evaluate a false statement
assert(1 == 2, "error: unequal values")

# evaluate a true statement
assert(2 == 2, "error: unequal values")

# set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")


## QUESTION 3 ##

# read in the data file
# skip the first 3 rows since there is additional column info
# specify the the NA is designated differently
datW <- read.csv("Y:\\Students\\tpantle\\Data\\activities\\a03\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
# preview data
print(datW[1,])

# get sensor info from file
# this data table will contain all relevent units
sensorInfo <-   read.csv("Y:\\Students\\tpantle\\Data\\activities\\a03\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
print(sensorInfo)

# get column names from sensorInfo table
# set weather station colnames  to be the same
colnames(datW) <- colnames(sensorInfo)
# preview data
print(datW[1,])

## Working with dates
# convert to standardized date format of m/d/y with the timezone set to New York
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

# calculate day of year
datW$doy <- yday(dates)
# calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
# calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
# quick preview of new date calcualtions
datW[1,]

## Checking for missing data
# see how many values have missing data for each sensor observation
# air temp
length(which(is.na(datW$air.temperature)))
# wind speed
length(which(is.na(datW$wind.speed)))
# precipitation
length(which(is.na(datW$precipitation)))
# soil temp
length(which(is.na(datW$soil.moisture)))
# soil moisture
length(which(is.na(datW$soil.temp)))

# there's a lot of missing soil temp and soil moisture data
# make a plot of soil moisture with filled in points (using pch) line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

## QA/QC: Visual Checks
# make a plot with filled in points (using pch) line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
# be on the lookout for odd repeating patterns, extreme jumps in short time periods, or unrealistic values

# establish how to convert unreliable data to NA
# use example that isn't a problem in our data (below freezing temps in summer)

# make a new column to work with that indicates that I am conducting QA/QC
# because overwriting values should be done cautiously and can lead to confusing issues.
# It can be particularily confusing when you are just learning R.
# Here I'm using the 'ifelse' function
# the first argument is a logical statement to be evaluated as true or false on a vector
# the second argument is the value that my air.tempQ1 column will be given if the statement is true
# The last value is the value that will be given to air.tempQ1 if the statement is false.
# In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

## QA/QC: Realistic Values
# check the values at the extreme range of the data and throughout the percentiles
quantile(datW$air.tempQ1)
# look at days with really low air temperature, using temps output by quantile code
datW[datW$air.tempQ1 < 8,]
# now look at days with really high air temperature. using temps output by quantile code
datW[datW$air.tempQ1 > 33,]  

## Measurements outside of sensor capabilities
# use precipitation and lightning sensors to mark any air temp and wind data that would be unreliable
# plot precipitation and lightning strikes on the same plot
# normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
# make the plot with precipitation and lightning activity marked
# make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")

# plot precipitation points only when there is precipitation 
# make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

# plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)


## QUESTION 5 ##

print(lightscale)
# create vector of days when the 'lightscale' calculation was executed
a <- c(datW$DD[max(datW$precipitation)/max(datW$lightning.acvitivy)*datW$lightning.acvitivy])
# create a vector of days of lightscale
b <- c(datW$DD[lightscale])
# use assert function to determine if our vectors are the same
assert(length(a) == length(b), "error: unequal values")
# the above code shows that you can use lightscale to subset datW and you'll return
# the same number of days as when the lightscale calculations are done on their own
# this confirms the explanation I gave for Question 5
# also the environment shows that a and b have equal values which further confirms why we can use lightscale


## QUESTION 6 ##

# filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
# create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
# remove suspect measurements from wind speed measurements
datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$precipitation > 5, NA, datW$wind.speed))
# create vector of days when air.tempQ2 == NA
a1 <- c(datW$DD[datW$air.tempQ2 == "NA"])
# create a vector of days when wind.speedQ2 == NA
b1 <- c(datW$DD[datW$wind.speedQ2 == "NA"])
# use assert function to determine if our vectors are the same
assert(values(a1) == values(b1), "error: unequal values")

# create plot of wind.speedQ2
plot(datW$DD, datW$wind.speedQ2, xlab = "Day of Year", ylab = "Windspeed", type = "b")


## QUESTION 7 ##

# plot soil temp
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temp (Degrees C)")
# plot soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
# plot soil moisture next to precipitation
par(mfrow=c(1,2))
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
# plot soil temp next to air temp
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air Temp")
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temp (Degrees C)")


## QUESTION 8 ##

# create a table of average air temperature, wind speed, soil moisture, and soil temp
# add total precipitation
mean_airtemp <- mean(datW$air.temperature, na.rm = TRUE)
mean_windspeed <- mean(datW$wind.speed, na.rm = TRUE)
mean_soilmoisture <- mean(datW$soil.moisture, na.rm = TRUE)
mean_soiltemp <- mean(datW$soil.temp, na.rm = TRUE)
total_precip <- sum(datW$precipitation, na.rm = TRUE)

table <- c(mean_airtemp, mean_windspeed, mean_soilmoisture, mean_soiltemp, total_precip)
data.frame(table)


## QUESTION 9 ##

# change plot setting to display 4 charts
par(mfrow=c(2,2))

# plot soil moisture, air temp, soil temp, and precip
# soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
# air temp
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air Temp")
# precipitation
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")
# soil temp
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temp (Degrees C)")

