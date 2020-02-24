###################
# Activity 5 Script
# Tyler Pantle
# __/__/__
###################

#load in lubridate
library(lubridate)

# read in streamflow data
datH <- read.csv("Z:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)    

# read in precipitation data (hourly precipitation is in mm)
datP <- read.csv("Z:\\Data\\activities\\a05\\2049867.csv")                          
head(datP)

# based on QA/QC flags, use only the most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

## define time for streamflow using decimal days/day of year
# convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
# get day of year
datD$doy <- yday(datesD)
# calculate year
datD$year <- year(datesD)
# define time
timesD <- hm(datD$time)

## define time for precipitation
dateP <- ymd_hm(datP$DATE) # tell R what the date format is, save as dateP
# get day of year
datP$doy <- yday(dateP) # using the format, set datP doy as the dateP yday values
# get year 
datP$year <- year(dateP) # using the format, set datP year as the dateP year values

## get decimal formats
# convert time from a string to a more usable format w/ decimal hour
# start with streamflow
datD$hour <- hour(timesD) + (minute(timesD)/60)
# get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
# calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/367),
                       datD$year + (datD$decDay/366))
# use 366 and 367 days to account for the fact that dates start at 0 and not 1

# calculate decimal times for precipitation
datP$hour <- hour(dateP ) + (minute(dateP )/60)
# get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
# calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/367),
                       datP$year + (datP$decDay/366))

# plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
