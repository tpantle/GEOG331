###################
# Activity 5 Script
# Tyler Pantle
# 3/4/20
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
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))

# calculate decimal times for precipitation
datP$hour <- hour(dateP ) + (minute(dateP )/60)
# get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
# calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))

# plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

# plot the average daily discharge across all years with the standard deviation for each day
# start by calculating the average
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

# start new plot window using 'dev.new' to start it with a standard size
dev.new(width=8,height=8)

# bigger margins using 'mai' argument
par(mai=c(1,1,1,1))0
# make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

# add a polygon showing the standard deviation interval around lines
# to do so, Y-axis needs to be larger in order to full show the range of SD
# X-axis also needs to be larger
# bigger margins
par(mai=c(1,1,1,1))
# make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
# show SD around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

# axis can also be adjusted using the 'axis' function to turn off the default axes
# 'las' function allows you to change the direction of the axis ticks
par(mai=c(1,1,1,1))
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2) # show y-axis ticks at 90 degree angle

# now add a legend in the top right
par(mai=c(1,1,1,1))
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show y-axis ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

# change box to square point for better alignment
par(mai=c(1,1,1,1))
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

###### Question 5 ######

# add a line showing observations for 2017
# create variable of 2017 observations
# datD$month <- month(datesD) ......add month column to datD
obs2017 <- datD[datD$year == '2017',]

ave2017 <- aggregate(obs2017$discharge, by=list(obs2017$doy), FUN="mean")
colnames(ave2017) <- c("doy","dailyAve")


par(mai=c(1,1,1,1))
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xlim=c(0,360),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
lines(ave2017$doy,ave2017$dailyAve,
      col="red")
axis(1, seq(0,360, by=40), #tick intervals
######################
     lab=seq(0,11, by=1)) #tick labels
######################
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation","2017"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

###### Question 7 ######

agg <- aggregate(datP,
          by = list(datP$doy,datP$year),
          length)
full24 <- agg[agg$doy == "24",]
# within 'full24', 'Group.1' is the DOYs for which there are 24 observations

par(mai=c(1,1,1,1))
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       


#Q7
points(aveF$dailyAve[aveF$doy == agg$Group.1])



axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border