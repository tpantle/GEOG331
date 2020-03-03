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
     xlab="Day of Year", 
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
axis(1, seq(0,360, by=31), #tick intervals
     labels = c(1,32,60,91,121,152,182,213,244,274,305,335)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend(240,87, c("mean","1 standard deviation","2017 Observations"), #legend items
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
plot(datD$decYear,datD$discharge, 
     type="l", 
     xlab="Decimal Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,425),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
full24$decYear <- ifelse(leap_year(full24$Group.2),full24$Group.2 + ((full24$Group.1-1)/366),
                         full24$Group.2 + ((full24$Group.1-1)/365))
points(full24$decYear,
       y = rep(410,length(full24$decYear)),
       type = "p",
       pch=3,
       col="blue",
       cex=0.5
)
axis(1, seq(2007,2019, by=1), #tick intervals
     lab=seq(2007,2019, by=1)) #tick labels
axis(2, seq(0,500, by=50),
     seq(0,500, by=50),
     las = 2)#show ticks at 90 degree angle
legend(2016,410, c("Discharge","Days with 24 Observations"), #legend items
       lwd=c(2,NA),#lines
       col=c("black","blue"),#colors
       pch=c(NA,3),#symbols
       bty="n")#no legend border

## making a hydrograph
# use September 5 and 6 in 2011
# subset precipitation and discharge dataframes to be limited to this period
# subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

# look at min discharge value to use for y-axis min
min(hydroD$discharge)

# since there are periods with no precipitation, adjust the scaling of precipitation values...
# ...to account for a y minimum value not at zero
# create 'pscale' variable  to account for these issues
# get minimum and maximum range of discharge to plot
# go outside of the range so that it's easy to see high/low values
# floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
# celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
# minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
# scale precipitation to fit  
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

# now create plot using variabels set up in the scaling code above
par(mai=c(1,1,1,1))
# make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


###### Question 8 ######

hydroD2 <- datD[datD$doy >= 362 & datD$doy < 364 & datD$year == 2012,]
hydroP2 <- datP[datP$doy >= 362 & datP$doy < 364 & datP$year == 2012,]

# look at min discharge value to use for y-axis min
min(hydroD2$discharge)

# since there are periods with no precipitation, adjust the scaling of precipitation values...
# ...to account for a y minimum value not at zero
# create 'pscale' variable  to account for these issues
# get minimum and maximum range of discharge to plot
# go outside of the range so that it's easy to see high/low values
# floor rounds down the integer
D2min <- floor(min(hydroD2$discharge))-1
# celing rounds up to the integer
D2max <- ceiling(max(hydroD2$discharge))+1
# minimum and maximum range of precipitation to plot
Pmin <- 0
Pmax <- ceiling(max(hydroP2$HPCP))+.5
# scale precipitation to fit on the 
hydroP2$pscale <- (((D2max-D2min)/(Pmax-Pmin)) * hydroP2$HPCP) + D2min

# now create plot using variabels set up in the scaling code above
par(mai=c(1,1,1,1))
# make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(D2min,D2max), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
# add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
  polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
            hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
          c(D2min,hydroP2$pscale[i],hydroP2$pscale[i],D2min),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


## Box plots and Violin plots
# use ggplot2 to make the plots
library(ggplot2)
# specify year as a factor
datD$yearPlot <- as.factor(datD$year)
# make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()
# make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

###### Question 9 ######

# make a violin plot of streamflow by season for 2016 and 2017
# start of spring = March 20 - doy = 80,79
# summer = June 20 - doy = 172,171
# fall = Sept 22 - doy = 266,265
# winter = dec 21 - doy = 356,355

datD16 <- datD[datD$year == "2016",]
datD17 <- datD[datD$year == "2017",]

# create a function to label 2016 seasons based on doy
datD16function <- function(doy){
  if(doy<80 | doy>=355){
    season<-"winter"}
  else if(doy>=80 & doy<172){
    season<-"spring"}
  else if(doy>=172 & doy<266){
    season<-"summer"}
  else{season<-"fall"}
  return(season)
}

# apply season function to the datD16 doy to label days as their respective season
datD16$season <- lapply(datD16$doy, datD16function)
datD16$season <- unlist(datD16$season)
datD16$season <- as.factor(datD16$season)
is.factor(datD16$season)

# make the 2016 season violin plot
ggplot(datD16, aes(season,discharge)) +
  geom_violin() + ggtitle("Discharge by Season (2016)")



# create a function to label 2017 seasons based on doy
datD17function <- function(doy){
  if(doy<79 | doy>=354){
    season<-"winter"}
  else if(doy>=79 & doy<171){
    season<-"spring"}
  else if(doy>=171 & doy<265){
    season<-"summer"}
  else{season<-"fall"}
  return(season)
}

# apply season function to the datD17 doy to label days as their respective season
datD17$season <- lapply(datD17$doy, datD17function)
datD17$season <- unlist(datD17$season)
datD17$season <- as.factor(datD17$season)
is.factor(datD17$season)

# make the 2017 season violin plot
ggplot(datD17, aes(season,discharge)) +
  geom_violin() + ggtitle("Discharge by Season (2017)")

