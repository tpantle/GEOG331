###################
# Activity 7 Script
# Tyler Pantle
# 4/27/20
###################

setwd("/Users/TylerPantle/Documents/DataSci")

install.packages(c("raster","sp","rgdal","rgeos","mapview","caret","randomForest","nnet"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(mapview)
library(caret)
library(randomForest)
library(nnet)

# set up directory for oneida data folder
dirR <- "/Users/TylerPantle/Documents/DataSci/oneida"


### SATELLITE IMAGERY

# read in Sentinel data

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB5 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B05_20m.tif"))
rdatB6 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B06_20m.tif"))
rdatB7 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B07_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))
rdatB11 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B11_20m.tif"))
rdatB12 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B12_20m.tif"))
clouds <- raster(paste0(dirR,"/sentinel/MSK_CLDPRB_20m.tif"))

# read in validation data
# here verbose=FALSE hiddes
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
built <- readOGR(paste0(dirR,"/Oneida/built.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

# stack red green blue bands
rgbS <- stack(rdatB4,rdatB3,rdatB2)
# stack all raster data
allbands <- stack(rdatB2,rdatB3,rdatB4,rdatB5,rdatB6,rdatB7, rdatB8,rdatB11, rdatB12,clouds)
# view true color raster image, maximum digital is around 20000 so set scale to that
plotRGB(rgbS, scale=20000)

# add linear contrast stretch for more variation in colors
plotRGB(rgbS, scale=20000, stretch="lin")

# use mapview package
# view rgb and set up a contrast stretch, exclude clouds with high values
viewRGB(rgbS,r=1,g=2,b=3,maxpixels =  2297430, #view all pixels don' lower resolution
        quantiles = c(0.00,0.995), #quantiles for stretch. Cuts off high reflectance from clouds
        homebutton=FALSE,
        viewer.suppress=FALSE) #view in Rstudio, don't generate html document

# look at randomly selected classification points using mapview package
# view rgb and set up a contrast stretch, exclude clouds with high values
# view all landcover types
# viewRGB(rgbS,r=1,g=2,b=3,maxpixels =  2297430,quantiles = c(0.00,0.995), homebutton=FALSE,
#        viewer.suppress=FALSE) + mapview(algae, color="grey25",col.regions="palegreen")+mapview(agri, color="grey25",col.regions="violet")+mapview(built, color="grey25",col.regions="darkgoldenrod3")+mapview(forest, color="grey25",col.regions="tan4")+mapview(water, color="grey25",col.regions="royalblue")+mapview(wetlands, color="grey25",col.regions="orangered2")

# let's exclude all cloud regions
plot(allbands[[10]])
# filter out values in cloud area with values above 60%
# set them to NA
allbandsCloud <- list()
for(i in 1:9){
  allbandsCloud[[i]] <- setValues(allbands[[i]],
                                  ifelse(getValues(allbands[[10]])>60,NA,getValues(allbands[[i]])))
  
}
allbandsCloudf <- stack(allbandsCloud[[1]],allbandsCloud[[2]],allbandsCloud[[3]],allbandsCloud[[4]],allbandsCloud[[5]],allbandsCloud[[6]],allbandsCloud[[7]],allbandsCloud[[8]],allbandsCloud[[9]])

# view all band layers
plot(allbandsCloudf)

# make false color image
# note that removing clouds brought digital number down to 10,000
plotRGB(allbandsCloudf,r=4, g=3, b=2,
        scale=10000, 
        stretch="lin", 
        margins=TRUE,
        colNA="grey50")

### CLASSIFY LANDCOVER

# when using random generation of numbers, it is good to set a random seed
# this generates random numbers in the same way everytime you run your script
# random seeds are good for reproducibility
# NOTE:only works if you run script in same order AND don't rerun random number function over and over again
# to ensure numbers are the same, set the seed BEFORE each vector
set.seed(12153)

# 'sample' function randomly selects numbers
# make a vector to indicate which points are training vs. validation
# randomly select algae points
algSamp <- sort(sample(seq(1,120),60))
# set up vector for data type
algData <- rep("train",120)
# randomly replace half of the data to be validating data
algData[algSamp] <- "valid"

# randomly select water points
waterSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
waterData <- rep("train",120)
#randomly replace half of the data to be validating data
waterData[waterSamp] <- "valid"

# randomly select agriculture points
agriSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
agriData <- rep("train",120)
#randomly replace half of the data to be validating data
agriData[agriSamp] <- "valid"

# randomly select built points
builtSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
builtData <- rep("train",120)
#randomly replace half of the data to be validating data
builtData[builtSamp] <- "valid"

# randomly select forest points
forestSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
forestData <- rep("train",120)
#randomly replace half of the data to be validating data
forestData[forestSamp] <- "valid"

# randomly select wetland points
wetlandsSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
wetlandsData <- rep("train",120)
#randomly replace half of the data to be validating data
wetlandsData[wetlandsSamp] <- "valid"

# set up dataframe that includes all raster data for each landclass type
# create id table that gives each landcover an ID
landclass <- data.frame(landcID= seq(1,6),
                        landcover = c("algal bloom", "open water","agriculture","built","forest","wetlands"))

# set up table with coordinates and data type (validate or train) for each point
landExtract <-  data.frame(landcID = rep(seq(1,6),each=120),
                           sampleType=c(algData,waterData,agriData,builtData,forestData, wetlandsData),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],built@coords[,1],forest@coords[,1],wetlands@coords[,1] ),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],built@coords[,2],forest@coords[,2],wetlands@coords[,2] ))

# extract raster data at each point using point coordinates
# extract into dataframe
rasterEx <- data.frame(extract(allbandsCloudf,landExtract[,3:4])) 
# give names of bands
colnames(rasterEx) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")

# combine point information with raster information
dataAll <- cbind(landExtract,rasterEx)
# preview table that now includes coordinates and raster info at each point
head(dataAll)

# now separate into training and validation points
trainD <- dataAll[dataAll$sampleType == "train",]
validD <- dataAll[dataAll$sampleType == "valid",]

# Random Forest
# use 'Caret' package to find the optimal parameter settings
# Kfold cross validation
# trainControl indicates how we want to set our parameter tuning
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
### random forests
# with random forests, the one parameter than can be tuned with 'Caret' is mtry
# this is the # of variables sampled as candidates in splitting the decision tree
# Typically the value for classification is square root of number of variables
rf.grid <- expand.grid(mtry=1:sqrt(9)) # number of variables available for splitting at each tree node (9 bc we have 9 bands)

# now that we've set up the tuning
# train the random forest model to the Sentinel-2 data through Caret
# note that "caret::" will make sure we use 'train'' from the caret package
trainD <- na.omit(trainD) #omit points in cloud areas from training  points
rf_model <- caret::train(x = trainD[,c(5:13)],  #digital number data
                         y = as.factor(trainD$landcID),  #land class we want to predict (landclass ID which is in the LandExtract dataframe we made)
                         method = "rf",  #use random forest
                         metric="Accuracy",  #assess by accuracy
                         trainControl = tc,  #use parameter tuning method that we created above
                         tuneGrid = rf.grid)  #parameter tuning grid
#check output
rf_model

# now that the model is trained, predictions can be made for the entire raster
# Change name in raster stack to match training data
names(allbandsCloudf) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")
# Apply the random forest model to the Sentinel-2 data
rf_prediction <- raster::predict(allbandsCloudf, model=rf_model)
#view predictions
plot(rf_prediction)
# landcover class names
landclass
# prediction looks pretty good initially

# change the continuous raster landclass legend on the map
# set up categorical colors for each class using hex codes
landclass$cols <-c("#a6d854","#8da0cb","#66c2a5",
                   "#fc8d62","#ffffb3","#ffd92f")
# make plot and hide legend
plot(rf_prediction, #random forest prediction
     breaks=seq(0,6), #number of landclasses
     col=landclass$cols , 
     legend=FALSE, axes=FALSE) #hide legend
legend("bottomleft", paste(landclass$landcover), #locate legend
       fill=landclass$cols ,bty="n")   

# evaluate predictions using validation dataset
# first you have to extract the validation data from the raster of predictions
# cell values at the cell coordinates
rf_Eval <- extract(rf_prediction, validD[,3:4])

# compare using a confusion matrix
# make the confusion matrix
rf_errorM <- confusionMatrix(as.factor(rf_Eval),as.factor(validD$landcID))
# add landcover names
colnames(rf_errorM$table) <- landclass$landcover
rownames(rf_errorM$table) <- landclass$landcover
# view the matrix
rf_errorM$table
# look at the overall accuracy
rf_errorM$overall

# Neural Networks
# set the parameter tuning in 'Caret' with the 'nnet' function
# requires parameters size (number of nodes) of the hidden layer in your network topology
# also need to specify the decay parameter that will be used to prevent overfitting in the model training
# set up grid
nnet.grid <- expand.grid(size = seq(from = 16, to = 28, by = 2), #number of nodes in the hidden layer 
                         decay = seq(from = 0.1, to = 0.6, by = 0.1)) # regularization parameter to avoid over-fitting 
# now train the model
nnet_model <- caret::train(x = trainD[,c(5:13)], 
                           y = as.factor(trainD$landcID),
                           method = "nnet", 
                           metric= "Accuracy", 
                           trainControl = tc, 
                           tuneGrid = nnet.grid,
                           trace=FALSE)
# view the training summary
nnet_model
# apply the neural network model to the Sentinel-2 data
nnet_prediction <- raster::predict(allbandsCloudf, model=nnet_model)
# make plot and hide legend
plot(nnet_prediction, #plot the neural network predictions
     breaks=seq(0,6), #number of landclasses
     col=landclass$cols ,
     legend=FALSE) #hide the legend
legend("bottomleft", paste(landclass$landcover), #locate the legend
       fill=landclass$cols ,bty="n")

# extract predictions and compare to validation data using confusion matrix
# extract predictions
nn_Eval = extract(nnet_prediction, validD[,3:4])
# confusion matrix
nn_errorM = confusionMatrix(as.factor(nn_Eval),as.factor(validD$landcID))
colnames(nn_errorM$table) <- landclass$landcover
rownames(nn_errorM$table) <- landclass$landcover
nn_errorM$table

# look at overall accuracy
nn_errorM$overall

# compare random forest and neural network maps side by side
par(mfrow=c(2,1), mai=c(0,0,0,0))
#random forest
plot(rf_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE)
#legend
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols ,bty="n")
#add title
mtext("Random Forest", side=3,cex=2, line=-5)

#neural network
plot(nnet_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
#add legend
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols, bty="n")   
#add title
mtext("Neural network", side=3,cex=2, line=-5)

### ANALYZING PREDICTIONS OF LAND COVER

# start by comparing area of land predicted to be an algal bloom (algal = 1)
# cell count neural net
freq(nnet_prediction)
# calculate nnet algal bloom cells * 400 (20mx20m cells = 400 sq m)
293519*400
# cell count random forest
freq(rf_prediction)
# calculate rf algal bloom cells * 400
253178*400
# nnet - rf area
(293519*400)-(253178*400)


### QUESTION 5
diff <- overlay(rf_prediction, nnet_prediction, fun=function(a,b) return(a==b))
plot(diff,
     col=c('#FFE4E1','#228B22'),
     legend=FALSE,
     axes=FALSE)
legend("left", legend=c("Agree", "Disagree"),
       col=c("#228B22", "#FFE4E1"), pch = 15, cex=0.8)



