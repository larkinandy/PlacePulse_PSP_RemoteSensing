

############ WithinCityModels.R ############
# Author: Andrew Larkin
# Developed for Perry Hystad, Oregon State University
# Date created: November 30th, 2018
# This script performs lasso varaible selection and incremental varaible buffer reduction for streetscape land use
# regression variables.  Models based on remote sensing estimates, street view image-based estimates, and a combination
# of the two are derived for perception scores of safety, lively, and beauty.  


####### load required packages #########
library(glmnet) # lasso regression
library(matrixStats)

################ helper functions ################


# calculate average RMSE of model predictions for each city in the dataset
calcCityRMSE <- function(inData,outcome) {
  
  
  cityNames <- unique(inData$CITY_NAME)
  
  rmse <- rep(0,length(cityNames)+1)
  medianPred <- rep(0,length(cityNames)+1)
  iqrPred <- rep(0,length(cityNames)+1)
  n <- rep(0,length(cityNames)+1)
  
  # stratify by city and store names 
  stratName <-rep("a",length(cityNames)+1)
  
  # calculate statistics for the entire dataset (i.e. all cities)
  medianActual <- rep(0,length(cityNames)+1)
  iqrActual <- rep(0,length(cityNames)+1)
  rmse[1] <-  sqrt(sum(inData$residuals^2)/length(inData$residuals))
  medianPred[1] <- median(inData$predictions)
  iqrPred[1] <- IQR(inData$predictions)
  stratName[1] <- "all"
  
  if(outcome == 'beautiful') {
    medianActual[1] <- median(inData$mu_beautiful)
    iqrActual[1] <- IQR(inData$mu_beautiful)
  }
  else if(outcome == 'safety') {
    medianActual[1] <- median(inData$mu_safety)
    iqrActual[1] <- IQR(inData$mu_safety)
  }
  
  else {
    medianActual[1] <- median(inData$mu_lively)
    iqrActual[1] <- IQR(inData$mu_lively)
  }
  
  # calculate statistics for each city 
  for(i in 1:length(cityNames)) {
    citySubset <- subset(inData,inData$CITY_NAME == cityNames[i])
    cat(length(citySubset$residuals))
    if(length(citySubset$residuals) >0) {
      rmse[i+1] <- sqrt(sum(citySubset$residuals^2)/length(citySubset$residuals))
      medianPred[i+1] <- median(citySubset$predictions)
      iqrPred[i+1] <- IQR(citySubset$predictions)
      if(outcome == 'beautiful') {
        medianActual[i+1] <- median(citySubset$mu_beautiful)
        iqrActual[i+1] <- IQR(citySubset$mu_beautiful)
      }
      else if(outcome == 'safety') {
        medianActual[i+1] <- median(citySubset$mu_safety)
        iqrActual[i+1] <- IQR(citySubset$mu_safety)
      }
      
      else {
        medianActual[i+1] <- median(citySubset$mu_lively)
        iqrActual[i+1] <- IQR(citySubset$mu_lively)
        
      }
      n[i+1] <- length(citySubset[,1])
      stratName[i+1] <- as.character(cityNames[i])
    }
  }
  
  # calculate ranks and difference in ranks for each city
  medianDiff <- medianPred - medianActual
  predRank <- rank(medianPred)
  actualRank <- rank(medianActual)
  rankDiff <- predRank - actualRank
  dataSet <- data.frame(rmse,medianPred,iqrPred,medianActual,iqrActual,medianDiff,predRank,actualRank, rankDiff,n,stratName)
  dataSet <- dataSet[order(dataSet$rankDiff),]
  return(dataSet)
  
} # end of calcCityRMSE




calcRedR <- function(predictionData,outcomeData,varsToReduce,startDrops) {
  redR <- rep(0,length(varsToReduce))
  drops <- c()
  keepDrops <- c(startDrops)
  tempModel <- predictionData[,!(names(predictionData)%in%keepDrops)]
  adjRMaster <- (summary(lm(outcomeData ~ as.matrix(tempModel))))$adj.r.squared
  for(i in 1:length(varsToReduce)) {
    drops <- c(keepDrops,varsToReduce[i])
    tempModel <- predictionData[,!(names(predictionData) %in% drops)]
    tempModelForm <- lm(outcomeData ~ as.matrix(tempModel))
    redR[i] <- (adjRMaster - summary(tempModelForm)$adj.r.squared)*100
  }
  return(redR)
}

# calculate percent variance explained for each city
calcCityRSquared <- function(inData,outcome) {
  
  # initialize result arrays
  cityNames <- unique(inData$CITY_NAME)
  SSR <- rep(0,length(cityNames)+1)
  SSTOTAL <- rep(0,length(cityNames)+1)
  PartialR <- rep(0,length(cityNames)+1)
  stratName <-rep("a",length(cityNames)+1)
  
  # calculate SSR and SSTOTAL for the entire dataset
  stratName[1] <- "all"
  if(outcome == 'beautiful') {
    meanActual <- mean(inData$mu_beautiful)
    SSTOTAL[1] <- sum((inData$mu_beautiful - meanActual)^2)
  }
  else if(outcome == 'safety') {
    meanActual <- mean(inData$mu_safety)
    SSTOTAL[1] <- sum((inData$mu_safety - meanActual)^2)
  }
  else {
    meanActual <- mean(inData$mu_lively)
    SSTOTAL[1] <- sum((inData$mu_lively - meanActual)^2)
  }
  
  SSR[1] <- sum(inData$residuals^2)
  
  # calculate SSR and SSTOTAL for each city
  for(i in 1:length(cityNames)) {
    citySubset <- subset(inData,inData$CITY_NAME == cityNames[i])
    if(outcome == 'beautiful') {
      meanActual <- mean(citySubset$mu_beautiful)
      SSTOTAL[i+1] <- sum((citySubset$mu_beautiful - meanActual)^2)
    }
    else if(outcome == 'safety') {
      meanActual <- mean(citySubset$mu_safety)
      SSTOTAL[i+1] <- sum((citySubset$mu_safety - meanActual)^2)
    }
    else {
      meanActual <- mean(citySubset$mu_lively)
      SSTOTAL[i+1] <- sum((citySubset$mu_lively - meanActual)^2)
    }
    
    SSR[i+1] <- sum(citySubset$residuals^2)
    stratName[i+1] <- as.character(cityNames[i])
  }
  
  # calculate partial R2 for all strat levels 
  PartialR <- 1 - (SSR/SSTOTAL)
  
  returnData <- data.frame(SSR,SSTOTAL,PartialR,stratName)
  return(returnData)
  
} # end of calcCityRSquared

######################## setup #####################

setwd("insert wd filepath here")

rawData <- read.csv("insert csv data filepath here")

imagePredictors <- c('accessibility_norm','allNature_norm','animate_norm','bluespace_norm',
                     'building_norm','builtEnv_norm','car_norm','grass_norm',
                     'greenspace_norm','otherNature_norm','person_norm','plant_norm',
                     'road_norm','sidewalk_norm','sky_norm','tree_norm','bench_norm',
                     'g_green_perc_norm','g_tree_perc_norm','g_grass_perc_norm',
                     'g_plant_perc_norm','g_field_perc_norm','g_flower_perc_norm',
                     'g_green_joint_norm','g_tree_joint_norm','g_grass_joint_norm',
                     'g_plant_joint_norm','g_field_joint_norm','g_flower_joint_norm',
                     'accessibility_ratio_norm','allNature_ratio_norm','animate_ratio_norm',
                     'bluespace_ratio_norm','building_ratio_norm','building_ratio_norm',
                     'builtEnv_ratio_norm','car_ratio_norm','grass_ratio_norm',
                     'otherNature_ratio_norm','person_ratio_norm','plant_ratio_norm',
                     'sidewalk_ratio_norm','sky_ratio_norm','tree_ratio_norm',
                     'bench_ratio_norm','greenspace_ratio_norm'
)



remotePredictors <- c('PM251000m_norm','mjRds100m_norm','mjRds250m_norm','NO2100m_norm',
                      'NO2250m_norm','tr100m_norm','tr250m_norm','imp1000m_norm',
                      'pop1000m_norm','NDVI250m_norm')


# image and remote sensing variables
allPredictors <- c(imagePredictors,remotePredictors)

################# SAFETY MAIN MODEL ########################
safeSubset <- subset(rawData,count_safety>=9)
safeSubset[is.na(safeSubset)] <- 0
safeOutcome <- safeSubset$mu_safety_norm
safePred <- safeSubset[allPredictors]
safeAllPredictors <- as.matrix(safePred[allPredictors])
safeImagePredictors <- as.matrix(safePred[imagePredictors])
safeRemotePredictors <- as.matrix(safePred[remotePredictors])

# lasso variable selection
cvfit <- glmnet::cv.glmnet(safeAllPredictors,safeOutcome,type.measure = "mse",standardize=TRUE,alpha = 1) # perform lasso regression
coefRaw <- coef(cvfit, s = "lambda.1se")


# get names of selected variables
nameVals <- rownames(coefRaw)
keeps <- c()
for( i in 2:length(coefRaw)) {
  if(coefRaw[i]^2 > 0) {
    keeps <- c(keeps,nameVals[i])
  }
  
}
keeps <- keeps[2:length(keeps)]
safeModelAll <- safePred[keeps]

# remove non-significant variables
drops <- c("building_ratio_norm.1","g_tree_joint_norm","plant_ratio_norm",
           "grass_norm","sky_norm","bench_norm","sidewalk_ratio_norm",
           "animate_ratio_norm","imp1000m_norm","mjRds100m_norm")
safeModelAll <- safeModelAll[ , !(names(safeModelAll) %in% drops)]
safeModelForm <- lm(safeOutcome ~ as.matrix(safeModelAll))
summary(safeModelForm)

# sensitivity analyses 

dropNew <- c("building_ratio_norm.1","g_tree_joint_norm","plant_ratio_norm",
             "grass_norm","sky_norm","bench_norm","sidewalk_ratio_norm",
             "animate_ratio_norm","imp1000m_norm","mjRds100m_norm")
b <- data.frame(c(calcRedR(safeModelAll,safeOutcome,keeps,dropNew)))
b$names <- keeps


############# BEAUTY MAIN MODEL ####################
beautySubset <- subset(rawData,count_beautiful>=4)
beautySubset[is.na(beautySubset)] <- 0
beautyOutcome <- beautySubset$mu_beautiful_norm
beautyPred <- beautySubset[allPredictors]
beautyAllPredictors <- as.matrix(beautyPred[allPredictors])
beautyImagePredictors <- as.matrix(beautyPred[imagePredictors])
beautyRemotePredictors <- as.matrix(beautyPred[remotePredictors])

# lasso variable selection
cvfit <- glmnet::cv.glmnet(beautyImagePredictors,beautyOutcome,type.measure = "mse",standardize=TRUE,alpha = 1) # perform lasso regression
coefRaw <- coef(cvfit, s = "lambda.1se")


# get names of selected variables
nameVals <- rownames(coefRaw)
keeps <- c()
for( i in 2:length(coefRaw)) {
  if(coefRaw[i]^2 > 0) {
    keeps <- c(keeps,nameVals[i])
  }
  
}
keeps <- keeps[1:length(keeps)]
beautyModelAll <- beautyPred[keeps]

# remove non-significant variables
drops <- c("g_plant_joint_norm","bench_ratio_norm","bluespace_norm","g_field_joint_norm","otherNature_norm","bench_norm","builtEnv_ratio_norm","person_ratio_norm","bluespace_ratio_norm",
           "g_grass_joint_norm","car_norm","accessibility_norm")
beautyModelAll <- beautyModelAll[ , !(names(beautyModelAll) %in% drops)]
beautyModelForm <- lm(beautyOutcome ~ as.matrix(beautyModelAll))
summary(beautyModelForm)

# sensitivity analyses 

dropNew <- c("g_field_joint_norm","otherNature_norm","bench_norm","builtEnv_ratio_norm","person_ratio_norm","bluespace_ratio_norm",
             "g_grass_joint_norm","car_norm","accessibility_norm")
b <- data.frame(c(calcRedR(beautyModelAll,beautyOutcome,keeps,dropNew)))
b$names <- keeps



############# LIVELY SETUP ####################
livelySubset <- subset(rawData,count_lively>=6)
livelySubset[is.na(livelySubset)] <- 0
livelyOutcome <- livelySubset$mu_lively_norm
livelyPred <- livelySubset[allPredictors]
livelyAllPredictors <- as.matrix(livelyPred[allPredictors])
livelyImagePredictors <- as.matrix(livelyPred[imagePredictors])
livelyRemotePredictors <- as.matrix(livelyPred[remotePredictors])

############# LIVELY MAIN MODEL ##############


# lasso variable selection
cvfit <- glmnet::cv.glmnet(livelyAllPredictors,livelyOutcome,type.measure = "mse",standardize=TRUE,alpha = 1) # perform lasso regression
coefRaw <- coef(cvfit, s = "lambda.1se")


# get names of selected variables
nameVals <- rownames(coefRaw)
keeps <- c()
for( i in 2:length(coefRaw)) {
  if(coefRaw[i]^2 > 0) {
    keeps <- c(keeps,nameVals[i])
  }
  
}
keeps <- keeps[1:length(keeps)]
livelyModelAll <- livelyPred[keeps]

# remove non-significant variables
drops <- c("building_ratio_norm.1","greenspace_norm","mjRds100m_norm","sidewalk_ratio_norm",
           "bench_norm","sky_norm","g_field_joint_norm","g_grass_joint_norm","g_flower_perc_norm",
           "person_norm")
livelyModelAll <- livelyModelAll[ , !(names(livelyModelAll) %in% drops)]
livelyModelForm <- lm(livelyOutcome ~ as.matrix(livelyModelAll))
summary(livelyModelForm)


dropNew <- c("bench_norm","sky_norm","g_field_joint_norm","g_grass_joint_norm","g_flower_perc_norm",
             "person_norm")
b <- data.frame(c(calcRedR(livelyModelAll,livelyOutcome,keeps,dropNew)))
b$names <- keeps





