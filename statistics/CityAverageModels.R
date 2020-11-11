
############ CityAverageModels.R ############
# Author: Andrew Larkin
# Developed for Perry Hystad, Oregon State University
# Date created: November 30th, 2018
# This script performs lasso varaible selection and incremental varaible buffer reduction for streetscape land use
# regression variables.  Models based on remote sensing estimates, street view image-based estimates, and a combination
# of the two are derived for perception scores of safety, lively, and beauty. 


##################### helper functions ####################

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


#################### end of helper functions ##################

####################### setup #############################


setwd("define wd filepath here")

rawData <- read.csv("define dataset csv filepath here")

imagePredictors <- c('accessibility_city_mean','allNature_city_mean','animate_city_mean',
                     'bluespace_city_mean','building_city_mean','builtEnv_city_mean',
                     'car_city_mean','grass_city_mean','greenspace_city_mean',
                     'otherNature_city_mean','person_city_mean','plant_city_mean',
                     'road_city_mean','sidewalk_city_mean','sky_city_mean',
                     'tree_city_mean','bench_city_mean','g_green_perc_city_mean',
                     'g_tree_perc_city_mean','g_grass_perc_city_mean',
                     'g_plant_perc_city_mean','g_field_perc_city_mean',
                     'g_flower_perc_city_mean','g_green_joint_city_mean',
                     'g_tree_joint_city_mean','g_grass_joint_city_mean',
                     'g_plant_joint_city_mean','g_field_joint_city_mean',
                     'g_flower_joint_city_mean','accessibility_ratio_city_mean',
                     'allNature_ratio_city_mean','animate_ratio_city_mean',
                     'bluespace_ratio_city_mean','building_ratio_city_mean',
                     'building_ratio_city_mean','builtEnv_ratio_city_mean',
                     'car_ratio_city_mean','grass_ratio_city_mean',
                     'otherNature_ratio_city_mean','person_ratio_city_mean',
                     'plant_ratio_city_mean','sidewalk_ratio_city_mean',
                     'sky_ratio_city_mean','tree_ratio_city_mean',
                     'bench_ratio_city_mean','greenspace_ratio_city_mean'
)



remotePredictors <- c('PM251000m_city_mean','mjRds100m_city_mean',
                      'mjRds250m_city_mean','NO2100m_city_mean',
                      'NO2250m_city_mean','tr100m_city_mean',
                      'tr250m_city_mean','imp1000m_city_mean',
                      'pop1000m_city_mean','NDVI250m_city_mean')


varsToKeep <- c("greenspace_city_mean","g_green_perc_city_mean","g_green_joint_city_mean","tree_city_mean",
                "builtEnv_city_mean","road_city_mean","animate_city_mean","person_city_mean","building_city_mean",
                "accessibility_city_mean","sidewalk_city_mean","NDVI250m_city_mean","imp1000m_city_mean",
                "pop1000m_city_mean","tr100m_city_mean","mjRds100m_city_mean","CITY_NAME")


varsToKeepStd <- c("greenspace_city_std","g_green_perc_city_std","g_green_joint_city_std","tree_city_std",
                "builtEnv_city_std","road_city_std","animate_city_std","person_city_std","building_city_std",
                "accessibility_city_std","sidewalk_city_std","NDVI250m_city_std","imp1000m_city_std",
                "pop1000m_city_std","tr100m_city_std","mjRds100m_city_std","CITY_NAME")

# image and remote sensing variables
allPredictors <- c(imagePredictors,remotePredictors)


##################### end of setup ####################


################# SAFETY MAIN MODEL ########################
safeSubset <- rawData
safeSubset[is.na(safeSubset)] <- 0
safeOutcome <- safeSubset$mu_safety_city_mean
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
drops <- c("g_flower_joint_city_mean","allNature_ratio_city_mean","tr100m_city_mean",
           "bench_ratio_city_mean","grass_ratio_city_mean","road_city_mean","animate_city_mean",
           "g_field_perc_city_mean","bluespace_city_mean")
safeModelAll <- safeModelAll[ , !(names(safeModelAll) %in% drops)]
safeModelForm <- lm(safeOutcome ~ as.matrix(safeModelAll))
summary(safeModelForm)

# sensitivity analyses, modify by hand to test how dropping additional variables influences regression model

dropNew <- c("g_flower_joint_city_mean","allNature_ratio_city_mean","tr100m_city_mean",
             "bench_ratio_city_mean","grass_ratio_city_mean","road_city_mean","animate_city_mean",
             "g_field_perc_city_mean")
b <- data.frame(c(calcRedR(safeModelAll,safeOutcome,keeps,dropNew)))
b$names <- keeps



################# BEAUTY MAIN MODEL ########################
beautySubset <- rawData
beautySubset[is.na(beautySubset)] <- 0
beautyOutcome <- beautySubset$mu_beautiful_city_mean
beautyPred <- beautySubset[allPredictors]
beautyllPredictors <- as.matrix(beautyPred[allPredictors])
beautyImagePredictors <- as.matrix(beautyPred[imagePredictors])
beautyRemotePredictors <- as.matrix(beautyPred[remotePredictors])

# lasso variable selection
cvfit <- glmnet::cv.glmnet(beautyllPredictors,beautyOutcome,type.measure = "mse",standardize=TRUE,alpha = 1) # perform lasso regression
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
drops <- c("building_ratio_city_mean.1","mjRds250m_city_mean","NDVI250m_city_mean","grass_ratio_city_mean",
           "plant_ratio_city_mean","g_flower_joint_city_mean","tr100m_city_mean","accessibility_city_mean",
           "NO2100m_city_mean","g_field_perc_city_mean","pop1000m_city_mean","allNature_ratio_city_mean",
           "animate_city_mean","building_ratio_city_mean","imp1000m_city_mean","sidewalk_city_mean")
beautyModelAll <- beautyModelAll[ , !(names(beautyModelAll) %in% drops)]
beautyModelForm <- lm(beautyOutcome ~ as.matrix(beautyModelAll))
summary(beautyModelForm)

dropNew <- c()
b <- data.frame(c(calcRedR(beautyModelAll,beautyOutcome,keeps,dropNew)))
b$names <- keeps




############# LIVELY SETUP ####################
livelySubset <- rawData
livelySubset[is.na(livelySubset)] <- 0
livelyOutcome <- livelySubset$mu_lively_city_mean
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
drops <- c("animate_ratio_city_mean")
livelyModelAll <- livelyModelAll[ , !(names(livelyModelAll) %in% drops)]
livelyModelForm <- lm(livelyOutcome ~ as.matrix(livelyModelAll))
summary(livelyModelForm)

dropNew <- c()
b <- data.frame(c(calcRedR(livelyModelAll,livelyOutcome,keeps,dropNew)))
b$names <- keeps
