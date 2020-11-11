

############ PlacePulseMainModels.R ############
# Author: Andrew Larkin
# Developed for Perry Hystad, Oregon State University
# Date created: November 30th, 2018
# This script performs lasso varaible selection and incremental varaible buffer reduction for streetscape land use
# regression variables.  Models based on remote sensing estimates, street view image-based estimates, and a combination
# of the two are derived for perception scores of safety, lively, and beauty.  


####### load required packages #########
library(glmnet) # lasso regression
library(matrixStats)



################## helper functions ###############

# calculate reduced R2 when removing one predictor variable #
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



################ end of helper functions ###########




######################## setup #####################

setwd("insert wd here")

rawData <- read.csv("insert dataset csv filepath here")


imagePredictors <- c(
  'accessibility','allNature','animate','bluespace','building','builtEnv','car','grass','greenspace',
  'otherNature','person','plant','road','sidewalk','sky','tree','bench',
  'g_green','g_tree','g_grass','g_plant','g_field','g_flower',
  'g_green_ratio','g_tree_ratio','g_grass_ratio','g_plant_ratio','g_field_ratio','g_flower_ratio',
  'accessibility_ratio','allNature_ratio','animate_ratio','bluespace_ratio','building_ratio',
  'building_ratio','builtEnv_ratio','car_ratio','grass_ratio','otherNature_ratio','person_ratio',
  'plant_ratio','sidewalk_ratio','sky_ratio','tree_ratio','bench_ratio','greenspace_ratio'
)


remotePredictors <- c(
  'PM251000m','mjRds100m','mjRds250m','NO2100m','NO2250m','tr100m','tr250m','imp1000m','pop1000m',
  'NDVI250m'
)



################# SAFETY MAIN MODEL ########################
safeSubset <- subset(rawData,count_safety>=9) # change cutoff for sensitivity analyses
safeSubset[is.na(safeSubset)] <- 0
safeOutcome <- safeSubset$mu_safety
safePred <- safeSubset[allPredictors]
safeAllPredictors <- as.matrix(safePred[allPredictors])
safeImagePredictors <- as.matrix(safePred[imagePredictors])
safeRemotePredictors <- as.matrix(safePred[remotePredictors])

# lasso variable selection
cvfit <- glmnet::cv.glmnet(safeRemotePredictors,safeOutcome,type.measure = "mse",standardize=TRUE,alpha = 1) # perform lasso regression
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
safeModelAll <- safePred[keeps]


# remove non-significant variables by hand if necessary
drops <- c()
safeModelAll <- safeModelAll[ , !(names(safeModelAll) %in% drops)]
safeModelForm <- lm(safeOutcome ~ as.matrix(safeModelAll))
summary(safeModelForm)




dropNew <- c()
b <- data.frame(c(calcRedR(safeModelAll,safeOutcome,keeps,dropNew)))
b$names <- keeps


############# BEAUTY MAIN MODEL ####################
beautySubset <- subset(rawData,count_beautiful>=9) # change cutoff for sensitivity analyses
beautySubset[is.na(beautySubset)] <- 0
beautyOutcome <- beautySubset$mu_beautiful
beautyPred <- beautySubset[allPredictors]
beautyAllPredictors <- as.matrix(beautyPred[allPredictors])
beautyImagePredictors <- as.matrix(beautyPred[imagePredictors])
beautyRemotePredictors <- as.matrix(beautyPred[remotePredictors])

# lasso variable selection
cvfit <- glmnet::cv.glmnet(beautyAllPredictors,beautyOutcome,type.measure = "mse",standardize=TRUE,alpha = 1) # perform lasso regression
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
drops <- c()
beautyModelAll <- beautyModelAll[ , !(names(beautyModelAll) %in% drops)]
beautyModelForm <- lm(beautyOutcome ~ as.matrix(beautyModelAll))
summary(beautyModelForm)


dropNew <- c()
b <- data.frame(c(calcRedR(beautyModelAll,beautyOutcome,keeps,dropNew)))
b$names <- keeps



############# LIVELY MAIN MODEL  ####################
livelySubset <- subset(rawData,count_lively>=6) # change cutoff for sensitivity analyses
livelySubset[is.na(livelySubset)] <- 0
livelyOutcome <- livelySubset$mu_lively
livelyPred <- livelySubset[allPredictors]
livelyAllPredictors <- as.matrix(livelyPred[allPredictors])
livelyImagePredictors <- as.matrix(livelyPred[imagePredictors])
livelyRemotePredictors <- as.matrix(livelyPred[remotePredictors])

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
drops <- c()
livelyModelAll <- livelyModelAll[ , !(names(livelyModelAll) %in% drops)]
livelyModelForm <- lm(livelyOutcome ~ as.matrix(livelyModelAll))
summary(livelyModelForm)


################## Example Code for creating a correlation heatmap ##############
# image and remote sensing variables
allPredictors <- c(imagePredictors,remotePredictors)

basicCorr <- c('otherNature','sky','accessibility','allNature','sidewalk','animate','car','building','bluespace','builtEnv','road','greenspace','mjRds100m','pop1000m','tr100m','NDVI250m','imp1000m','PM251000m','NO2100m','person','mu_lively','mu_safety','mu_beautiful')

my_palette <- colorRampPalette(c("red","white","blue"))(n=299)
corData <- rawData[basicCorr]
corData2 <- subset(corData,mu_lively>0)
rnames <- names(corData)
heatmap.2(cor(corData2),main="correlation",notecol="black",density.info="none",trace="none",margins=c(12,9),col=my_palette,dendrogram="col")


