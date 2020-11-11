import os
import numpy as np
import pp_constants as ppConst
from copy import deepcopy
from scipy import ndimage, misc # for focal stats
import pandas as ps
import cv2

# calculate joint count statistics and green screen values
class ImgFeatures:
    
    def __init__(self,imgFolder,npyFolder):
        self.categoryDict = self.defineCategoryDicts()
        self.allCategories = self.getAllCategories()
        self.imgFolder = imgFolder
        self.npyFolder = npyFolder
        self.imgFiles = os.listdir(self.imgFolder)
        self.npyFiles = os.listdir(self.npyFolder)
        self.numDict = self.addCateogryNumsToDict(self.categoryDict,self.getAllCategories())
        self.statCategories = ['ratio']
        self.header = self.createHeader(self.categoryDict,self.statCategories)
        if(len(self.imgFiles) != len(self.npyFiles)):
            print("warning: unequal number of img (%i) and npy (%i) files " %(len(self.imgFiles),len(self.npyFiles)))
        print("completed initializing the ImgFeatures object")
        self.greenPSPDict = {'tree':4,'grass':9,'plant':17,'field':29,'flower':66}
        
    # define sets or "categories" of PSPNet labels
    def defineCategoryDicts(self):
        categoryDict = {}
        categoryDict['greenspace'] = ['tree','grass','plant','field','flower']
        categoryDict['accessibility'] = ['sidewalk','escalator','path','stairs','stairway','bench','step']
        categoryDict['allNature'] = ['tree','grass','plant','field','land','flower','water','sea','waterfall','lake','earth',
                'mountain','rock','sky','sand','hill','dirt track']
        categoryDict['bluespace'] = ['water','sea','waterfall','lake']
        categoryDict['otherNature'] = ['earth','mountain','rock','sky','sand','hill','dirt track','land']
        categoryDict['animate'] = ['person','boat','car','bus','truck','airplane','van','ship','minibike','animal','bicycle']
        categoryDict['builtEnv'] = ['wall','building','road','windowpane','sidewalk','hovel','house','fence','railing',
               'signboard','skyscraper','path','stairs','runway','screen', 'door', 'screen door','stairway',
                'bridge','bench','booth','awning','streetlight','pole','bannister','escalator',
               'fountain','swimming pool','step','sculpture','traffic light','pier','bulletin board']
        return(categoryDict)
    
    # define all labels produced by the version of PSPNet we're using
    def getAllCategories(self):
        return(["wall","building","sky","floor","tree","ceiling","road","bed",
              "windowpane","grass","cabinet","sidewalk","person","earth",
              "door","table","mountain","plant","curtain","chair","car",
              "water","painting","sofa","shelf","house","sea","mirror",
              "rug","field","armchair","seat","fence","desk","rock",
              "wardrobe","lamp","bathtub","railing","cushion","base",
              "box","column","signboard","chest of drawers","counter",
              "sand","sink","skyscraper","fireplace","refrigerator",
              "grandstand","path","stairs","runway","case","pool table",
              "pillow","screen door","stairway","river","bridge","bookcase",
              "blind","coffee table","toilet","flower","book","hill","bench",
              "countertop","stove","palm","kitchen island","computer","swivel chair",
              "boat","bar","arcade machine","hovel","bus","towel","light",
              "truck","tower","chandelier","awning","streetlight","booth",
              "television receiver","airplane","dirt track","apparel","pole",
              "land","bannister","escalator","ottoman","bottle","buffet","poster",
              "stage","van","ship","fountain","conveyer belt","canopy","washer",
              "plaything","swimming pool","stool","barrel","basket","waterfall",
              "tent","bag","minibike","cradle","oven","ball","food","step","tank",
              "trade name","microwave","pot","animal","bicycle","lake","dishwasher",
              "screen","blanket","sculpture","hood","sconce","vase","traffic light",
              "tray","ashcan","fan","pier","crt screen","plate","monitor",
              "bulletin board","shower","radiator","glass","clock","flag"
    ])
    
    # add key-values for composite categories to the dictionary of PSPNet labels and their numpy integer values
    # INPUTS: 
    #    inDict (dictionary) - key-value pairs for PSPNet labels and their numpy integer values
    #    categories (list of str lists) - custom composite categories with the set of PSPNet labels 
    #                                     that make up each composite cateogry
    # OUTPUTS:
    #    newDict (dictionary) - the input dict, updated with key-value pairs for composite categories
    def addCateogryNumsToDict(self,inDict,categories):
        keys = deepcopy(list(inDict.keys()))
        keys.sort()
        newDict = {}
        for key in keys:
            keyVals = inDict[key]
            tempList = []
            for val in keyVals:
                tempList.append(categories.index(val))
            newDict[key + "_num"] = tempList
        return(newDict)
    
    # screen numpy arrays for category(ies) of interest
    # INPUTS:
    #    categoryNums (integer array) - imgArray numbers that should return positive 
    #    imgArray (2d integer matrix) - PSPNet lables for each pixel in the image
    # OUTPUTS:
    #    (2d boolean matrix) - For each pixel, True if pixel belongs to one of the categories of interest,
    #                          False otherwise
    def createBinaryCategorical(self,categoryNums,imgArray):
        return(np.isin(imgArray,categoryNums))
    
    # create header for output statistics.  Used for writing arrays out to csv files. Note that order is 
    # tightly coupled with other class operations.  If other operations are modified, this function
    # may need to be updated as well
    # INPUTS:
    #    catDict(dictionary) - key value pairs, where the key corresponds to composite categories and 
    #                           values are the set of PSPNet labels that make up the composite category
    #    statsCategories (string array) - list of derived statistics (e.g. mean, max)
    # OUTPUTS:
    #    header (string) - header for output csv file
    def createHeader(self,catDict,statCategories):
        categories = list(catDict.keys())
        categories.sort()
        header = ["filename"]
        subSep = "_"
        for category in categories:
            for subCat in statCategories:
                header.append(category + subSep + subCat)
            subGroups = catDict[category]
            for subGroup in subGroups:
                for subCat in statCategories:
                    header.append(subGroup + subSep + subCat)
        return(header)
    
    # calculate joint count statistics for one image and one PSPNet label or composite category
    # INPUTS:
    #    imgArr (2d integer matrix) - binary 2d matrix - For each pixel, 1 if pixel belongs to the label
    #                                 of interest, 0 otherwise
    # OUTPUTS:
    #    (float) - joint count statistic
    def calcStatsOneImageOneLabel(self,imgArr):
        neighbImg = np.multiply(ndimage.uniform_filter(imgArr*1.0,2),imgArr*1.0)*100
        return([np.sum(neighbImg)/np.sum(imgArr)])
    
    # calculate joint count statistics for a single composite category
    # INPUTS:
    #    numDict (dictionary) - key-value pairs of composite labels and corresponding PSPNet integer values
    #    catName (str) - name of the category to calculate statistics for.  Used for the dictionary
    #    results (float array) - joint count statistics derived for all categories so far. Each
    #                            impelementation of the function appends a value to the array
    #    img (2d integer array) - PSPNet class labels for all pixels in an image
    # OUTPUTS:
    #    results (float array) - input results, appended with the derived statistical value
    def processSingleCategory(self,numDict,catName,results,img):
        catBinary = self.createBinaryCategorical(numDict[catName],img)
        #numPixels = (img.shape[0]*img.shape[1]*4 - 2*(img.shape[0]+img.shape[1]))
        catResults = np.asarray(self.calcStatsOneImageOneLabel(catBinary))
        if(results.shape[0] == 0):
            results = catResults.reshape((catResults.shape[0],1))
        else:
            results = np.concatenate((results,catResults.reshape((catResults.shape[0],1))))
        for subsetCat in numDict[catName]:
            binarySubset = self.createBinaryCategorical([subsetCat],img)
            numPixels = np.sum(binarySubset*1)
            tempResults = None
            if(numPixels > 0):
                tempResults = np.asarray(self.calcStatsOneImageOneLabel(binarySubset))
            else:
                numOutcomes = 1
                tempResults = fillArray = np.full((numOutcomes,1),np.nan)
            results = np.concatenate((results,tempResults.reshape((tempResults.shape[0],1))))
        return(results)
    
    # derive joint count statistics for all categories and labels of interest for a single image
    # INPUTS:
    #    imgFilepath (str) - absolute filepath where .npy file is stored
    #    numDict (dictionary) - key-value pairs of categories and corresponding PSPNEt integer values
    #    imgName (str) - relative filepath of .npy image (i.e. filename)
    # OUTPUTS:
    #    results (array) - derived joint count statistics for the image being processed
    def processSingleImage(self,imgFilepath,numDict,imgName):
        img = np.load(imgFilepath)
        # seed results array with image filename
        results = np.asarray([imgName]).reshape((1,1))
        categories = list(numDict.keys())
        categories.sort()
        for category in categories:
            results = self.processSingleCategory(numDict,category,results,img)
        return(results)

    # deriving spatial statistics can take a long time.  This function is to identify which images within a folder
    # have already been processed, and which images still need to be processed for deriving spatial statistics
    # INPUTS:
    #    imageFolder (str) - absolute filepath to folder containing candidate images to process
    #    areadyProcessed (str array) - relative filepaths of images that have already been processed
    #    debug (boolean) - whether or not to print progress updates, as part of the debugging and throughput evaluation
    # OUTPUTS:
    #    filesToProcess (str array) - list of files which still need to be processed
    def getImgsToProcess(self,imageFolder,alreadyProcessed=[],debug=False):
        candidateFiles = os.listdir(imageFolder)
        filesToProcess = []
        self.alreadyProcessed = list(alreadyProcessed)
        index = 0
        for candidate in candidateFiles:
            if(candidate[-3:] == 'npy' and candidate not in alreadyProcessed.values):
                filesToProcess.append(candidate)
            index+=1
            if(index%5000==0):
                print(index)
        filesToProcess = [i for i in filesToProcess if i] 
        if debug:
            print("found %i images to process.  %i images were already processed" %(len(filesToProcess),len(candidateFiles)-len(filesToProcess)))
        return filesToProcess

    # process all images within a given folder
    # INPUTS: 
    #    imageFolder (str) - absoluste filepath to folder containing PSPNet image predictions
    #    numDict (dictionary) - set of integers that belong to each category
    # OUTPUTS:
    #    results (float numpy array) - summary statistics for each images within the folder
    def processAllImagesSpatial(self,imageFolder,filesToProcess,numDict={},debug=False):
        if(len(numDict.keys())==0):
            numDict = self.numDict
        results = np.asarray([])
        index = 0
        for filename in filesToProcess:
            filepath = imageFolder + "/" + filename
            try:
                tempResults = self.processSingleImage(filepath,numDict,filename)
                if(results.shape[0] ==0):
                    results = tempResults.reshape((tempResults.shape[0],1))
                else:
                    results = np.concatenate((results,tempResults.reshape((tempResults.shape[0],1))),axis=1)
            except Exception:
                print("couldn't process image %s " %(filename))
            index+=1
            if(debug and index%100==0):
                print("derived spatial statistics for %i images" %(index))
        resultsDataframe = ps.DataFrame(results.transpose())
        resultsDataframe.columns = self.header
        return(resultsDataframe)
    
    # calculate joint count statistics for a single composite category
    # INPUTS:
    #    numDict (dictionary) - key-value pairs of composite labels and corresponding PSPNet integer values
    #    catName (str) - name of the category to calculate statistics for.  Used for the dictionary
    #    results (float array) - joint count statistics derived for all categories so far. Each
    #                            impelementation of the function appends a value to the array
    #    img (2d integer array) - PSPNet class labels for all pixels in an image
    # OUTPUTS:
    #    results (float array) - input results, appended with the derived statistical value
    def processSingleCategory(self,numDict,catName,results,img):
        catBinary = self.createBinaryCategorical(numDict[catName],img)
        #numPixels = (img.shape[0]*img.shape[1]*4 - 2*(img.shape[0]+img.shape[1]))
        catResults = np.asarray(self.calcStatsOneImageOneLabel(catBinary))
        if(results.shape[0] == 0):
            results = catResults.reshape((catResults.shape[0],1))
        else:
            results = np.concatenate((results,catResults.reshape((catResults.shape[0],1))))
        for subsetCat in numDict[catName]:
            binarySubset = self.createBinaryCategorical([subsetCat],img)
            numPixels = np.sum(binarySubset*1)
            tempResults = None
            if(numPixels > 0):
                tempResults = np.asarray(self.calcStatsOneImageOneLabel(binarySubset))
            else:
                numOutcomes = 1
                tempResults = fillArray = np.full((numOutcomes,1),np.nan)
            results = np.concatenate((results,tempResults.reshape((tempResults.shape[0],1))))
        return(results)
    
    # calculate joint count statistics for a single composite category
    # INPUTS:
    #    results (float array) - joint count statistics derived for all categories so far. Each
    #                            impelementation of the function appends a value to the array
    #    img (2d integer array) - PSPNet class labels for all pixels in an image
    # OUTPUTS:
    #    results (float array) - input results, appended with the derived statistical value
    def processSingleCategoryGreenScreen(self,results,img):
        catBinary = self.createBinaryCategorical(1,img)
        #numPixels = (img.shape[0]*img.shape[1]*4 - 2*(img.shape[0]+img.shape[1]))
        catResults = self.calcStatsOneImageOneLabel(catBinary)
        #results = np.concatenate((results,catResults.reshape((catResults.shape[0],1))))
        return(catResults[0])
    
    def loadJpgImg(self,imgFilepath):
        img = cv2.imread(imgFilepath)
        hsv = cv2.cvtColor(img, cv2.COLOR_RGB2HSV)
        return(hsv)
    
    # screen for green pixels within an hsv image
    def applyGreenScreen(self,hsvImage):
        lower_green = np.array([57,26,0])
        upper_green = np.array([98,255,255])
        kernel = np.ones((5,5),np.uint8)
        mask = cv2.inRange(hsvImage, lower_green, upper_green)
        opening = cv2.morphologyEx(mask, cv2.MORPH_OPEN, kernel)
        res = cv2.bitwise_and(hsvImage,hsvImage, mask= opening)
        output = cv2.cvtColor(res, cv2.COLOR_HSV2RGB)
        return(output)
    
    # apply the screen based on PSPNet to images previously screened for green pixels
    def applyNPYScreen(self,greenScreenedImg,npyScreen):
        output = deepcopy(greenScreenedImg)
        output[:,:,0] = np.multiply(output[:,:,0],npyScreen)
        output[:,:,1] = np.multiply(output[:,:,1],npyScreen)
        output[:,:,2] = np.multiply(output[:,:,2],npyScreen)
        return(output)
    
    # calculate green pixel statistics for one image and one PSPNet category
    # INPUTS:
    #    valsToScreen (str int) - PSPNet integers that corrrespond to the objects that should be included in the screened image
    #    results (np array) = results for previous images.  Statistics for the images to process will be appended to results
    #    npyImg (np integer array) - numpy images of PSPNet maximum likelihood classification for each pixel
    #    screenedHSV (float array) - image to process, in hsv format
    # OUTPUTS:
    #    
    def performGreenScreenOneImgOneCategory(self,valsToScreen,results,npyImg,screenedHSV):
        outputSum = np.sum(np.minimum(np.maximum(screenedHSV[:,:,0] + screenedHSV[:,:,1] + screenedHSV[:,:,2],0),1))
        npyScreened = self.createBinaryCategorical(valsToScreen,npyImg)
        output = self.applyNPYScreen(screenedHSV,npyScreened)
        binaryRaster = np.minimum(np.maximum(output[:,:,0] + output[:,:,1] + output[:,:,2],0),1)
        outputSum = np.sum(binaryRaster)
        numPixels = output.shape[0]*output.shape[1]
        results = np.asarray(['a']).reshape((1,1))
        jointCountStat = self.processSingleCategoryGreenScreen(results,binaryRaster)
        percentPixels = (outputSum/numPixels)*100
        return([jointCountStat,percentPixels])
    
    # derive joint count statistics for all categories and labels of interest for a single image
    # INPUTS:
    #    imgFilepath (str) - absolute filepath where .npy file is stored
    #    numDict (dictionary) - key-value pairs of categories and corresponding PSPNEt integer values
    #    imgName (str) - relative filepath of .npy image (i.e. filename)
    # OUTPUTS:
    #    results (array) - derived joint count statistics for the image being processed
    def processSingleImageGreen(self,jpgFilepath,npyFilepath):
        try:
            img = self.loadJpgImg(jpgFilepath)
            screenedImg = self.applyGreenScreen(img)
            npyImg = np.load(npyFilepath)
            results = self.performGreenScreenOneImgOneCategory(
            self.numDict['greenspace_num'],[],npyImg,screenedImg
        )
        except Exception as e:
            print("couldn't load imagery for file %s" %(jpgFilepath))
            return np.ones((1,1))
        for label in self.greenPSPDict.keys():
            try:
                tempResults = self.performGreenScreenOneImgOneCategory(
                    self.greenPSPDict[label],results,npyImg,screenedImg
                )
                results += tempResults
            except Exception as e:
                print("error: could process img %s. %s" %(jpgFilepath,str(e)))
                return np.ones((1,1))
        return(results)
    
    def createGreenspaceHeader(self):
        varNames = ['green','tree','grass','plant','field','flower']
        stats = ['joint','perc']
        headerList = []
        for name in varNames:
            for stat in stats:
                headerList.append('g_' + name + "_" + stat)
        return(headerList)
    
    def processAllImagesGreen(self,debug=False):
        imgNames = []
        greenResults = np.ones((1,1))
        index=0
        for img in self.imgFiles:
            index+=1
            #if(index>2500):
            #    resultsDataframe = ps.DataFrame(greenResults.transpose())
            #    resultsDataframe.columns = self.createGreenspaceHeader()
            #    resultsDataframe['filename'] = imgNames
            #    return(resultsDataframe)
            if(debug and index%5000 ==0):
                print("processed %i images" %(index))
            npyImg = img[:-4]+'.npy'
            if (npyImg in self.npyFiles):
                tempResults = np.array(self.processSingleImageGreen(
                        self.imgFolder + "/" + img,
                        self.npyFolder + "/" + npyImg
                    )).astype(np.double)
                if(greenResults.shape[0]==1):
                    imgNames.append(img[:-4])
                    greenResults = np.array(tempResults).astype(np.double).reshape((12,1))
                else:
                    if(tempResults.shape[0]>1):
                        imgNames.append(img[:-4])
                        greenResults = np.concatenate((greenResults,tempResults.reshape((12,1))),axis=1)
        resultsDataframe = ps.DataFrame(greenResults.transpose())
        resultsDataframe.columns = self.createGreenspaceHeader()
        resultsDataframe['filename'] = imgNames
        return(resultsDataframe)
    
    # images were processed in batches, with subsequent batches of csv files.  This function loads a set of 
    # csv files and combines them.  Assumes files have identical variable column order
    # INPUTS:
    #    spatialFolder (str) - folder where csv files are stored
    # OUTPUTS:
    #    spatialDF ()
    def loadSpatialFiles(self,spatialFolder):
        filesToLoad = os.listdir(spatialFolder)
        spatialDF = ps.DataFrame()
        for file in filesToLoad:
            loadedFile = ps.read_csv(spatialFolder + "/" + file)
            spatialDF = ps.concat([spatialDF,loadedFile], ignore_index=True)
        spatialDF = spatialDF.fillna(0)
        return(spatialDF)