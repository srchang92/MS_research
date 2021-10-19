#################################################
###########  620 Final Project ##################
###########                    ##################
###########    Stephen Chang   ##################
#################################################

##### Hello Fellow travelers, this is a script I wrote to do the 
##### raster stuff for RandomForest. The Vector manipulation was 
##### done in ArcGIS or QGIS, they include basic buffering functions

###### Load Packages ######
library(raster)           #for raster covariate data; version 2.6-7 used
library(reshape2)         #for re-formatting data; version 1.4.3 used
library(mgcv)             #for gams; version 1.8-24 used
library(dismo)            #for SDMs; version 1.1-4 used
library(randomForest)     #for random forest SDMs; version 4.6-14 used
library(maxnet)           #maxent with maxnet; version 0.1.2 used
library(glmnet)           #needed for maxnet; version 2.0-16 used
library(MuMIn)            #for model selection; version 1.42.1 used
library(PresenceAbsence)  #for model evaluation; version 1.1.9 used
library(ecospat)         #for model evaluation; version 3.0 used
library(ggplot2)
library(viridis)
library(data.table)
library(rgdal)
library(dplyr)
library(splancs)

rm(list = ls())

#set working directory where data were downloaded
setwd("C:/Users/srcha/Documents/Grad_school/research/usb_12-8-20/620_final_project")

##### Read in Rasters #####
### 1994 ###
#b1_1994 <- raster('LT05_L2SP_158077_19940630_20200913_02_T1/LT05_L2SP_158077_19940630_20200913_02_T1_SR_B1.tif')
#plot(b1_1994)
#b2_1994 <- raster('LT05_L2SP_158077_19940630_20200913_02_T1/LT05_L2SP_158077_19940630_20200913_02_T1_SR_B2.tif')
#b3_1994 <- raster('LT05_L2SP_158077_19940630_20200913_02_T1/LT05_L2SP_158077_19940630_20200913_02_T1_SR_B3.tif')
#b4_1994 <- raster('LT05_L2SP_158077_19940630_20200913_02_T1/LT05_L2SP_158077_19940630_20200913_02_T1_SR_B4.tif')
#b5_1994 <- raster('LT05_L2SP_158077_19940630_20200913_02_T1/LT05_L2SP_158077_19940630_20200913_02_T1_SR_B5.tif')

### 2005 ###
b1_2005_blue <- raster('LT05_L2SP_158077_20050815_20200902_02_T1/LT05_L2SP_158077_20050815_20200902_02_T1_SR_B1.tif')
b2_2005_green <- raster('LT05_L2SP_158077_20050815_20200902_02_T1/LT05_L2SP_158077_20050815_20200902_02_T1_SR_B2.tif')
b3_2005_red <- raster('LT05_L2SP_158077_20050815_20200902_02_T1/LT05_L2SP_158077_20050815_20200902_02_T1_SR_B3.tif')
b4_2005_NIR <- raster('LT05_L2SP_158077_20050815_20200902_02_T1/LT05_L2SP_158077_20050815_20200902_02_T1_SR_B4.tif')
b5_2005_SWIR1 <- raster('LT05_L2SP_158077_20050815_20200902_02_T1/LT05_L2SP_158077_20050815_20200902_02_T1_SR_B5.tif')

### 2014 ###
b1_2014_violet <- raster('LC08_L2SP_158077_20140621_20200911_02_T1/LC08_L2SP_158077_20140621_20200911_02_T1_SR_B1.tif')
b2_2014_blue <- raster('LC08_L2SP_158077_20140621_20200911_02_T1/LC08_L2SP_158077_20140621_20200911_02_T1_SR_B2.tif')
b3_2014_green <- raster('LC08_L2SP_158077_20140621_20200911_02_T1/LC08_L2SP_158077_20140621_20200911_02_T1_SR_B3.tif')
b4_2014_red <- raster('LC08_L2SP_158077_20140621_20200911_02_T1/LC08_L2SP_158077_20140621_20200911_02_T1_SR_B4.tif')
b5_2014_NIR <- raster('LC08_L2SP_158077_20140621_20200911_02_T1/LC08_L2SP_158077_20140621_20200911_02_T1_SR_B5.tif')
b6_2014_SWIR1 <- raster('LC08_L2SP_158077_20140621_20200911_02_T1/LC08_L2SP_158077_20140621_20200911_02_T1_SR_B6.tif')



##### Read in vectors #####
polys <- readOGR("vectors/andohahela_polys.shp")
buffers <- readOGR('vectors/dissolved_village_buffers_proj.shp')
test <- readOGR('vectors/dissolved_village_buffers.shp')
truthing<- readOGR('vectors/groundtruth_all_final.shp')
truth_df<-as.data.frame(truthing)



##### Clip to areas of interest #####
new_extent <- extent(647958,700224,-2767479,-2693251) #buffered extent +-1km
dummy <- crop(b1_2005_blue, new_extent)
b1_05_blue_clip <- crop(b1_2005_blue, new_extent)
b2_05_green_clip <- crop(b2_2005_green, new_extent)
b3_05_red_clip <- crop(b3_2005_red, new_extent)
b4_05_NIR_clip <- crop(b4_2005_NIR, new_extent)
b5_05_SWIR1_clip <-crop(b5_2005_SWIR1, new_extent)

b1_14_violet_clip <- crop(b1_2014_violet, new_extent)
b2_14_blue_clip <- crop(b2_2014_blue, new_extent)
b3_14_green_clip <- crop(b3_2014_green, new_extent)
b4_14_red_clip <- crop(b4_2014_red, new_extent)
b5_14_NIR_clip <- crop(b5_2014_NIR, new_extent)
b6_14_SWIR1_clip <- crop(b6_2014_SWIR1, new_extent)
#extents are now the same

#Export this points in this extent, to be manipulated in Qgis and brought back in
testpoints<-randomPoints(dummy, n=500)
testpoints<-as.data.frame(testpoints)
write.csv(testpoints, "groundtruth_points.csv") #export dataframe to csv
###NOW WE NEED TO GET THE TRUTHING DONE. QUITE A BIT OF WORK ! ###



##### Create Vegetation INdices #####
compareRaster(b1_05_blue_clip, b2_05_green_clip, b3_05_red_clip, b4_05_NIR_clip, b5_05_SWIR1_clip)
compareRaster(b1_14_violet_clip, b2_14_blue_clip, b3_14_green_clip, b4_14_red_clip, b5_14_NIR_clip, b6_14_SWIR1_clip)

### NDVI ###
ndvi_2005 <-(b4_05_NIR_clip - b3_05_red_clip) / (b4_05_NIR_clip + b3_05_red_clip)
ndvi_2014 <- (b5_14_NIR_clip - b4_14_red_clip) / (b5_14_NIR_clip + b4_14_red_clip)

### EVI ###
evi_2005 <- (2.5* (b4_05_NIR_clip - b3_05_red_clip)) / (b4_05_NIR_clip + (6*b3_05_red_clip) - (7.5*b1_05_blue_clip) + 1)
evi_2014 <- (2.5* (b5_14_NIR_clip - b4_14_red_clip)) / (b5_14_NIR_clip + (6*b4_14_red_clip) - (7.5*b2_14_blue_clip) + 1)

### MSAVI2 ###
msavi2_2005 <- ((2*b4_05_NIR_clip) + 1 - sqrt(((2*b4_05_NIR_clip) + 1)^2 - (8*(b4_05_NIR_clip - b3_05_red_clip)))) / 2
msavi2_2014 <- ((2*b5_14_NIR_clip) + 1 - sqrt(((2*b5_14_NIR_clip) + 1)^2 - (8*(b5_14_NIR_clip - b4_14_red_clip)))) / 2

### NDMI/NDWI depending on the sensor ###
ndmi_2005 <- (b4_05_NIR_clip - b5_05_SWIR1_clip)/(b4_05_NIR_clip + b5_05_SWIR1_clip)
ndmi_2014 <- (b5_14_NIR_clip - b6_14_SWIR1_clip) / (b5_14_NIR_clip + b6_14_SWIR1_clip)

##### stack the rasters ###### 
#I believe only one stack is needed for my lab here, since I am not having 
layers_extract_2005 <- stack(b1_05_blue_clip,b2_05_green_clip,b3_05_red_clip,b4_05_NIR_clip,b5_05_SWIR1_clip,
                             ndvi_2005,evi_2005,msavi2_2005,ndmi_2005)
layers_extract_2014<-stack(b1_14_violet_clip,b2_14_blue_clip,b3_14_green_clip,b4_14_red_clip,b5_14_NIR_clip,b6_14_SWIR1_clip,
                           ndvi_2014,evi_2014,msavi2_2014,ndmi_2014)




##### Separate  trainig and validation data #####
head(truth_df)
truth_df_sub <- truth_df[,c("ID","X2005","X2014","coords.x1","coords.x2")]
names(truth_df_sub)<- c('ID','2005','2014','Eastings','Northings')

#split into training/truth randomly- these datasets contain BOTH years in them
dummy_rows<-sample(nrow(truth_df_sub))
truth_df_shuffled <-truth_df_sub[dummy_rows,]
nrow(truth_df_shuffled)
566*0.8
566-453
# 453 and 113
validation_df<-truth_df_shuffled[1:113,]
training_df <- truth_df_shuffled[114:566,]



##### extract GIS data #####
head(training_df)
training_xy <- as.matrix(training_df[,c('Eastings','Northings')])#separate xy coordinates and the data
training_cov_2005 <-extract(layers_extract_2005, training_xy)
training_cov_2014 <-extract(layers_extract_2014, training_xy) #extract the data
training_2005_extracted <-data.frame(training_df, training_cov_2005)
training_2014_extracted <-data.frame(training_df, training_cov_2014)
names(training_2005_extracted) <- c('ID','class_2005','class_2014','Eastings','Northings','B1','B2','B3','B4','B5','NDVI','EVI','MSAVI2','NDMI')
names(training_2014_extracted)<- c('ID','class_2005','class_2014','Eastings','Northings','B1','B2','B3','B4','B5','B6','NDVI','EVI','MSAVI2','NDMI')
head(training_2014_extracted)
head(training_2005_extracted)

#drop the location information and ID
training_2005<- subset(training_2005_extracted, select = -c(ID,Eastings,Northings))
training_2014<-subset(training_2014_extracted, select = -c(ID, Eastings, Northings))

#drop training column that isn't necessary - 
training_2005<- training_2005[-c(2)]
training_2005<- training_2005[complete.cases(training_2005),]
training_2014<- training_2014[-c(1)]
training_2014<- training_2014[complete.cases(training_2014),]

#change remaining 6s to 2s - late decision to ease classification
training_2005[training_2005$class_2005==6,]['class_2005']<-2
#training_2014[training_2005$class_2014==6,]['class_2014']<-2 #ROW NOT NECESSARY, 2014 already done?

#Change names of the layers stack
names(layers_extract_2005)<-c('B1','B2','B3','B4','B5','NDVI','EVI','MSAVI2','NDMI')
names(layers_extract_2014)<-c('B1','B2','B3','B4','B5','B6','NDVI','EVI','MSAVI2','NDMI')

##### Random Forest #####
rf.2005 <- randomForest(as.factor(class_2005) ~ B1+B2+B3+B4+B5+NDVI+EVI+MSAVI2+NDMI, na.action = na.omit, ntree=500, data=training_2005)
varImpPlot(rf.2005)
summary(rf.2005)
rf.2005.map <- predict(layers_extract_2005, rf.2005, type="class",index=1, nan.rm=TRUE,inf.rm = TRUE) 

rf.2014 <- randomForest(as.factor(class_2014)~ B1+B2+B3+B4+B5+B6+NDVI+EVI+MSAVI2+NDMI, na.action = na.omit, ntree=500, data=training_2014)
varImpPlot(rf.2014)
rf.2014.map <- predict(layers_extract_2014, rf.2014, type="class",index=1, nan.rm=TRUE,inf.rm = TRUE) 


##### Write the rasters #####
outdir<-'C:/Users/srcha/Documents/Grad_school/research/usb_12-8-20/620_final_project/output'
file.path(outdir,"test.tif")

writeRaster(rf.2005.map, filename=file.path(outdir, "rf_2005_1.tif"), format="GTiff", overwrite=TRUE)
writeRaster(rf.2014.map, filename=file.path(outdir, 'rf_2014_1.tif'), format="GTiff", overwrite=TRUE)


##### WRite validation points to csv #####
#### So that you can do it somewhere else
write.csv(validation_df, file.path(outdir, "validation_points.csv"))
