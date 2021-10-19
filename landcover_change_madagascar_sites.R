#################################################
###########      Madagascar    ##################
###########  Landcover Change  ##################
###########      Analysis      ##################
###########    Stephen Chang   ##################
#################################################

##### Dear Passengers,
##### The first script of my research 
##### Landsat Manipulation
##### and pseudo-coding for things I will do in QGIS.

###### Load Packages ######
library(raster)           #for raster covariate data; version 2.6-7 used
library(reshape2)         #for re-formatting data; version 1.4.3 used
library(mgcv)             #for gams; version 1.8-24 used
library(dismo)            #for SDMs; version 1.1-4 used
library(randomForest)     #for random forest SDMs; version 4.6-14 used
#library(maxnet)           #maxent with maxnet; version 0.1.2 used
#library(glmnet)           #needed for maxnet; version 2.0-16 used
library(MuMIn)            #for model selection; version 1.42.1 used
library(PresenceAbsence)  #for model evaluation; version 1.1.9 used
library(ecospat)         #for model evaluation; version 3.0 used
#library(ggplot2)
#library(viridis)         #Color Palettes
library(data.table)
library(rgdal)
library(dplyr)
library(splancs)

#rm(list = ls())          #I think this is a clearing line


#####################################
##### Functions for this script   ###
#####################################
#seems to be empty

#####################################
##### Create landsat directories  ###
#####################################
####Parent
setwd("C:/Users/srcha/Documents/Grad_school/research/landsat")
dir <- 'C:/Users/srcha/Documents/Grad_school/research/landsat'

### create sub-directories by site 
dir_AA <- '/AA'
dir_AS <- '/AS'
dir_MM <- '/MM'

## create sub-directories by year
dir_1990 <-'/1990'
dir_1995 <- '/1995'
dir_2000 <- '/2000'


#####################################
##### Read in Landsat 5 TM data  ###
#####################################

##### Ambondrona_Andonakaomby #####
AA_dir<-paste(dir,dir_AA, sep = "")
AA_folders<-list.dirs(AA_dir)
AA_folders<-AA_folders[grepl(pattern = "*LT05",AA_folders)]
AA_scenes<-vector()

for (folder in AA_folders){
  if (grepl(pattern='1990',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #assign(paste('AA_',date,'_vec',sep = ""),vector()) #assign AS_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('AA_',date,'_vec',sep=""))
      assign(paste('AA_',date,'_vec',sep=""), emptyvec)
      AA_scenes<-append(AA_scenes, paste('AA_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
  else if(grepl(pattern = '1995',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #assign(paste('AA_',date,'_vec',sep = ""),vector()) #assign AS_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('AA_',date,'_vec',sep=""))
      assign(paste('AA_',date,'_vec',sep=""), emptyvec)
      AA_scenes<-append(AA_scenes, paste('AA_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
  else if(grepl(pattern = '2000',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #assign(paste('AS_',date,'_vec',sep = ""),vector()) #assign AS_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('AA_',date,'_vec',sep=""))
      assign(paste('AA_',date,'_vec',sep=""), emptyvec)
      AA_scenes<-append(AA_scenes, paste('AA_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
}
print(AA_scenes)



##### Anosivola_Sahamalaza #####
AS_dir<-paste(dir,dir_AS, sep = "")
AS_folders<-list.dirs(AS_dir)
AS_folders<-AS_folders[grepl(pattern = "*LT05",AS_folders)]
AS_scenes<-vector()

for (folder in AS_folders){
  if (grepl(pattern='1990',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #assign(paste('AS_',date,'_vec',sep = ""),vector()) #assign AS_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('AS_',date,'_vec',sep=""))
      assign(paste('AS_',date,'_vec',sep=""), emptyvec)
      AS_scenes<-append(AS_scenes, paste('AS_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
  else if(grepl(pattern = '1995',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #assign(paste('AS_',date,'_vec',sep = ""),vector()) #assign AS_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('AS_',date,'_vec',sep=""))
      assign(paste('AS_',date,'_vec',sep=""), emptyvec)
      AS_scenes<-append(AS_scenes, paste('AS_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
  else if(grepl(pattern = '2000',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #assign(paste('AS_',date,'_vec',sep = ""),vector()) #assign AS_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('AS_',date,'_vec',sep=""))
      assign(paste('AS_',date,'_vec',sep=""), emptyvec)
      AS_scenes<-append(AS_scenes, paste('AS_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
}
print(AS_scenes)

#####Marohotro_Montifeno#####
MM_dir<-paste(dir,dir_MM, sep = "")
MM_folders<-list.dirs(MM_dir)
MM_folders<-MM_folders[grepl(pattern = "*LT05",MM_folders)]
MM_scenes<-vector()

for (folder in MM_folders){
  if (grepl(pattern='1990',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #MMsign(paste('MM_',date,'_vec',sep = ""),vector()) #assign MM_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('MM_',date,'_vec',sep=""))
      assign(paste('MM_',date,'_vec',sep=""), emptyvec)
      MM_scenes<-append(MM_scenes, paste('MM_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
  else if(grepl(pattern = '1995',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #assign(paste('MM_',date,'_vec',sep = ""),vector()) #assign MM_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('MM_',date,'_vec',sep=""))
      assign(paste('MM_',date,'_vec',sep=""), emptyvec)
      MM_scenes<-append(MM_scenes, paste('MM_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
  else if(grepl(pattern = '2000',folder)){
    for (file in folder){
      scene<-list.files(folder,pattern = "*B1|B2|B3|B4|B5|B6|B7", full.names = TRUE, recursive = FALSE) #pattern separates out Bands from other squabble
      date<-scene[1] 
      date<-substr(date,80,87 )#variable here that extracts the exact date from the image, this should work unless naming conventions and folder lengths change
      #print(date)
      emptyvec<-vector()
      #assign(paste('MM_',date,'_vec',sep = ""),vector()) #assign MM_YYYYMMDD_vec as an empty vector to add rasters to
      for (band in scene){
        band_read<-raster(band)#read in band as raster and assign to variable
        emptyvec<-append(emptyvec,band_read)#append band_read to 
      }
      print(paste('MM_',date,'_vec',sep=""))
      assign(paste('MM_',date,'_vec',sep=""), emptyvec)
      MM_scenes<-append(MM_scenes, paste('MM_',date,'_vec',sep=""))
      print(paste('assigned above variable to list of rasters'))
    }
  }
}
print(MM_scenes)








#####################################
####### Read in extra data set    ###
#####################################
#really only elevation
elev<-raster('C:/Users/srcha/Documents/Grad_school/research/elevation/srtm_madagascar.TIF')



#####################################
#### Verify & conform projections ###
#####################################
#To be completed in Qgis
#AA EPSG = 32639
#AS EPSG = 32639
#MM EPSG = 32638      These are all projected to UTM North Zones, which is fine


#####################################
#### Crop Scenes to study areas  ###
#####################################

#### National Parks and buffers ####
MA_10km<-readOGR("C:/Users/srcha/Documents/Grad_school/research/vectors/nat_parks/MA_10km.shp")
AN_10km<-readOGR("C:/Users/srcha/Documents/Grad_school/research/vectors/nat_parks/Andohahela_10km.shp")
ZA_10km<-readOGR("C:/Users/srcha/Documents/Grad_school/research/vectors/nat_parks/Zahamena_10km.shp")


#### Crop elevation ####
#AA_extent <-extent(SOME BUFFER HERE)
#AS_extent <-extent(SOME BUFFER HERE)
#MM_extent <-exxtent(SOME BUFFER HERE)
#elev_AA <- crop(elev,AA_extent)
#elev_AS <- crop(elev, AS_extent)
#elev_MM <- crop(elev, MM_extent)
#CHECK FOR MADA NAT PARKS FILE



#### Crop AA ####
# IM CURRENTLY MISSING MONTAGNE DAMBRE SHAPEFILE UNTIL I GET LOGGED INTO THE LAB SHARED DRIVE

#### Crop AS ####

#### Crop MM ####



#####################################
#### Digitize Built-up areas     ###
#####################################
#Done in either QGIS or Google Earth Pro, or both


#####################################
############## Water?? #############
#####################################
#Inspect scenes visually, lakes/large rivers could be digitized out. 


#####################################
#### Calculate spectral indices ###
#####################################
#evi, NDMI, NBR


#####################################
############ Training ##############
#####################################
#QGIS and Google Earth Pro


#####################################
####### Image classification #######
#####################################


#####################################
####### Change quantification ######
#####################################
#it would be nice to learn how to do this in R. I can easily 
#do it in Q or ArcGIS though.

