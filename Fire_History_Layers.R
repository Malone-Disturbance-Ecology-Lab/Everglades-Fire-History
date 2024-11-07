# FIRE HISTORY LAYERS
# M. Grace McLeod (2023)


# Preprocessing:
# Fire perimeter shapefiles for Everglades National Park (EVER 1948-2020) and Big Cypress National Preserve (BICY 1978-2020) were obtained from National Park Service personnel.
# shapefiles were cleaned in the Fire_Data_Edits script and saved individually in EVG_AllFires_1978_2020.


# This script: 
# 1. Rasterizes yearly fire history shapefile data 
# 2. Extracts raster data to upland sample pts to generate fire history dataframes

##########################################################################################################################################################


library(sf)
library(rgdal)
library(sp)
library(dplyr)
library(tidyverse)
library(raster)
library(ggplot2)
library(readr)
library(fasterize)
library(gtools)
library(tools)
library(RColorBrewer)
library(matrixStats)
library(mapview)
library(terra)

rm(list=ls())
setwd("\\\\corellia.environment.yale.edu/MaloneLab/Research/ENP/ENP Fire/Grace_McLeod")

# Import grid for rasterizing (Upland Raster)
Uplands_raster <- raster("./Upland_veg/Uplands_raster.tif")
# Make a path to pull in combined EVG shapefiles
FireSHP <- "./Fire_History/EVG_AllFires_1978-2020"
# Make a path for annual fire occurance rasters 
FreqRAS <- "./Fire_History/Yearly_Frequency"
# Make a path for annual fire year rasters
YearRAS <- "./Fire_History/Fire_Year"



##########################################################################################################################################################
# 1. CREATING YEARLY FIRE RASTERS
##########################################################################################################################################################
#creates annual fire rasters for multiple variables using the EVG fire history shapefile data (EVG_AllFires_1978-2020)

# BURNED/UNBURNED YEARLY RASTERS (1978-2020).................................................................................................
# used to calculate fire frequency 

# Tell R to use combined EVG shapefiles
setwd(FireSHP)
All <-dir(FireSHP, "*.shp$")
shp <- All[1:43]

# Create a loop to make annual burned/unburned rasters
for (shp in shp){
  
  # Import shapefile
  setwd(FireSHP)
  print(shp)
  sp <- readOGR(shp) 
  # Transform crs to match upland vegetation (grid)
  crs(Uplands_raster)
  sp <- spTransform(sp, crs("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")) 
  # Create column to represent the the pixels occupied by that fire have "burned" and set it equal to 1
  sp$Burned <- 1
  # Rasterize to Upland vegetation raster 
  sp <- st_as_sf(sp) # must be simple feature (sf) for fasterize
  Freq<- fasterize(sp, Uplands_raster, field= "Burned", fun="max", background= NA)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save rasters
  setwd(FreqRAS)
  writeRaster(Freq, filename=n.2[1], format="GTiff", overwrite=TRUE)
  
}


# YEAR OF FIRE OCCURANCE RASTERS (1978-2020).................................................................................................

# Tell R to use combined EVG shapefiles
setwd(FireSHP)
All <-dir(FireSHP, "*.shp$")
shp <- All[1:43]
# Create a loop to make annual rasters for year of fire
for (shp in shp){
  
  # Import shapefile
  setwd(FireSHP)
  print(shp)
  sp <- readOGR(shp) 
  # Transform crs to match upland vegetation (grid)
  crs(Uplands_raster)
  sp <- spTransform(sp, crs("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")) 
  # Rasterize to Upland vegetation raster 
  sp <- st_as_sf(sp) # must be simple feature (sf) for fasterize
  sp$Year <- as.numeric(sp$Year)
  FireYear<- fasterize(sp, Uplands_raster, field= "Year", fun="max", background= NA)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save rasters
  setwd(YearRAS)
  writeRaster(FireYear, filename=n.2[1], format="GTiff", overwrite=TRUE)
  
}

##########################################################################################################################################################
# 2. FIRE HISTORY DATAFRAME FOR UPLAND SAMPLE POINTS   
##########################################################################################################################################################

# load upland sample pts
Sample_pts_upland <- rgdal::readOGR(dsn="./Sampling", layer="Sample_pts_upland")

# FIRE FREQUENCY (for total fires) ............................................................................................................
# stack fire frequency rasters
FreqRAS <- "./Fire_History/Yearly_Frequency"
setwd(FreqRAS) # point to yearly rasters
all <- list_files_with_exts(FreqRAS, "tif") # identify .tifs
files <- all[1:43] # select year range
fires <- stack(files, RAT= TRUE, quick= TRUE) # Stack rasters
# check layer names
names(fires)
# extract to sample pts
FireHistory <- raster::extract(fires, Sample_pts_upland, method= "simple", buffer=NULL, df=TRUE, sp=TRUE, factors=TRUE)
FireHistory_df <- as.data.frame(FireHistory)
# calculate total fires 
FireHistory_df <- unfactor(FireHistory_df) # make numeric
FireHistory_df[3:45] <- sapply(FireHistory_df[3:45],as.numeric)
FireHistory_df[is.na(FireHistory_df)] <- 0 # turn NAs to 0
# total fire history
FireHistory_df$freq_1978_2020 <- rowSums(FireHistory_df[ , c(3:45)])
summary(FireHistory_df$freq_1978_2020)
names(FireHistory_df)
# recovery window 
# leave 2000 so there is room to calculate severity/see pre-fire spectral values
FireHistory_df$freq_2001_2007 <- rowSums(FireHistory_df[ , c(26:32)])
summary(FireHistory_df$freq_2001_2007)
# BL sample period
FireHistory_df$freq_2010_2020 <- rowSums(FireHistory_df[ , c(35:45)])
summary(FireHistory_df$freq_2010_2020)
# save
setwd("./Fire_History")
write_csv(FireHistory_df, "FireHistory_df.csv")
save(FireHistory_df, file="FireHistory_df.RDATA")


# FIRE YEAR (for time since fire)............................................................................................................
# stack fire year rasters
YearRAS <- "./Fire_History/Fire_Year"
setwd(YearRAS) # point to yearly rasters
all <- list_files_with_exts(YearRAS, "tif") # identify .tifs
files <- all[1:43] # select year range
years <- stack(files, RAT= TRUE, quick= TRUE) # Stack rasters
# check layer names
names(years)
# extract to sample pts
FireYears <- raster::extract(years, Sample_pts_upland, method= "simple", buffer=NULL, df=TRUE, sp=TRUE, factors=TRUE)
FireYears_df <- as.data.frame(FireYears)
summary(FireYears_df)
# save
setwd("./Fire_History")
write_csv(FireYears_df, "FireYears_df.csv")
save(FireYears_df, file="FireYears_df.RDATA")













