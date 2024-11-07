# FIXING ISSUES WITH EVERGLADES FIRE HISTORY DATA 
  # M.Grace McLeod (2022)

# This script cleans and combines fire history shapefiles from Everglades National Park (EVER 1948-2020) and Big Cypress National Preserve (BICY 1978-2020)  
# Resource: https://r-spatial.github.io/sf/articles/sf1.html

##########################################################################################################################################################

rm(list=ls())

# Confirmed Libraries:
library(sf)
library(tidyverse)
library(dplyr)
library(gtools)
library(splitstackshape)

#library(spatial)
#library(raster)
#library(rgdal)
#library(maptools)
#library(cleangeo)

#library(readr)

#library(lubridate)
#library(anytime) 
#library(parsedate)





# Original shapefiles obtained from EVER are saved here:
    # "/Volumes/SERCDATA/Malone Lab/ENP Fire/Everglades Fire History/Spatial Layers/EVG FIRE PERIMETERS_original"
# Original shapefiles obtained from BICY are saved here:
    # "/Volumes/SERCDATA/Malone Lab/ENP Fire/Everglades Fire History/Spatial Layers/BICY FIRE PERIMETERS_original"

setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History")
#_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

####################################################################################################################################################################################################################################
# FIXING ISSUES WITH EVERGLADES NP DATA (EVER)
####################################################################################################################################################################################################################################

# Some years have different columns and other issues. These issues are resolved below
# 1948-2010 are formatted the same. We make years >= 2011  match this format before applying additional edits to all years
# Variables Harmonized:Fire_ID, Fire_Number, Fire_Name, DISC_DATE, DECLED_DATE, YEAR_, Fire_Type

  
dir <- "/Users/sm3466/Desktop/ENP_FireHistory/EVER_FIRE PERIMETERS_original"
 
# IMPORT ALL ENP SHAPEFILES (1948-2020) ................................................................................................................................
  
  setwd(dir)
  All <-dir(dir, "*.shp$")
  shp <- All[1:73]
  
  for (shp in shp){
    
    setwd(original)
    print(shp)
    sp <- read_sf(shp[1])  %>% st_transform(sp, "+proj=longlat +datum=WGS84 +no_defs") %>% st_make_valid
    
    # Extract the name of the file to re-use when saving:
    n <-shp 
    n.2 <- strsplit(n, ".", fixed=T) [[1]]
    print(n.2[1])
    
    # Save updates to "clean"
    writeOGR( sp, clean, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
}

# RESOLVING REMAINING DATE FORMAT ISSUES.....................................................................................................................
  # I think these issues were causing problems with date formatting when binding files
  # These edits are saved to the original location, overwriting faulty files. (if file are needed as they came out of GIS, use associated zip file)
 {
   
# 1955
  setwd(clean)
  shp <-dir(clean, "*.shp$")
  shp <- shp[8]
  print(shp)
  sp <- readOGR(shp) # import the shapefile
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Edit errored dates
  sp$DECLD_DATE[22] <- as.character('19550404')
  # Save updates 
  writeOGR( sp, clean, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")

# 1959
  setwd(clean)
  shp <-dir(clean, "*.shp$")
  shp <- shp[12]
  print(shp)
  sp <- readOGR(shp) # import the shapefile
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # EDIT ERRORED DATES
  sp$DECLD_DATE[23] <- as.character('19590215')
  # Save updates
  writeOGR( sp, clean, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")

# 1962
  setwd(clean)
  shp <-dir(clean, "*.shp$")
  shp <- shp[15]
  print(shp)
  sp <- readOGR(shp) # import the shapefile
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # EDIT ERRORED DATES
  sp$DECLD_DATE[38] <- as.character('19620310')
  # Save updates
  writeOGR( sp, clean, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")


#2017
  shp <- All[70] # point to just the year of interest
  sp <- readOGR(shp) # import the shapefile
  sp$YEAR_[31] <- "2017" 
  DF <- as.data.frame(sp)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # save updated file to folder
  writeOGR(sp, clean, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
  
}

# YEAR-SPECIFIC EDITS....................................................................................................................................................................

{
# 1948-2015
  setwd(clean)
  All <-dir(clean, "*.shp$")
  shp <- All[1:68]
  master <- data.frame()
  
  for (shp in shp){
    
    setwd(clean)
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # if dates are 0 make sure they are NA
    sp$DISC_DATE[ sp$DISC_DATE == 0]<- NA
    sp$DECLD_DATE[ sp$DECLD_DATE == 0]<- NA
    # change column names
    sp$gmID <- sp$FIRE_ID
    sp$gmNumber <-sp$FIRE_NUM
    sp$gmName <- sp$FIRE_NAME
    sp$gmYear <- sp$YEAR_
    sp$StartDate <- sp$DISC_DATE
    sp$EndDate <- sp$DECLD_DATE
    sp$gmType <- sp$FIRE_TYPE
    # make a dataframe 
    df <- as.data.frame(sp) # convert to df
    master <- smartbind(master, df)
    # Extract the name of the file to re-use when saving:
    n <-shp 
    n.2 <- strsplit(n, ".", fixed=T) [[1]]
    print(n.2[1])
    # save updates
    writeOGR(sp, GM, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
    
}

  # 2016-2017
  All <-dir(clean, "*.shp$")
  shp <- All[69:70]
  master <- data.frame()
  
  for (shp in shp){
    
    setwd(clean )
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # if dates are 0 make sure they are NA
    sp$DISC_DATE[ sp$DISC_DATE == 0]<- NA
    sp$DECLD_DATE[ sp$DECLD_DATE == 0]<- NA
    # change column names
    sp$gmID <- sp$FIRE_ID
    sp$gmNumber <-sp$FIRE_NUM
    sp$gmName <- sp$FIRE_NAME
    sp$gmYear <- sp$YEAR_
    sp$StartDate <- sp$DISC_DATE
    sp$EndDate <- sp$DECLD_DATE
    sp$gmType <- sp$FIRE_TYPE
    # make a dataframe 
    df <- as.data.frame(sp) # convert to df
    master <- smartbind(master, df)
    # Extract the name of the file to re-use when saving:
    n <-shp 
    n.2 <- strsplit(n, ".", fixed=T) [[1]]
    print(n.2[1])
    # save updates
    writeOGR(sp, GM, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
    
  }
  

# 2018
    All <-dir(clean, "*.shp$")
    shp <- All[71]
    
    for (shp in shp){
    
    setwd(clean )
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # (2018 only) If DECLD_DATE is Na, replace with controlDat 
    sp$FireOutDat[18] <- "2018/03/24"
    sp$FireOutDat[42] <- "2018/07/09"
    # There was a fire that started in 2017 and ended in 2018. Change year to 2018
    sp$YEAR[2] <- "2018"
    # if dates are 0 make sure they are NA
    sp$DISC_DATE[ sp$DISC_DATE == 0]<- NA
    sp$FireOutDat[ sp$FireOutDat == 0]<- NA
    # change column names
    sp$gmID <- sp$UniqueFire
    sp$gmNumber <-sp$LocalIncid
    sp$gmName <- sp$FIRE_NAME
    sp$gmYear <- sp$YEAR
    sp$StartDate <- sp$DISC_DATE
    sp$EndDate <- sp$FireOutDat
    sp$gmType <- sp$FIRE_TYPE
    # Extract the name of the file to re-use when saving:
    n <-shp 
    n.2 <- strsplit(n, ".", fixed=T) [[1]]
    print(n.2[1])
    # save updates
    df <- as.data.frame(sp)
    writeOGR(sp, GM, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
    }
  
# 2019
    All <-dir(clean, "*.shp$")
    shp <- All[72]
    master <- data.frame()
    
    for (shp in shp){
    
    setwd(clean )
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # if dates are 0 make sure they are NA
    sp$DISC_DATE[ sp$DISC_DATE == 0]<- NA
    sp$FireOutDat[ sp$FireOutDat == 0]<- NA
    # change column names
    sp$gmID <- sp$UniqueFire
    sp$gmNumber <-sp$LocalIncid
    sp$gmName <- sp$FIRE_NAME
    sp$gmYear <- sp$YEAR
    sp$StartDate <- sp$DISC_DATE
    sp$EndDate <- sp$FireOutDat
    sp$gmType <- sp$FIRE_TYPE
    # make a dataframe 
    df <- as.data.frame(sp) # convert to df
    master <- smartbind(master, df)
    # Extract the name of the file to re-use when saving:
    n <-shp 
    n.2 <- strsplit(n, ".", fixed=T) [[1]]
    print(n.2[1])
    # save updates
    writeOGR(sp, GM, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
}


# 2020
  All <-dir(clean, "*.shp$")
  shp <- All[73]
  master <- data.frame()
  
  for (shp in shp){
    
    setwd(clean )
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # Assign / fix fire types
    try(sp$FIRE_TYPE[sp$Incident_t=="WF"] <- "11", silent=T)
    try(sp$FIRE_TYPE[sp$Incident_t=="FU"] <- "11", silent=T)
    try(sp$FIRE_TYPE[sp$Incident_t =="RX" & sp$fire_cause== "Management"] <- "48", silent=T)
    try(sp$FIRE_TYPE[sp$Incident_t =="RX" & sp$fire_cause != "Management"] <- "11", silent=T)
    # if dates are 0 make sure they are NA
    sp$DISC_DATE[ sp$DISC_DATE == 0]<- NA
    sp$DECLD_DATE[ sp$DECLD_DATE == 0]<- NA
    # rename columns
    sp$gmID <- sp$FIRE_ID
    sp$gmNumber <-sp$FIRE_NUM
    sp$gmName <- sp$FIRE_NAME
    sp$gmYear <- sp$CY_YEAR
    sp$StartDate <- sp$DISC_DATE
    sp$EndDate <- sp$DECLD_DATE
    sp$gmType <- sp$FIRE_TYPE
    # make a dataframe 
    df <- as.data.frame(sp) # convert to df
    master <- smartbind(master, df)
    # Extract the name of the file to re-use when saving:
    n <-shp 
    n.2 <- strsplit(n, ".", fixed=T) [[1]]
    print(n.2[1])
    # save updates
    writeOGR(sp, GM, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
    
}

}


# EDITS APPLIED TO ALL FILES...................................................................................................................................................
# trim data down to just columns of interest
{  
# Run on all files 
setwd(GM)
All <-dir(GM, "*.shp$")
shp <- All[1:73]
master <- data.frame()

for (shp in shp){
  
  setwd(GM)
  print(shp)
  sp <- readOGR(shp) # import the shapefile
  # select only columns of interest
  sp <- sp[c( "gmID", "gmNumber", "gmName", "gmYear", "StartDate", "EndDate", "gmType" )]
  # Parse date to make all formats match 
  sp$StartDate <- parse_date(sp$StartDate )
  sp$EndDate <- parse_date(sp$EndDate ) 
  sp$StartDate <- as.Date(sp$StartDate, format= "%Y%m%d")
  sp$EndDate <- as.Date(sp$EndDate, format= "%Y%m%d")
  sp$gmYear <- format(sp$StartDate, "%Y")
  # rename columns to match BICY
  sp$FIRE_ID <- sp$gmID; sp$gmID = NULL
  sp$FireNumber <- sp$gmNumber; sp$gmNumber = NULL
  sp$FireName <- sp$gmName; sp$gmName = NULL
  sp$Year <- sp$gmYear; sp$gmYear = NULL
  sp$FireType <- sp$gmType; sp$gmType = NULL
  # re- order columns
  sp <- sp[, c(3,4,5,1,2,6,7)]
  # Assign WF or RX for FireType 
  sp$Type <- "WF"
  sp$Type[sp$FireType == 48] <- "Rx"
  # overwrite FireType with Type for column name consistancy
  sp$FireType <- sp$Type ; sp$Type <- NULL
  # make a dataframe 
  df <- as.data.frame(sp) # convert to df
  master <- smartbind(master, df)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save updates 
  writeOGR(sp, GM, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
}

# FIXING END DATE BEING ASSIGNED TO 2022
# The end dates of two fires that did not originally have end dates are being filled with the day of processing.
  # Overwrite to set as NA (year is ok)
# 1996 fire
  setwd(GM)
  All <-dir(GM, "*.shp$")
  shp <- All[49]
  sp <- readOGR(shp)
  sp$EndDate [65] <- "1996-12-19"
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save updates 
  writeOGR(sp, GM, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
#2017 fire
  setwd(GM)
  All <-dir(GM, "*.shp$")
  shp <- All[70]
  sp <- readOGR(shp)
  sp$EndDate [37] <- "2017-08-25"
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save updates 
  writeOGR(sp, GM, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")


}



####################################################################################################################################################################################################################################
# FORMATTING BIG CYPRESS FIRE DATA (BICY)
####################################################################################################################################################################################################################################

# IMPORT SHAPEFILE............................................................................................................
# unlike EVER, BICY fires are all stored in a single shapefile, rather than by year. 

# Import BICY fire history (1978-2021)
setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/BICY_FIRE_Perimeter_original")
# Make a list of all of the shapefiles in that folder
shps <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/BICY_FIRE_Perimeter_original", "*.shp$")
# Brings in the shpefile
BICYfires <- readOGR(shps[1])
# Assign CRS
BICYfires <- spTransform(BICYfires, crs("+proj=longlat +datum=WGS84 +no_defs"))
# Convert to dataframe to view data
BICYfires.df <- as.data.frame(BICYfires)


# FORMAT COLUMNS TO MATCH EVERGLADES..........................................................................................
# Create new fire type column and assign wild or RX  
# seems like Some of the 0s in FIRE_TYPE are incorrect (ex: Management fire should be 48 not 0)
unique(BICYfires$FireType)
# Wildfires
try(BICYfires$gmType[BICYfires$FireType == "Wildland"  ]  <- "WF", silent=T)
try(BICYfires$gmType[BICYfires$FireType == "Other" ]  <- "WF", silent=T)
try(BICYfires$gmType[BICYfires$FireType =="Unknown"  ]  <- "WF", silent=T)
try(BICYfires$gmType[BICYfires$FireType == "Vehicle Fire" ]  <- "WF", silent=T)
try(BICYfires$gmType[BICYfires$FireType =="Fire Use" ]  <- "WF", silent=T)
try(BICYfires$gmType[BICYfires$FireType =="Chemical" ]  <- "WF", silent=T)
# RX fires
try(BICYfires$gmType[BICYfires$FireType =="Prescribed"] <- "RX", silent=T)
try(BICYfires$gmType[BICYfires$FireType == "Burn Area"]  <- "RX", silent=T)
try(BICYfires$gmType[BICYfires$FireType == "Mechanical"] <- "RX", silent=T)

# Select columns of interest
BICYfires <- BICYfires[c("FIRE_ID","FireNumber","FireName","CalendYear", "StartDate","EndDate", "gmType")]
# rename desired columns
BICYfires$Year <- BICYfires$CalendYear; BICYfires$CalendYear <- NULL
BICYfires$FireType <- BICYfires$gmType;  BICYfires$gmType <- NULL
BICYfires.df <- as.data.frame(BICYfires)

# Fix date errors
BICYfires$EndDate[19] <- "2020/07/16"
BICYfires$EndDate[20] <- "2020/07/14"

# Clean the geometry
#some polygons are messy (overlapping lines, slivers, etc) this cleans them up
BICYfires <- clgeo_Clean(BICYfires, errors.only = NULL, strategy = "POLYGONATION", verbose = FALSE)

# Save cleaned shapefile
writeOGR(BICYfires, "/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/BICY_FIRE_Perimeter_GM", layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")

# save/laod BICY fire info for quick viewing
setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History")
save(BICYfires, BICYfires.df, file= "BICY_Fires_Clean_1978_2021.RDATA")



####################################################################################################################################################################################################################################
# COMBINING REGIONS (EVER and BICY) FOR ALL EVERGLADES FIRES (EVG) 
####################################################################################################################################################################################################################################

# load BICY fires if not already in directory
    load("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/BICY_Fires_Clean_1978_2021.RDATA")

# Clip EVER fire dataset down to same timeframe as BICY.............................................................................................................
    setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVER_FIRE_Perimeter")
    All <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVER_FIRE_Perimeter", "*.shp$")
    shp <- All[31:73] # (1978-2020)
    master <- data.frame()
    
    for (shp in shp){
      
      setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVER_FIRE_Perimeter")
      print(shp)
      sp <- readOGR(shp) # import the shapefile
      # make a dataframe 
      df <- as.data.frame(sp) # convert to df
      master <- smartbind(master, df)
      # Extract the name of the file to re-use when saving:
      n <-shp 
      n.2 <- strsplit(n, ".", fixed=T) [[1]]
      print(n.2[1])
      # Save 1978-2020 dataset
      writeOGR(sp, "/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVER_fires_1978_2020", layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
    }
      # convert to dataframe
      EVERfires <- as.data.frame(master)

# Combinging regions..........................................................................................................................................
  # make a value for the working directory to pull ENP fires from
  EVER.f <- "/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVER_fires_1978_2020"
  # make a value for the working directory where combined files will be saved to
  out <- "/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020"
  
  # Make a loop
  # for each unique year in the BICY shapefile
  for (i in unique(BICYfires[["Year"]])){
    print(i)
    
    # 1: subset BICY by year i
    BICY <- BICYfires[which(BICYfires$Year== i), ] 
    # 2: bring in EVER for year i 
    setwd(EVER.f)
    EVER.list <- list.files(path=EVER.f, pattern=i ) # make a list of all years in ENP by looking for i in the name
    EVER <- readOGR (EVER.list [3]) # there are multiple files per year. Only want the shapefile, which is 3rd in the list
    # 3: Merge
    sp <- rbind(EVER, BICY)
    df <- as.data.frame(sp)
    # 4: Write shapefile
    setwd(out)
    paste("EVG_", i) # adds NP_ to the file name
    writeOGR(sp, dsn=out, layer=paste("EVG_", i), driver= "ESRI Shapefile", overwrite_layer = TRUE) # save EVG shapefiles
    
  }



# LOAD AND VIEW ALL EVG FIRES..................................................................................................................................
  
setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
All <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020", "*.shp$")
shp <- All[1:43]
master <- data.frame()

for (shp in shp){
  
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
  print(shp)
  sp <- readOGR(shp) # import the shapefile
  # make a dataframe 
  df <- as.data.frame(sp) # convert to df
  master <- smartbind(master, df)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
}

  # change master dataframe name for saving
  EVGfires <- as.data.frame(master)
  # save all dataframes for easy future viewing
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History")
  save(BICYfires.df, EVERfires, EVGfires, file="FireDataframes.RDATA")

  
####################################################################################################################################################################################################################################
# MO DATA MO PROBLMES 
####################################################################################################################################################################################################################################
  
# still having issues with date format when rasterizing....here we go again. 
  # work on EVGfires dataframe and then go back and reapply to all shps.
  EVGsp <- "/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020" 
  
# START DATES................................................................................................................
  
# File-specific edits
  # 1982
  setwd(EVGsp)
  All <-dir(EVGsp, "*.shp$")
  shp <- All[4]
  sp <- readOGR(shp)
  sp$StartDate [113] <- "1981/03/29"
  # make dataframe
  df <- as.data.frame(sp)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save updates 
  writeOGR(sp, EVGsp, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
  # 1998
  # ...looks fine but I'm retyping it just in case...
  setwd(EVGsp)
  All <-dir(EVGsp, "*.shp$")
  shp <- All[21]
  sp <- readOGR(shp)
  class(sp$StartDate[132])
  # date not recognizing 29th of Febuary! Maybe...change to March 1st
  sp$StartDate [132] <- "1998-03-01"
  # make dataframe
  df <- as.data.frame(sp)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save updates 
  writeOGR(sp, EVGsp, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
  #2014
  # missing day. Setting to last day of the month
  setwd(EVGsp)
  All <-dir(EVGsp, "*.shp$")
  shp <- All[37]
  sp <- readOGR(shp)
  sp$StartDate [78] <- "2014/06/30"
  sp$StartDate [72] <- "2014/05/31"
  # make dataframe
  df <- as.data.frame(sp)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save updates 
  writeOGR(sp, EVGsp, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
  
# Now run date formatting on all files  
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
  All <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020", "*.shp$")
  shp <- All[1:43]
  master <- data.frame()
  
  for (shp in shp){
    
    setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # format dates
    sp$StartDate <- as_date(sp$StartDate, format("%Y-%m-%d"))
    # make a dataframe 
    df <- as.data.frame(sp) # convert to df
    master <- smartbind(master, df)
    # Extract the name of the file to re-use when saving:
    n <-shp 
    n.2 <- strsplit(n, ".", fixed=T) [[1]]
    print(n.2[1])
    # Save updates 
    writeOGR(sp, EVGsp, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  }
  
  
# END DATES................................................................................................................
  
  #1984
  # missing day. Setting to last day of the month
  setwd(EVGsp)
  All <-dir(EVGsp, "*.shp$")
  shp <- All[7]
  sp <- readOGR(shp)
  sp$EndDate [190] <- "1984/02/06"
  # make dataframe
  df <- as.data.frame(sp)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save updates 
  writeOGR(sp, EVGsp, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
  #2020
  # missing day. Setting to last day of the month
  setwd(EVGsp)
  All <-dir(EVGsp, "*.shp$")
  shp <- All[43]
  sp <- readOGR(shp)
  sp$EndDate [44] <- "2020/07/16"
  sp$EndDate [45] <- "2020/07/14"
  sp$EndDate [46] <- "2020/07/16"
  # make dataframe
  df <- as.data.frame(sp)
  # Extract the name of the file to re-use when saving:
  n <-shp 
  n.2 <- strsplit(n, ".", fixed=T) [[1]]
  print(n.2[1])
  # Save updates 
  writeOGR(sp, EVGsp, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
  
# Try formatting all files
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
  All <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020", "*.shp$")
  shp <- All[1:43]
  master <- data.frame()
  
  for (shp in shp){
    
    setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # format dates
    sp$EndDate <- as_date(sp$EndDate, format("%Y-%m-%d"))
    # make a dataframe 
    df <- as.data.frame(sp) # convert to df
    master <- smartbind(master, df)
    # Extract the name of the file to re-use when saving:
    n <-shp 
    n.2 <- strsplit(n, ".", fixed=T) [[1]]
    print(n.2[1])
    # Save updates 
    writeOGR(sp, EVGsp, layer =  n.2[1], overwrite_layer=TRUE, driver= "ESRI Shapefile")
    
  }

  
# compare number of NAs between EVGfires and master
  sum(is.na(master$EndDate))
  sum(is.na(EVGfires$EndDate)) 
    # getting 6 extra NAs but I don't now where from...
    # honestly the likelyhood of me even using that fire's data is probably pretty slim so I'm not going to worry about it now
  

# MERGE ALL EVG SHPS INTO ONE 
  # make a template to bind to using the first shp
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
  All <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020", "*.shp$")
  shp <- All[1]
  sp <- readOGR(shp) 
  EVG.master.sp <- sp
  # run loop on other shps
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
  All <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020", "*.shp$")
  shp <- All[2:43]
  
  
for (shp in shp){
    
    setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # bind togethr
    EVG.master.sp <- rbind(EVG.master.sp, sp)
    # Save
    FH_layers <- "/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/FH_layers"
    writeOGR(EVG.master.sp, FH_layers, layer =  "EVG.master", overwrite_layer=TRUE, driver= "ESRI Shapefile")
    
  }

  # reload master shapefile 
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/FH_layers")
  EVG.master.sp <- rgdal::readOGR(dsn = ".", layer = "EVG.master")
  # transform crs
  EVG.master.sp <- spTransform(EVG.master.sp, crs("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")) 
  # make dataframe
  EVG.master.df <- as.data.frame(EVG.master.sp)
  # save .RDATA for easy access
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History")
  save(EVG.master.sp, EVG.master.df, file ="EVG.master.RDATA")
    
    
    
    
####################################################################################################################################################################################################################################
# RECOVERY PERIOD FIRES
####################################################################################################################################################################################################################################
  
# MERGE ALL EVG SHPS INTO ONE ....jsut for recovery period
  # make a template to bind to using the first shp
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
  All <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020", "*.shp$")
  shp <- All[23]
  sp <- readOGR(shp) 
  RECOV.master.sp <- sp
  # run loop on other shps
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
  All <-dir("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020", "*.shp$")
  shp <- All[24:33]
  
  
  for (shp in shp){
    
    setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/EVG_AllFires_1978-2020")
    print(shp)
    sp <- readOGR(shp) # import the shapefile
    # bind togethr
    RECOV.master.sp <- rbind(RECOV.master.sp, sp)
    # Save
    FH_layers <- "/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/FH_layers"
    writeOGR(RECOV.master.sp, FH_layers, layer =  "RECOV.master", overwrite_layer=TRUE, driver= "ESRI Shapefile")
    
  }
  
  # reload master shapefile 
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History/FH_layers")
  RECOV.master.sp <- rgdal::readOGR(dsn = ".", layer = "RECOV.master")
  # transform crs
  RECOV.master.sp <- spTransform(RECOV.master.sp, crs("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")) 
  # make dataframe
  RECOV.master.df <- as.data.frame(RECOV.master.sp)
  # save .RDATA for easy access
  setwd("/Volumes/inwedata/Malone Lab/ENP Fire/Grace_McLeod/Fire_History")
  save(RECOV.master.sp, RECOV.master.df, file ="RECOV.master.RDATA")
  