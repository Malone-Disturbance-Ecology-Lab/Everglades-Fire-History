## --------------------------------------------- ##
#   Everglades Fire Perimeter Harmonization
## --------------------------------------------- ##
# Script author(s): Angel Chen

# Purpose:
## This script cleans and combines fire history shapefiles from Everglades National Park (EVER 1948-2020) and Big Cypress National Preserve (BICY 1978-2020)

## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##

# Load necessary libraries
# If you don't have the "librarian" package, uncomment the next line and run it to install the package
# install.packages("librarian")
librarian::shelf(sf, tidyverse, gtools, splitstackshape)

## ----------------------------------------------- ##
# FIXING ISSUES WITH EVERGLADES NP DATA (EVER) ----
## ----------------------------------------------- ##

# Some years have different columns and other issues. These issues are resolved below
# 1948-2010 are formatted the same.
# We make years >= 2011  match this format before applying additional edits to all years
# Variables Harmonized:Fire_ID, Fire_Number, Fire_Name, DISC_DATE, DECLED_DATE, YEAR_, Fire_Type

# IMPORT ALL ENP SHAPEFILES (1948-2020) -------------

folder_EVER <- "EVER_FIRE PERIMETERS_original"

# Identify all EVER data files
( raw_files_EVER <- dir(path = folder_EVER, "*.shp$") ) 

# Specify the files we don't want
not_wanted_EVER <- c("EVER_Fires_2021.shp")

# Remove the unwanted files from our vector
raw_files_EVER <- raw_files_EVER[! raw_files_EVER %in% not_wanted_EVER]

# Create empty list to populate with shapefiles later
shp_list_EVER <- list()

# For each shapefile...
for (i in 1:length(raw_files_EVER)){
  
  # Grab its name
  raw_file_name <- raw_files_EVER[i]
  
  message(paste0("Reading ", raw_file_name))
  
  # If the file name is "EVER_FIRES_2020.shp"...
  if (raw_file_name == "EVER_FIRES_2020.shp"){
    # Read in shapefile
    shp <- read_sf(file.path(folder_EVER, raw_files_EVER[i])) %>% 
      # Drop Z dimension
      st_zm(shp, drop=T, what = "ZM") %>%
      # Transform to WGS84
      st_transform("EPSG:4326") %>% 
      # Make valid
      st_make_valid()
    
  } else {
  # Read in shapefile
  shp <- read_sf(file.path(folder_EVER, raw_files_EVER[i])) %>% 
    # Transform to WGS84
    st_transform("EPSG:4326") %>% 
    # Make valid
    st_make_valid()
  }
  
  # Add to list
  shp_list_EVER[[raw_file_name]] <- shp
}
