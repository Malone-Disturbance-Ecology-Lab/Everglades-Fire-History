## --------------------------------------------- ##
#              Fire History Layers
## --------------------------------------------- ##
# Script author(s): Angel Chen

# Purpose:
## This script: 
## 1. Rasterizes yearly fire history shapefile data 
## 2. Extracts raster data to upland sample pts to generate fire history dataframes

## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##

# Load necessary libraries
# If you don't have the "librarian" package, uncomment the next line and run it to install the package
# install.packages("librarian")
librarian::shelf(sf, raster, fasterize, tidyverse)

# Create necessary sub-folder(s)
dir.create(path = file.path("03_tidy_fire_history"), showWarnings = F)

## ----------------------------------------------- ##
#       Creating Yearly Fire Rasters -----
## ----------------------------------------------- ##

# creates annual fire rasters for multiple variables using the EVG fire history shapefile data

# Creating burned/unburned yearly rasters (1948-2021) -------------
# (Used to calculate fire frequency)

# Read in uplands raster
Uplands_raster <- raster::raster(file.path("02_tidy_uplands_raster", "Uplands_raster.tif"))

# Read in harmonized fire perimeter data
EVER_BICY_1978_2021_perim <- sf::read_sf(file.path("01_tidy_perimeters", "EVER_BICY_1978_2021_perim.shp")) %>%
  # Transform to same CRS as uplands raster
  sf::st_transform(raster::crs(Uplands_raster)) %>%
  # Create column to represent the the pixels occupied by that fire have "burned" and set it equal to 1
  dplyr::mutate(Burned = 1)





# Creating year of fire occurrence rasters (1978-2020) -------------






