## --------------------------------------------- ##
#              Fire History Layers
## --------------------------------------------- ##
# Script author(s): Angel Chen

# Purpose:
## This script rasterizes yearly fire history shapefile data 

## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##

# Load necessary libraries
# If you don't have the "librarian" package, uncomment the next line and run it to install the package
# install.packages("librarian")
librarian::shelf(tidyterra, tidyverse)

## ----------------------------------------------- ##
#       Creating Yearly Fire Rasters -----
## ----------------------------------------------- ##

# creates annual fire rasters for multiple variables using the EVG fire history shapefile data

# Read in uplands raster
Uplands_raster <- terra::rast(file.path("Uplands_raster.tif"))

# Point to FireHistory folder on Malone Lab server
firehist_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "FireHistory") 

# Read in harmonized fire perimeter data
EVER_BICY_1978_2021_perim <- terra::vect(file.path(firehist_folder, "EVER_BICY_1978_2021_perim.shp")) %>%
  # Transform to same CRS as uplands raster
  terra::project(terra::crs(Uplands_raster)) 

# Creating burned/unburned yearly rasters (1978-2021) -------------
# (Used to calculate fire frequency)

burned_EVER_BICY_1978_2021_perim <- EVER_BICY_1978_2021_perim %>%
  # Create column to represent the the pixels occupied by that fire have "burned" and set it equal to 1
  dplyr::mutate(Burned = 1) 

# Create empty list to populate with rasters
raster_list <- list()

# Create a loop to make annual burned/unburned rasters
for (a_year in unique(burned_EVER_BICY_1978_2021_perim$Year)){
  
  message(paste0("Rasterizing for year ", a_year))
  
  # Subsetting to one year
  year_subset <- burned_EVER_BICY_1978_2021_perim %>%
    dplyr::filter(Year == a_year)
  
  # Rasterize to Upland vegetation raster 
  Freq <- terra::rasterize(x = year_subset, y = Uplands_raster, field = "Burned", fun = "max", background = NA)
  
  # Add to list
  raster_list[[paste0("burned_", a_year)]] <- Freq
  
}

# Stack rasters
burned_rasters <- terra::rast(raster_list)

# Export tidy burned rasters
terra::writeRaster(burned_rasters, file.path(firehist_folder, "EVER_BICY_1978_2021_burned.tif"),
                   overwrite = T)

# Creating year of fire occurrence rasters (1978-2021) -------------

# Create empty list to populate with rasters
raster_list2 <- list()

# Create a loop to make annual rasters for year of fire
for (a_year in unique(EVER_BICY_1978_2021_perim$Year)){
  
  message(paste0("Rasterizing for year ", a_year))
  
  # Subsetting to one year
  year_subset <- EVER_BICY_1978_2021_perim %>%
    dplyr::filter(Year == a_year)
  
  # Rasterize to Upland vegetation raster 
  FireYear <- terra::rasterize(x = year_subset, y = Uplands_raster, field = "Year", fun = "max", background = NA)
  
  # Add to list
  raster_list2[[paste0("year_", a_year)]] <- FireYear
  
}

# Stack rasters
year_rasters <- terra::rast(raster_list2)

# Export tidy year of fire occurrence rasters
terra::writeRaster(year_rasters, file.path(firehist_folder, "EVER_BICY_1978_2021_year_occurrence.tif"),
                   overwrite = T)
