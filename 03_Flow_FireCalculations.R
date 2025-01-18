## --------------------------------------------- ##
#              Fire Calculations
## --------------------------------------------- ##
# Script author(s): Angel Chen

# Purpose:
## This script calculates these statistics for the following files created in the step 1 and 2 scripts:
## 
## Raster Files:
## 
## - Total files, with the option of calculating decadal total fires. - Using the O/1 stack
## - Time Since Fire: Current year - Maximum(Fire Year.Stack)
## - Mean annual TSF.
## - Annual total fires.
##
## Perimeter files:
##  
## 1. Add area to files and perimeter length
## 2. get the total annual area burned, Total Rx area burned, total WF area burned
## 3. Mean annual fire size
## 4. Mean Annual Area/perimeter

## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##

# Load necessary libraries
# If you don't have the "librarian" package, uncomment the next line and run it to install the package
# install.packages("librarian")
librarian::shelf(terra, tidyterra, sf, tidyverse)

# Point to FireHistory folder on Malone Lab server
firehist_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "FireHistory") 

# Read in tidy annual burned/unburned rasters
burned_rasters <- terra::rast(file.path(firehist_folder, "EVER_BICY_1978_2023_burned.tif"))

# Read in tidy year of fire occurrence rasters
year_rasters <- terra::rast(file.path(firehist_folder, "EVER_BICY_1978_2023_year_occurrence.tif"))

# Read in fire perimeters shapefile
perim_shape <- sf::st_read(file.path(firehist_folder, "EVER_BICY_1978_2023_perim.shp"))

# Load custom functions
source("R/my_functions.R")

## ----------------------------------------------- ##
#         Fire Raster Calculations -----
## ----------------------------------------------- ##

# TEMPORARY FIX FOR REORDERING RASTER LAYERS
# I'll make sure to export the rasters in the correct chronological order next time
burned_rasters_reordered <- terra::subset(burned_rasters, order(names(burned_rasters)))
# Check that it's correct
names(burned_rasters_reordered)


# Calculating total fires (1978-2023) ------------------------------------------------

# Testing
# x <- total_fires(raster_stack = burned_rasters_reordered, start_year = 2010, end_year = 2011)
# terra::plot(x)
# 
# y <- total_fires(raster_stack = burned_rasters_reordered, start_year = 2014, end_year = 2017)
# terra::plot(y, ext = c(400000, 600000, 2770000, 2940000))
# terra::plot(y, ext = c(485000, 500000, 2870000, 2885000),
#             main = "Total fires: how many times an area got burned from 2014 to 2017")
# 
# z <- total_fires(raster_stack = burned_rasters_reordered, start_year = 2012, end_year = 2022)
# terra::plot(z, ext = c(485000, 520000, 2855000, 2885000),
#             main = "Total fires: how many times an area got burned from 2012 to 2022")

# Find the total fires from 1978 to 2023
total_fires_1978_2023 <- total_fires(raster_stack = burned_rasters_reordered, start_year = 1978, end_year = 2023)
# Check plot
terra::plot(total_fires_1978_2023, ext = c(460000, 540000, 2815000, 2900000),
            main = "Total fires: how many times an area got burned from 1978 to 2023")

# Export tidy total fires raster
terra::writeRaster(total_fires_1978_2023, file.path(firehist_folder, "EVER_BICY_1978_2023_total_fires.tif"),
                   overwrite = T)

# Calculating time since last fire ------------------------------------------------

# TEMPORARY FIX FOR REORDERING RASTER LAYERS
# I'll make sure to export the rasters in the correct chronological order next time
year_rasters_reordered <- terra::subset(year_rasters, order(names(year_rasters)))
# Check that it's correct
names(year_rasters_reordered)

# Get current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))
# Find the time since last fire
time_since <- current_year - max(year_rasters_reordered, na.rm = TRUE)
# Check plot
terra::plot(time_since, ext = c(460000, 540000, 2815000, 2900000),
            main = "Time since last fire: how many years since an area got burned")

# Export tidy time since raster
terra::writeRaster(time_since, file.path(firehist_folder, "EVER_BICY_1978_2023_time_since.tif"),
                   overwrite = T)

## ----------------------------------------------- ##
#     Fire Perimeter Shapefile Calculations -----
## ----------------------------------------------- ##

# Calculate annual total area burned, total WF area burned, total RX area burned, 
# mean fire size, and mean area/perimeter
summary_calculations <- fire_summary(fire_shapefile = perim_shape)

# Check
dplyr::glimpse(summary_calculations)
