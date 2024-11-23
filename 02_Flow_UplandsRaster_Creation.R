## --------------------------------------------- ##
#            Uplands Raster Creation
## --------------------------------------------- ##
# Script author(s): Angel Chen

# Purpose:
## This script uses Landsat images downloaded from Earth Explorer to generate the 30m raster grid used throughout this project
## Then by using that raster grid, it also generates the upland vegetation layer for Everglades National Park and Big Cypress National Preserve

## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##

# Load necessary libraries
# If you don't have the "librarian" package, uncomment the next line and run it to install the package
# install.packages("librarian")
librarian::shelf(terra, tidyverse)

# Create necessary sub-folder(s)
dir.create(path = file.path("02_tidy_uplands_raster"), showWarnings = F)

## ----------------------------------------------- ##
#          Importing Landsat Images -----
## ----------------------------------------------- ##

# Point to folder containing Landsat images
landsat_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "Grace_McLeod", "AOI", "Landsat_ref") 

# Import Landsat images (.tif)
r1 <- terra::rast(file.path(landsat_folder, "LC08_L1TP_015042_20200307_20200822_02_T1_refl.tif"))
r2 <- terra::rast(file.path(landsat_folder, "LC08_L1TP_015043_20200307_20200822_02_T1_refl.tif"))
r3 <- terra::rast(file.path(landsat_folder, "LC08_L1TP_016042_20200314_20200822_02_T1_refl.tif"))

# Check
terra::plot(r1)

## ----------------------------------------------- ##
#           Merging Landsat Images -----
## ----------------------------------------------- ##

# Merge images together using mosaic() (takes a while)
# This will be our reference raster
Raster30x30 <- terra::mosaic(r1, r2, r3, fun = "mean")

# Check
terra::plot(Raster30x30)

## ----------------------------------------------- ##
#     Importing Upland Ecosystems Shapefile -----
## ----------------------------------------------- ##

# Point to folder containing vegetation layers
veg_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "Grace_McLeod", "Veg_layers")

# Import upland ecosystems shapefile
UpEco <- terra::vect(file.path(veg_folder, "Uplands_poly.shp")) %>%
  # Transform to same CRS as reference raster
  terra::project(terra::crs(Raster30x30)) 

# Check
terra::crs(Raster30x30)
terra::crs(UpEco)
terra::plot(UpEco)

## ----------------------------------------------- ##
#     Rasterizing to Create Uplands Raster -----
## ----------------------------------------------- ##

# Rasterize upland ecosystems shapefile with Landsat reference raster
Uplands_raster <- terra::rasterize(x = UpEco, y = Raster30x30, field = "value", fun = "sum", background = NA)

# Check
terra::plot(Uplands_raster)

# Export tidy uplands raster
terra::writeRaster(Uplands_raster, file.path("02_tidy_uplands_raster", "Uplands_raster.tif"))
