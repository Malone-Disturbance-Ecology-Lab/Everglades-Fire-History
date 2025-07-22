# This script was developed to validate the 30 meter fire history layer.
rm(list=ls())

librarian::shelf(terra, tidyterra, tidyverse, sf, ggpubr, dplyr, ggplot2)

# Random points: ####

# Point to FireHistory folder on Malone Lab server
firehist_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "FireHistory") 

Uplands_raster <- terra::rast(file.path("Uplands_raster.tif"))

# Read in harmonized fire perimeter data:
EVER_BICY_1978_2023_perim <- terra::vect(file.path(firehist_folder, "EVER_BICY_1978_2023_perim.shp")) %>%
  # Transform to same CRS as uplands raster
  terra::project(terra::crs(Uplands_raster)) %>% st_as_sf() %>% st_make_valid()

# Create a burned area polygon to generate random points:
EVER_BICY_1978_2023_burned_Area <- st_union( EVER_BICY_1978_2023_perim)

sample.points <- EVER_BICY_1978_2023_burned_Area  %>% st_sample(size = 2500, type = "random") %>%  st_as_sf()

# Extract information from 30 meter layers ####

firehist_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "FireHistory") 

total_fires_1978_2023 <- terra::rast(file.path(firehist_folder, "EVER_BICY_1978_2023_total_fires.tif"))

names(total_fires_1978_2023) <- 'total_fires_1978_2023'


predicted <- terra::extract(total_fires_1978_2023, vect(sample.points))[, 2]

sample.points$predicted <- predicted %>% as.numeric

#sample.points$predicted[ is.na(sample.points$predicted)] <- 0

# Extract information from the vector files

# Find intersections
intersections <- st_intersects(sample.points, EVER_BICY_1978_2023_perim)

# Count points per polygon
point_counts <- lengths(intersections )

# Add counts to polygon data (optional)
sample.points$observed = point_counts

hist( sample.points$observed, n=100)
hist( sample.points$predicted, n=100)

lm( sample.points$observed ~ sample.points$predicted) %>% summary

png("FIGURES/Validation.png", width = 900, height=600, res=300)
sample.points %>% ggplot(aes( x=observed, y = predicted)) + 
  geom_point(aes( x=observed, y = predicted), col= 'grey') + 
  geom_smooth(method="lm", col="black") + theme_bw() + ylab("Predicted (30m Raster)") + xlab("Observed (Vector Data)") + ggpubr::stat_regline_equation() +
  ggpubr::stat_cor(method = "pearson", label.x = 1, label.y = 10)

dev.off()

sample.points$observed %>% summary
sample.points$predicted %>% summary

mean(sample.points$observed - sample.points$predicted, na.rm=T)
