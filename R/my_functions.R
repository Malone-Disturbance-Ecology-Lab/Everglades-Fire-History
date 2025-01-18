# Function for summing up 0/1 fire rasters from a raster stack
# raster_stack: SpatRaster 
# start_year: (numeric) the starting year
# end_year: (numeric) the ending year
total_fires <- function(raster_stack, start_year, end_year){
  
  # Error out if the stack of rasters is not a SpatRaster
  if (methods::is(object = raster_stack, class2 = "SpatRaster") == FALSE){
    stop("The stack of rasters needs to be in SpatRaster format")
  }
  
  # Error out if the specified start or end year is not numeric
  if (!is.numeric(start_year) | !is.numeric(end_year)) {
    stop("Start or end year is not numeric")
  }
  
  # Assemble the name of the layer for the specified starting year
  start_layer_name <- paste0("burned_", start_year)
  
  # Find the numeric index of the layer that has the starting year
  start_index <- which(names(raster_stack) == start_layer_name)
  
  # Assemble the name of the layer for the specified ending year
  end_layer_name <- paste0("burned_", end_year)
  
  # Find the numeric index of the layer that has the ending year
  end_index <- which(names(raster_stack) == end_layer_name)
  
  # Subset the raster stack to the specified time interval
  subset_raster_stack <- terra::subset(raster_stack, start_index:end_index)
  
  # Sum the layers in the subset
  # If na.rm = FALSE then only the overlapping areas will show up
  added_rasters <- sum(subset_raster_stack, na.rm=TRUE)
  
  return(added_rasters)
}

# Function for taking a shapefile of fire perimeters and calculating its 
# annual total area burned, total WF area burned, total RX area burned, mean fire size, and mean area/perimeter
# fire_shapefile: sf shapefile 
fire_summary <- function(fire_shapefile){
  
  # Error out if the shapefile is not a sf dataframe
  if(methods::is(object = fire_shapefile, class2 = "sf") == FALSE |
     methods::is(object = fire_shapefile, class2 = "data.frame") == FALSE){
    stop("Shapefile needs to be a sf dataframe")
  }
  
  # Turn off spherical geometry (s2) to avoid st_area() and st_perimeter() erroring out
  # Spherical geometry stops some sf functions from working on overlapping geometries
  # See https://github.com/r-spatial/sf/issues/1762
  sf::sf_use_s2(FALSE)
  
  tidy_v1 <- fire_shapefile %>%
    # Make the geometries valid 
    sf::st_make_valid() %>%
    # Calculate area and perimeter length
    dplyr::mutate(area_m2 = sf::st_area(geometry),
                  perim_m = sf::st_perimeter(geometry))
  
  # Turn spherical geometry back on
  sf::sf_use_s2(TRUE)
  
  tidy_v2 <- tidy_v1 %>%
    # Convert to data frame so we can drop sticky geometry column
    as.data.frame() %>%
    # Drop geometry column
    dplyr::select(-geometry) %>%
    # Group by year to make calculations by year...
    dplyr::group_by(Year) %>%
    # Find total area burned by year
    dplyr::mutate(total_area_burned = sum(area_m2)) %>%
    # Find mean fire size by year
    dplyr::mutate(mean_fire_size = mean(area_m2, na.rm = T)) %>%
    # Divide area over perimeter
    dplyr::mutate(area_over_perim = area_m2/perim_m) %>%
    # Find mean area/perimeter by year
    dplyr::mutate(mean_area_over_perim = mean(area_over_perim, na.rm = T)) %>%
    # Group by year and fire type to make calculations by year & fire type...
    dplyr::group_by(Year, Fir_Typ) %>%
    # Find total area burned by fire type and year
    dplyr::mutate(total_by_fire_type = sum(area_m2)) %>%
    # Ungroup
    dplyr::ungroup()
  
  tidy_v3 <- tidy_v2 %>%
    # Group by year, fire type, and the previous calculations because we will need these in the summary
    dplyr::group_by(Year, Fir_Typ, total_area_burned, mean_fire_size, mean_area_over_perim) %>%
    # Get a single value for the area burned by each fire type and year combo
    dplyr::summarize(actual_total_by_fire_type = max(total_by_fire_type)) %>%
    # Drop rows where fire type is NA
    dplyr::filter(!is.na(Fir_Typ)) %>%
    # Pivot wider to create WF and RX columns
    tidyr::pivot_wider(names_from = Fir_Typ, values_from = actual_total_by_fire_type) %>%
    # Rename columns
    dplyr::rename(total_WF_burned = WF,
                  total_RX_burned = RX) %>%
    # Reorder columns
    dplyr::select(Year, total_area_burned, total_WF_burned, total_RX_burned, mean_fire_size, mean_area_over_perim)
  
  return(tidy_v3)
  
}


