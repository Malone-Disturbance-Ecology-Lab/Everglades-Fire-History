# Function for summing up 0/1 fire rasters from a raster stack
# raster_stack: SpatRaster 
# start_year: (numeric) the starting year
# num_years: (numeric) number of years from the starting year (specifies the endpoint for the time interval)
total_fires <- function(raster_stack, start_year, num_years){
  
  # Error out if the stack of rasters is not a SpatRaster
  if (class(raster_stack) != "SpatRaster"){
    stop("The stack of rasters needs to be in SpatRaster format")
  }
  
  # Error out if the specified start year or time interval is not numeric
  if (!is.numeric(start_year) | !is.numeric(num_years)) {
    stop("Start year or time interval is not numeric")
  }
  
  # Assemble the name of the layer for the specified starting year
  start_layer_name <- paste0("burned_", start_year)
  
  # Find the numeric index of the layer that has the starting year
  start_index <- which(names(raster_stack) == start_layer_name)
  
  # Subset the raster stack to the specified time interval
  subset_raster_stack <- terra::subset(raster_stack, start_index:(start_index+num_years))
  
  # Sum the layers in the subset
  # If na.rm = FALSE then only the overlapping areas will show up
  added_rasters <- sum(subset_raster_stack, na.rm=TRUE)
  
  return(added_rasters)
}
