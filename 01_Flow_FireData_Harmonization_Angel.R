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

# IMPORT ALL EVER SHAPEFILES (1948-2020) -------------

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

# COMBINING ALL EVER SHAPEFILES ----------------------

years_2018_2020 <- c("EVER_FIRES_2018.shp", "EVER_FIRES_2019.shp", "EVER_FIRES_2020.shp")

years_1948_2017 <- setdiff(raw_files_EVER, years_2018_2020)

# For each shapefile...
for (i in 1:length(shp_list_EVER)){
  # Grab its name
  file_name <- names(shp_list_EVER[i])
  
  # If the file is from years 1948 through 2017...
  if (file_name %in% years_1948_2017){
    shp_list_EVER[[file_name]] <- shp_list_EVER[[file_name]] %>%
      # Select relevant columns
      select(FIRE_ID, FIRE_NUM, FIRE_NAME, YEAR_, DISC_DATE, DECLD_DATE, FIRE_TYPE) %>%
      # Standardize column names
      rename(Fire_ID = FIRE_ID) %>%
      rename(Fire_Number = FIRE_NUM) %>%
      rename(Fire_Name = FIRE_NAME) %>%
      rename(Year = YEAR_) %>%
      rename(Disc_Date = DISC_DATE) %>%
      rename(Decld_Date = DECLD_DATE) %>%
      rename(Fire_Type = FIRE_TYPE)  %>%
      # Create a new column to denote the shapefile where the data came from
      mutate(File_Name = file_name, .before = Fire_ID) %>%
      # Make all columns into character columns
      mutate(dplyr::across(.cols = -c(geometry), .fns = as.character))
  }
  
  # If the file is from 2018 or 2019...
  if (file_name == "EVER_FIRES_2018.shp" | file_name == "EVER_FIRES_2019.shp"){
    shp_list_EVER[[file_name]] <- shp_list_EVER[[file_name]] %>%
      # Select relevant columns
      select(UniqueFire, LocalIncid, FIRE_NAME, YEAR, DISC_DATE, FireOutDat, FIRE_TYPE) %>%
      # Standardize column names
      rename(Fire_ID = UniqueFire) %>%
      rename(Fire_Number = LocalIncid) %>%
      rename(Fire_Name = FIRE_NAME) %>%
      rename(Year = YEAR) %>%
      rename(Disc_Date = DISC_DATE) %>%
      rename(Decld_Date = FireOutDat) %>%
      rename(Fire_Type = FIRE_TYPE) %>%
      # Create a new column to denote the shapefile where the data came from
      mutate(File_Name = file_name, .before = Fire_ID) %>%
      # Make all columns into character columns
      mutate(dplyr::across(.cols = -c(geometry), .fns = as.character))
  }
  
  # If the file is from 2020...
  if (file_name == "EVER_FIRES_2020.shp") {
    shp_list_EVER[[file_name]] <- shp_list_EVER[[file_name]] %>%
      # Select relevant columns
      select(FIRE_ID, FIRE_NUM, FIRE_NAME, CY_YEAR, DISC_DATE, DECLD_DATE, Incident_t, fire_cause) %>%
      # Standardize column names
      rename(Fire_ID = FIRE_ID) %>%
      rename(Fire_Number = FIRE_NUM) %>%
      rename(Fire_Name = FIRE_NAME) %>%
      rename(Year = CY_YEAR) %>%
      rename(Disc_Date = DISC_DATE) %>%
      rename(Decld_Date = DECLD_DATE) %>%
      # Make a new Fire_Type column according to these conditions:
      mutate(Fire_Type = case_when(
        # When Incident_t is "WF" or "FU", set the value to "11
        Incident_t == "WF" | Incident_t == "FU" ~ "11",
        # When Incident_t is "RX" and fire_cause is "Management", set the value to "48"
        Incident_t == "RX" & fire_cause == "Management" ~ "48",
        # When Incident_t is "RX" and fire_cause is not "Management", set the value to "11"
        Incident_t == "RX" & fire_cause != "Management" ~ "11",
      )) %>%
      # Drop Incident_t and fire_cause columns
      select(-Incident_t, -fire_cause) %>%
      # Create a new column to denote the shapefile where the data came from
      mutate(File_Name = file_name, .before = Fire_ID) %>%
      # Make all columns into character columns
      mutate(dplyr::across(.cols = -c(geometry), .fns = as.character))
  }
}

# Unlist the list we generated from above
tidy_v0_EVER <- shp_list_EVER %>%
  purrr::list_rbind(x = .)

# RESOLVING REMAINING DATE FORMAT ISSUE -------------

tidy_v1_EVER <- tidy_v0_EVER %>%
  mutate(Decld_Date = case_when(
    # Fix wrong dates in Decld_Date column
    File_Name == "EVER_FIRES_1955.shp" & stringr::str_detect(Decld_Date, "16550404") ~ "19550404",
    File_Name == "EVER_FIRES_1959.shp" & stringr::str_detect(Decld_Date, "21568") ~ "19590215",
    File_Name == "EVER_FIRES_1962.shp" & stringr::str_detect(Decld_Date, "16920310") ~ "19620310",
    File_Name == "EVER_FIRES_1996.shp" & stringr::str_detect(Decld_Date, "19962119") ~ "19961219",
    File_Name == "EVER_FIRES_2017.shp" & stringr::str_detect(Decld_Date, "20170852") ~ "20170825",
    File_Name == "EVER_FIRES_2018.shp" & Fire_ID == "2018-FLEVP-18039" ~ "2018/03/24",
    File_Name == "EVER_FIRES_2018.shp" & Fire_ID == "2018-FLEVP-18063" ~ "2018/07/09",
    # If Decld_Date is 0, set to NA
    Decld_Date == "0" ~ NA,
    T ~ Decld_Date
  )) %>%
  mutate(Year = case_when(
    # Fix wrong years in the Year column
    File_Name == "EVER_FIRES_2017.shp" & stringr::str_detect(Year, "0") ~ "2017",
    File_Name == "EVER_FIRES_2018.shp" & stringr::str_detect(Year, "2017") ~ "2018",
    T ~ Year
  )) %>%
  mutate(Disc_Date = case_when(
    # If Disc_Date is 0, set to NA
    Disc_Date == "0" ~ NA,
    T ~ Disc_Date
  ))

