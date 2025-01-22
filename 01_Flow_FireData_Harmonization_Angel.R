## --------------------------------------------- ##
#   Everglades Fire Perimeter Harmonization
## --------------------------------------------- ##
# Script author(s): Angel Chen

# Purpose:
## This script cleans and combines fire history shapefiles from Everglades National Park (EVER 1948-2023) and Big Cypress National Preserve (BICY 1978-2023)

## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##

# Load necessary libraries
# If you don't have the "librarian" package, uncomment the next line and run it to install the package
# install.packages("librarian")
librarian::shelf(sf, tidyverse)

## ----------------------------------------------- ##
# Fixing Issues with Everglades NP Data (EVER) ----
## ----------------------------------------------- ##

# Some years have different columns and other issues. These issues are resolved below
# 1948-2010 are formatted the same.
# We make years >= 2011  match this format before applying additional edits to all years
# Variables Harmonized: File_Name, Fire_ID, Fire_Number, Fire_Name, Year, Disc_Date, Decld_Date, Fire_Type

# Import all EVER shapefiles (1948-2023) -------------

folder_EVER <- "EVER_FIRE PERIMETERS_original"

# Identify all EVER data files
( raw_files_EVER <- dir(path = folder_EVER, "*.shp$") ) 

# Specify the files we don't want
not_wanted_EVER <- c("")

# Remove the unwanted files from our vector
raw_files_EVER <- raw_files_EVER[! raw_files_EVER %in% not_wanted_EVER]

# Create empty list to populate with shapefiles later
shp_list_EVER <- list()

# For each shapefile...
for (i in 1:length(raw_files_EVER)){
  
  # Grab its name
  raw_file_name <- raw_files_EVER[i]
  
  message(paste0("Reading ", raw_file_name))
  
  # If the file name is "EVER_FIRES_2020.shp", "EVER_Fires_2021.shp", "EVER_FIRES_2022_exp012025.shp", or "EVER_FIRES_2023.shp"...
  if (raw_file_name %in% c("EVER_FIRES_2020.shp", "EVER_Fires_2021.shp", "EVER_FIRES_2022_exp012025.shp", "EVER_FIRES_2023.shp")){
    # Read in shapefile
    shp <- sf::read_sf(file.path(folder_EVER, raw_files_EVER[i])) %>% 
      # Drop Z dimension
      sf::st_zm(shp, drop=T, what = "ZM") %>%
      # Transform to WGS84
      sf::st_transform("EPSG:4326") %>% 
      # Make valid
      sf::st_make_valid()
    
  } else {
  # Read in shapefile
  shp <- sf::read_sf(file.path(folder_EVER, raw_files_EVER[i])) %>% 
    # Transform to WGS84
    sf::st_transform("EPSG:4326") %>% 
    # Make valid
    sf::st_make_valid()
  }
  
  # Add to list
  shp_list_EVER[[raw_file_name]] <- shp
}

# Combining all EVER shapefiles ----------------------

years_2018_2023 <- c("EVER_FIRES_2018.shp", "EVER_FIRES_2019.shp", "EVER_FIRES_2020.shp", "EVER_Fires_2021.shp", "EVER_FIRES_2022_exp012025.shp", "EVER_FIRES_2023.shp")

years_1948_2017 <- setdiff(raw_files_EVER, years_2018_2023)

# For each shapefile...
for (i in 1:length(shp_list_EVER)){
  # Grab its name
  file_name <- names(shp_list_EVER[i])
  
  # If the file is from years 1948 through 2017...
  if (file_name %in% years_1948_2017){
    shp_list_EVER[[file_name]] <- shp_list_EVER[[file_name]] %>%
      # Select relevant columns
      dplyr::select(FIRE_ID, FIRE_NUM, FIRE_NAME, YEAR_, DISC_DATE, DECLD_DATE, FIRE_TYPE) %>%
      # Standardize column names
      dplyr::rename(Fire_ID = FIRE_ID) %>%
      dplyr::rename(Fire_Number = FIRE_NUM) %>%
      dplyr::rename(Fire_Name = FIRE_NAME) %>%
      dplyr::rename(Year = YEAR_) %>%
      dplyr::rename(Disc_Date = DISC_DATE) %>%
      dplyr::rename(Decld_Date = DECLD_DATE) %>%
      dplyr::rename(Fire_Type = FIRE_TYPE)  %>%
      # Create a new column to denote the shapefile where the data came from
      dplyr::mutate(File_Name = file_name, .before = Fire_ID) %>%
      # Make all columns into character columns
      dplyr::mutate(dplyr::across(.cols = -c(geometry), .fns = as.character))
  }
  
  # If the file is from 2018 or 2019...
  if (file_name == "EVER_FIRES_2018.shp" | file_name == "EVER_FIRES_2019.shp"){
    shp_list_EVER[[file_name]] <- shp_list_EVER[[file_name]] %>%
      # Select relevant columns
      dplyr::select(UniqueFire, LocalIncid, FIRE_NAME, YEAR, DISC_DATE, FireOutDat, FIRE_TYPE) %>%
      # Standardize column names
      dplyr::rename(Fire_ID = UniqueFire) %>%
      dplyr::rename(Fire_Number = LocalIncid) %>%
      dplyr::rename(Fire_Name = FIRE_NAME) %>%
      dplyr::rename(Year = YEAR) %>%
      dplyr::rename(Disc_Date = DISC_DATE) %>%
      dplyr::rename(Decld_Date = FireOutDat) %>%
      dplyr::rename(Fire_Type = FIRE_TYPE) %>%
      # Create a new column to denote the shapefile where the data came from
      dplyr::mutate(File_Name = file_name, .before = Fire_ID) %>%
      # Make all columns into character columns
      dplyr::mutate(dplyr::across(.cols = -c(geometry), .fns = as.character))
  }
  
  # If the file is from 2020, 2021, 2022, or 2023...
  if (file_name %in% c("EVER_FIRES_2020.shp", "EVER_Fires_2021.shp", "EVER_FIRES_2022_exp012025.shp", "EVER_FIRES_2023.shp")) {
    shp_list_EVER[[file_name]] <- shp_list_EVER[[file_name]] %>%
      # Select relevant columns
      dplyr::select(FIRE_ID, FIRE_NUM, FIRE_NAME, CY_YEAR, DISC_DATE, DECLD_DATE, Incident_t, fire_cause) %>%
      # Standardize column names
      dplyr::rename(Fire_ID = FIRE_ID) %>%
      dplyr::rename(Fire_Number = FIRE_NUM) %>%
      dplyr::rename(Fire_Name = FIRE_NAME) %>%
      dplyr::rename(Year = CY_YEAR) %>%
      dplyr::rename(Disc_Date = DISC_DATE) %>%
      dplyr::rename(Decld_Date = DECLD_DATE) %>%
      # Make a new Fire_Type column according to these conditions:
      dplyr::mutate(Fire_Type = dplyr::case_when(
        # When Incident_t is "WF" or "FU", set the value to "11"
        Incident_t == "WF" | Incident_t == "FU" ~ "11",
        # When Incident_t is "WF - State", set the value to "11",
        Incident_t == "WF - State" ~ "11",
        # When Incident_t is "RX" and fire_cause is "Management", set the value to "48"
        Incident_t == "RX" & fire_cause == "Management" ~ "48",
        # When Incident_t is "RX" and fire_cause is not "Management", set the value to "11"
        Incident_t == "RX" & fire_cause != "Management" ~ "11",
      )) %>%
      # Drop Incident_t and fire_cause columns
      dplyr::select(-Incident_t, -fire_cause) %>%
      # Create a new column to denote the shapefile where the data came from
      dplyr::mutate(File_Name = file_name, .before = Fire_ID) %>%
      # Make all columns into character columns
      dplyr::mutate(dplyr::across(.cols = -c(geometry), .fns = as.character))
  }
}

# Unlist the list we generated from above
tidy_v0_EVER <- shp_list_EVER %>%
  purrr::list_rbind(x = .)

# Resolving remaining date format issue -------------

tidy_v1_EVER <- tidy_v0_EVER %>%
  dplyr::mutate(Decld_Date = dplyr::case_when(
    # Fix wrong dates in Decld_Date column
    # For example, in the 1955 shapefile, change the Decld_Date from "16550404" to "19550404"
    File_Name == "EVER_FIRES_1955.shp" & Decld_Date == "16550404" ~ "19550404",
    File_Name == "EVER_FIRES_1959.shp" & Decld_Date == "21568" ~ "19590215",
    File_Name == "EVER_FIRES_1962.shp" & Decld_Date == "16920310" ~ "19620310",
    File_Name == "EVER_FIRES_1973.shp" & Decld_Date == "197301280" ~ "19730128",
    File_Name == "EVER_FIRES_1996.shp" & Decld_Date == "19962119" ~ "19961219",
    File_Name == "EVER_FIRES_2017.shp" & Decld_Date == "20170852" ~ "20170825",
    File_Name == "EVER_FIRES_2017.shp" & Decld_Date == "32820170" ~ "3282017",
    File_Name == "EVER_FIRES_2018.shp" & Fire_ID == "2018-FLEVP-18039" ~ "2018/03/24",
    File_Name == "EVER_FIRES_2018.shp" & Fire_ID == "2018-FLEVP-18063" ~ "2018/07/09",
    File_Name == "EVER_FIRES_2020.shp" & Decld_Date == "2020705" ~ "20200705",
    File_Name == "EVER_FIRES_2020.shp" & Decld_Date == "2020728" ~ "20200728",
    File_Name == "EVER_Fires_2021.shp" & Decld_Date == "32212021" ~ "3212021",
    # If Decld_Date is 0, set to NA
    Decld_Date == "0" ~ NA,
    T ~ Decld_Date
  )) %>%
  dplyr::mutate(Year = dplyr::case_when(
    # Fix wrong years in the Year column
    File_Name == "EVER_FIRES_2017.shp" & Year == "0" ~ "2017",
    File_Name == "EVER_FIRES_2018.shp" & Year == "2017" ~ "2018",
    T ~ Year
  )) %>%
  dplyr::mutate(Disc_Date = dplyr::case_when(
    # Fix wrong dates in Disc_Date column
    File_Name == "EVER_FIRES_2020.shp" & Disc_Date == "2020324" ~ "20200324",
    File_Name == "EVER_FIRES_2020.shp" & Disc_Date == "2020704" ~ "20200704",
    # If Disc_Date is 0, set to NA
    Disc_Date == "0" ~ NA,
    T ~ Disc_Date
  )) %>%
  dplyr::mutate(Fire_Type = dplyr::case_when(
    # If Fire_Type is 48, change it to RX,
    # Otherwise if it's not NA, change it to WF
    Fire_Type == "48" ~ "RX",
    !is.na(Fire_Type) ~ "WF",
    T ~ Fire_Type
  ))

# The 2016-2019, and 2021 shapefiles have different date formats in their Disc_Date and Decld_Date columns
# These need to be changed to YYYYMMDD like the rest of the files
tidy_v2_EVER <- tidy_v1_EVER %>%
  dplyr::mutate(date_format = dplyr::case_when(
    # Create a new date_format column that denotes each file's date formats
    File_Name == "EVER_FIRES_2016.shp" ~ "MMDDYYYY",
    File_Name == "EVER_FIRES_2017.shp" & stringr::str_detect(Disc_Date, "2017$") ~ "MMDDYYYY",
    File_Name == "EVER_FIRES_2017.shp" & stringr::str_detect(Disc_Date, "^2017") ~ "YYYYMMDD",
    File_Name == "EVER_FIRES_2018.shp" | File_Name == "EVER_FIRES_2019.shp" ~ "YYYY/MM/DD",
    File_Name == "EVER_Fires_2021.shp" ~ "MMDDYYYY",
    T ~ "YYYYMMDD"
  )) %>%
  dplyr::mutate(year_fix_disc = dplyr::case_when(
    # Create a new year_fix_disc column that contains the year from rows that have MMDDYYYY format
    date_format == "MMDDYYYY" ~ stringr::str_extract(Disc_Date, "[:digit:]{4}$"),
  )) %>%
  dplyr::mutate(year_fix_decld = dplyr::case_when(
    # Create a new year_fix_decld column that contains the year from rows that have MMDDYYYY format
    date_format == "MMDDYYYY" ~ stringr::str_extract(Decld_Date, "[:digit:]{4}$"),
  )) %>%
  dplyr::mutate(month_fix_disc = dplyr::case_when(
    # Create a new month_fix_disc column that contains the month from rows that have MMDDYYYY format
    date_format == "MMDDYYYY" & nchar(Disc_Date) == 7 ~ paste0("0", stringr::str_extract(Disc_Date, "^[:digit:]{1}")),
    date_format == "MMDDYYYY" & nchar(Disc_Date) == 8 ~ stringr::str_extract(Disc_Date, "^[:digit:]{2}"),
  )) %>%
  dplyr::mutate(month_fix_decld = dplyr::case_when(
    # Create a new month_fix_decld column that contains the month from rows that have MMDDYYYY format
    date_format == "MMDDYYYY" & nchar(Decld_Date) == 7 ~ paste0("0", stringr::str_extract(Decld_Date, "^[:digit:]{1}")),
    date_format == "MMDDYYYY" & nchar(Decld_Date) == 8 ~ stringr::str_extract(Decld_Date, "^[:digit:]{2}"),
  )) %>%
  dplyr::mutate(day_fix_disc = dplyr::case_when(
    # Create a new day_fix_disc column that contains the MMDD from rows that have MMDDYYYY
    date_format == "MMDDYYYY" ~ stringr::str_replace(Disc_Date, "[:digit:]{4}$", ""),
  )) %>%
  dplyr::mutate(day_fix_disc = dplyr::case_when(
    # Edit the day_fix_disc column to contain the day from rows that have MMDDYYYY format
    date_format == "MMDDYYYY" ~ stringr::str_extract(day_fix_disc, "[:digit:]{2}$"),
  )) %>%
  dplyr::mutate(day_fix_decld = dplyr::case_when(
    # Create a new day_fix_decld column that contains the MMDD from rows that have MMDDYYYY
    date_format == "MMDDYYYY" ~ stringr::str_replace(Decld_Date, "[:digit:]{4}$", ""),
  )) %>%
  dplyr::mutate(day_fix_decld = dplyr::case_when(
    # Edit the day_fix_decld column to contain the day from rows that have MMDDYYYY format
    date_format == "MMDDYYYY" ~ stringr::str_extract(day_fix_decld, "[:digit:]{2}$"),
  )) %>%
  dplyr::mutate(Disc_Date_fix = dplyr::case_when(
    # Create a Disc_Date_fix that contains the fixed discovered date in YYYYMMDD format 
    date_format == "MMDDYYYY" ~ paste0(year_fix_disc, month_fix_disc, day_fix_disc),
    date_format == "YYYY/MM/DD" ~ stringr::str_replace_all(Disc_Date, "/", ""),
    T ~ Disc_Date
  ), .after = Disc_Date) %>%
  dplyr::mutate(Decld_Date_fix = dplyr::case_when(
    # Create a Decld_Date_fix that contains the fixed declined date in YYYYMMDD format 
    date_format == "MMDDYYYY" ~ paste0(year_fix_decld, month_fix_decld, day_fix_decld),
    date_format == "YYYY/MM/DD" ~ stringr::str_replace_all(Decld_Date, "/", ""),
    T ~ Decld_Date
  ), .after = Decld_Date) %>%
  # Drop unneeded columns
  dplyr::select(-Disc_Date, -Decld_Date, -date_format,
         -year_fix_disc, -month_fix_disc, -day_fix_disc,
         -year_fix_decld, -month_fix_decld, -day_fix_decld) %>%
  # Rename the fixed columns 
  dplyr::rename(Disc_Date = Disc_Date_fix) %>%
  dplyr::rename(Decld_Date = Decld_Date_fix) %>%
  # Convert all dates into proper Date columns
  dplyr::mutate(Disc_Date = readr::parse_date(Disc_Date, "%Y%m%d"),
         Decld_Date = readr::parse_date(Decld_Date, "%Y%m%d")) %>%
  # Convert Year to a numeric column
  dplyr::mutate(Year = as.numeric(Year))

## ----------------------------------------------- ##
# Fixing Issues with Big Cypress Fire Data (BICY) ----
## ----------------------------------------------- ##

# Import BICY shapefiles (1978-2023) -------------

folder_BICY <- "BICY_FIRE_Perimeter_original"

# Identify relevant BICY files
( raw_files_BICY <- dir(path = folder_BICY, "*.shp$") ) 

# Create empty list to populate with shapefiles later
shp_list_BICY <- list()

# For each shapefile...
for (i in 1:length(raw_files_BICY)){
  
  # Grab its name
  raw_file_name <- raw_files_BICY[i]
  
  message(paste0("Reading ", raw_file_name))
  
  # Read in shapefile
  shp <- sf::read_sf(file.path(folder_BICY, raw_files_BICY[i])) %>%
    # Transform to WGS84
    sf::st_transform("EPSG:4326") %>%
    # Make valid
    sf::st_make_valid()
  
  # Add to list
  shp_list_BICY[[raw_file_name]] <- shp
}

# For each shapefile...
for (i in 1:length(shp_list_BICY)){
  # Grab its name
  file_name <- names(shp_list_BICY[i])
  
  # If the file is "FiresAndBurns_FY78toFY21.shp"...
  if (file_name == "FiresAndBurns_FY78toFY21.shp"){
    shp_list_BICY[[file_name]] <- shp_list_BICY[[file_name]] %>%
      # Create a new column to denote the shapefile where the data came from
      dplyr::mutate(File_Name = file_name, .before = FIRE_ID) %>%
      # Move Shape_Leng column
      dplyr::relocate(Shape_Leng, .after = Shape_Area)
  }
  
  # If the file is not "FiresAndBurns_FY78toFY21.shp"...
  if (file_name != "FiresAndBurns_FY78toFY21.shp"){
    shp_list_BICY[[file_name]] <- shp_list_BICY[[file_name]] %>%
      # Drop Shape_Leng and GlobalID columns
      dplyr::select(-Shape_Leng, -GlobalID) %>%
      # Rename the shape columns to match FiresAndBurns_FY78toFY21.shp
      dplyr::rename(Shape_Area = Shape__Are) %>%
      dplyr::rename(Shape_Leng = Shape__Len) %>%
      # Create a new column to denote the shapefile where the data came from
      dplyr::mutate(File_Name = file_name, .before = FIRE_ID)
  }
}

# Unlist the list we generated from above
tidy_v0_BICY <- shp_list_BICY %>%
  purrr::list_rbind(x = .)

# Format columns to match Everglades -------------

tidy_v1_BICY <- tidy_v0_BICY %>%
  # Select relevant columns
  dplyr::select(File_Name, FIRE_ID, FireNumber, FireName, CalendYear, StartDate, EndDate, FireType, geometry) %>%
  # Standardize column names
  dplyr::rename(Fire_ID = FIRE_ID) %>%
  dplyr::rename(Fire_Number = FireNumber) %>%
  dplyr::rename(Fire_Name = FireName) %>%
  dplyr::rename(Year = CalendYear) %>%
  dplyr::rename(Disc_Date = StartDate) %>%
  dplyr::rename(Decld_Date = EndDate) %>%
  dplyr::rename(Fire_Type = FireType) %>%
  # Edit Fire_Type column according to these conditions:
  dplyr::mutate(Fire_Type = dplyr::case_when(
    # When Fire_Type is "Wildland", "Other", "Unknown", "Vehicle Fire", "Fire Use", or "Chemical", set the value to "WF"
    Fire_Type %in% c("Wildland", "Other", "Unknown",
                     "Vehicle Fire", "Fire Use", "Chemical") ~ "WF",
    # When Fire_Type is "Prescribed", "Burn Area", or "Mechanical", set the value to "RX"
    Fire_Type %in% c("Prescribed", "Burn Area", "Mechanical") ~ "RX"
  )) %>%
  # Make all columns into character columns
  dplyr::mutate(dplyr::across(.cols = -c(geometry), .fns = as.character))
  
# Resolving remaining date format issue -------------

tidy_v2_BICY <- tidy_v1_BICY %>%
  # Create a flag for missing start/end days
  dplyr::mutate(Date_Flag = dplyr::case_when(
    Disc_Date %in% c("2003/09/", "2003/07/", "2007/05",
                     "2014/05/xx", "2014/06/xx") ~ "Missing exact start day, so it has been set to the last day of the month",
    Decld_Date %in% c("2016/06/xx", "2016/10/xx") ~ "Missing exact end day, so it has been set to the last day of the month",
    T ~ NA
  ), .after = Decld_Date) %>%
  dplyr::mutate(Disc_Date = dplyr::case_when(
    # Fix wrong dates in Disc_Date column
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "1981/0208" ~ "1981/02/08",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "198/103/29" ~ "1981/03/29",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "1981/0729" ~ "1981/07/29",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "1982/0109" ~ "1982/01/09",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "0//" ~ NA,
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "2003/09/" ~ "2003/09/30",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "2003/07/" ~ "2003/07/31",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "2007/05" ~ "2007/05/31",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "1998/02/29" ~ "1998/03/01",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "2012/0201" ~ "2012/02/01",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "2014/05/xx" ~ "2014/05/31",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Disc_Date == "2014/06/xx" ~ "2014/06/30",
    File_Name == "BICY_2022_Fires.shp" & Disc_Date == "10/18/2021" ~ "2021/10/18",
    T ~ Disc_Date
  )) %>%
  dplyr::mutate(Decld_Date = dplyr::case_when(
    # Fix wrong dates in Decld_Date column
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "7/16/2020" ~ "2020/07/16",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "07/14/2020" ~ "2020/07/14",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "07/16/2020" ~ "2020/07/16",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "1981/03/39" ~ "1981/03/29",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "0//" ~ NA,
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "200/02/15" ~ "2000/02/15",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "2006/15/13" ~ "2006/05/13",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "2016/06/xx" ~ "2016/06/30",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "2016/10/xx" ~ "2016/10/31",
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "2017/" ~ NA,
    File_Name == "FiresAndBurns_FY78toFY21.shp" & Decld_Date == "2021/09/31" ~ "2021/10/01",
    File_Name == "BICY_2022_Fires.shp" & Decld_Date == "10/17/2021" ~ "2021/10/17",
    File_Name == "BICY_2022_Fires.shp" & Decld_Date == "10/21/2021" ~ "2021/10/21",
    File_Name == "BICY_2022_Fires.shp" & Decld_Date == "12/14/2021" ~ "2021/12/14",
    File_Name == "BICY_2022_Fires.shp" & Decld_Date == "12/15/2021" ~ "2021/12/15",
    File_Name == "BICY_2022_Fires.shp" & Decld_Date == "05/13/2022" ~ "2022/05/13",
    T ~ Decld_Date
  )) %>%
  # Convert all dates into proper Date columns
  dplyr::mutate(Disc_Date = readr::parse_date(Disc_Date, "%Y/%m/%d"),
         Decld_Date = readr::parse_date(Decld_Date, "%Y/%m/%d")) %>%
  # Convert Year to a numeric column
  dplyr::mutate(Year = as.numeric(Year))


## ------------------------------------------------------------ ##
# Combining Regions (EVER and BICY) for all Everglades Fires (EVG) 
## ------------------------------------------------------------ ##

# Clip EVER fire dataset down to same timeframe as BICY (1978-2023)
tidy_v3_EVER <- tidy_v2_EVER %>%
  dplyr::filter(Year >= 1978)

# Combine EVER and BICY shapefiles together
tidy_v0_EVER_BICY <- tidy_v3_EVER %>%
  dplyr::bind_rows(tidy_v2_BICY) %>%
  dplyr::relocate(Date_Flag, .after = Decld_Date)

# Export harmonized tidy fire perimeters
sf::st_write(tidy_v0_EVER_BICY, file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "FireHistory", "EVER_BICY_1978_2023_perim.shp"),
             append = FALSE)
