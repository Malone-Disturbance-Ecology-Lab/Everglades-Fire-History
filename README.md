# Everglades-Fire-History
Uses fire perimeters to measure fire history patterns in the Everglades

## Script Explanations

**01_Flow_FireData_Harmonization_Angel.R**: This script cleans and combines fire history shapefiles from Everglades National Park (EVER 1948-2021) and Big Cypress National Preserve (BICY 1978-2021). The harmonized fire perimeters are exported as `EVER_BICY_1978_2021_perim.shp` in `malonelab -> Research -> ENP -> ENP Fire -> FireHistory`. This is meant to be a rewritten version of **Grace_scripts/01_Flow_FireData_Harmonization.R**.

**02_Flow_UplandsRaster_Creation.R**: This script uses Landsat images downloaded from Earth Explorer to generate a 30m raster grid, which is then used to generate the upland vegetation layer for Everglades National Park and Big Cypress National Preserve. The upland raster is exported as `Uplands_raster.tif` in a `02_tidy_uplands_raster` folder.

**03_Flow_FireHistory_Rasterization.R**: This script rasterizes yearly fire history shapefile data using `EVER_BICY_1978_2021_perim.shp` and `Uplands_raster.tif`. The yearly burned raster stack is exported as `EVER_BICY_1978_2021_burned.tif`, and the year of occurrence raster stack is exported as `EVER_BICY_1978_2021_year_occurrence.tif` in `malonelab -> Research -> ENP -> ENP Fire -> FireHistory`. This is meant to be a rewritten version of **Grace_scripts/Fire_History_Layers.R**.
