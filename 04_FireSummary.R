
rm(list=ls())

library(units)
library(ggplot2)
library(dplyr)
library(zoo)
library(terra)
library(sf)
library(tidyterra)
library(ggpubr)
library(paletteer)
library(forcats)
library(ggspatial)

directory <- file.path("/", 'Users', 'sm3466', "YSE Dropbox", "Sparkle Malone", "Research", "Everglades-Fire-History")
setwd(directory)

# Summary Time Series:  ####
firehist_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "FireHistory") 

summary_calculations <- read.csv(file.path(firehist_folder, "summary_calculations.csv"))

summary_calculations.ma <- summary_calculations %>% mutate( ma.total_area_burned = zoo::rollmean( total_area_burned, 5,fill=NA),
                                 ma.total_WF_burned = zoo::rollmean( total_WF_burned, 5,fill=NA),
                                 ma.total_RX_burned = zoo::rollmean( total_RX_burned, 5,fill=NA),
                                 ma.mean_fire_size = zoo::rollmean( mean_fire_size, 5,fill=NA),
                                 ma.mean_area_over_perim = zoo::rollmean( mean_area_over_perim, 5,fill=NA) ) 

# Evaluation of total area burned:
library(tseries)
(summary_calculations.ma$total_area_burned/1000000) %>% range
acf(summary_calculations.ma$total_area_burned ) # Test for autocorrelation over time.
acf(summary_calculations.ma$total_WF_burned ) # Test for autocorrelation over time.
acf(summary_calculations.ma$total_RX_burned ) # Test for autocorrelation over time.

lm( data = summary_calculations.ma,total_area_burned/1000000 ~ Year) %>% summary

sum(summary_calculations.ma$total_WF_burned /1000000)/sum(summary_calculations.ma$total_area_burned/1000000) *100
sum(summary_calculations.ma$total_RX_burned /1000000)/sum(summary_calculations.ma$total_area_burned /1000000) *100

lm( data = summary_calculations.ma,total_WF_burned/1000000 ~ Year) %>% summary

lm( data = summary_calculations.ma,total_RX_burned/1000000 ~ Year) %>% summary

# Evaluation of fire Size:
summary_calculations.ma$mean_fire_size/1000000

summary_calculations.ma$mean_fire_size %>% acf
summary_calculations.ma$mean_fire_size[summary_calculations.ma$Year <= 2000] %>% mean/1000000
summary_calculations.ma$mean_fire_size[summary_calculations.ma$Year <= 2000] %>% sd/sqrt(length(summary_calculations.ma$mean_fire_size[summary_calculations.ma$Year <= 2000] )) /1000000

summary_calculations.ma$mean_fire_size[summary_calculations.ma$Year > 2000] %>% mean/1000000
summary_calculations.ma$mean_fire_size[summary_calculations.ma$Year > 2000] %>% sd/sqrt(length(summary_calculations.ma$mean_fire_size[summary_calculations.ma$Year > 2000] )) /1000000

summary_calculations.ma$mean_area_over_perim %>% acf

# Number of fires:

total_fires_1978_2023.cm


# Summary of temporal change:
summary_calculations.ma %>% names


p.total.area <- summary_calculations.ma %>%  ggplot( aes(x= Year, y = total_area_burned/1000000)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y= ma.total_area_burned/1000000 ), col="goldenrod",  linewidth = 1) + 
  ylab( expression('Total Burned Area (km' ^2*')' )) + xlab('')


p.total.area.WF <- summary_calculations.ma %>%  ggplot( aes(x= Year, y = total_WF_burned/1000000)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y= ma.total_WF_burned/1000000 ), col="goldenrod",  linewidth = 1) + 
  ylab( expression('Wildfires (km' ^2*')' )) + xlab('')

p.total.area.Rx <- summary_calculations.ma %>%  ggplot( aes(x= Year, y = total_RX_burned/1000000)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y= ma.total_RX_burned/1000000 ), col="goldenrod",  linewidth = 1) + 
  ylab( expression('Prescribed Fires (km' ^2*')' )) + xlab('')

p.mean_fire_size <- summary_calculations.ma %>%  ggplot( aes(x= Year, y = mean_fire_size/1000000)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y= ma.mean_fire_size/1000000 ), col="goldenrod",  linewidth = 1) + 
  ylab( expression('Fire Size (km' ^2*')' )) + xlab('')

p.mean_area_over_perim <- summary_calculations.ma %>%  ggplot( aes(x= Year, y = mean_area_over_perim)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y= ma.mean_area_over_perim), col="goldenrod",  linewidth = 1) + 
  ylab( expression('Fire Area:Perimeter' )) + xlab('')

p.1 <- ggarrange(p.total.area, labels=c("a."))

p.2 <-ggarrange(p.total.area.WF, 
          p.total.area.Rx, ncol=2, nrow=1,
          labels=c("b.", "c."))

png("FIGURES/Total_Area_Timeseries.png", width = 2000, height=1500, res=300)
ggarrange(p.1, p.2, ncol=1, nrow=2)
dev.off()

png("FIGURES/FireSize_Timeseries.png", width = 2000, height=1400, res=300)
ggarrange( p.mean_fire_size,
p.mean_area_over_perim, ncol=1, nrow=2,labels=c("a.", "b."))
dev.off()

# Summary by Import Vegetation Layer ####

load( '//Volumes/malonelab/Research/ENP/EVERGLADES_VEGETATION_L1_L2.RDATA') 

firehist_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "ENP Fire", "FireHistory") 

total_fires_1978_2023 <- terra::rast(file.path(firehist_folder, "EVER_BICY_1978_2023_total_fires.tif"))

total_fires_1978_2023[ is.na(total_fires_1978_2023)] <- 0

time_since <- terra::rast(file.path(firehist_folder, "EVER_BICY_1978_2023_time_since.tif"))
time_since[ is.na(time_since)] <- 47

names(total_fires_1978_2023) <- 'total_fires_1978_2023'
names(time_since ) <- 'time_since'

shapefile_folder <- file.path("/", "Volumes", "malonelab", "Research", "ENP", "shapefiles") 

enp <- read_sf(file.path(shapefile_folder , "ENP.shp") )
bnp <- read_sf(file.path(shapefile_folder , "Big_Cypress.shp") )

EVG.L1 <- sf::st_as_sf(Everglades_veg_l1n) %>% mutate( area= st_area(geometry)) %>% st_transform(crs(total_fires_1978_2023))

EVG.L2 <- sf::st_as_sf(Everglades_veg_l2n) %>% mutate( area= st_area(geometry)) %>% st_transform(crs(total_fires_1978_2023))

names(enp) <-names(bnp)
aoi <- rbind(bnp, enp) %>% st_transform(crs(total_fires_1978_2023))

total_fires_1978_2023.cm <- terra::crop(total_fires_1978_2023,  EVG.L1) %>% mask(EVG.L1)
time_since.cm <- terra::crop( time_since,  EVG.L1) %>% mask(EVG.L1)

EVG.L1 <- zonal(x = total_fires_1978_2023, z= vect(EVG.L1) , 
                fun = "median", as.polygons=T,  na.rm=TRUE) %>% 
  zonal(x = time_since,
                fun = "median", as.polygons=T,  na.rm=TRUE)


# Figures: 
# Vegetation Layers:

map.l1 <- ggplot() + geom_sf( data = EVG.L1, 
                    aes( fill=L1.new), col=NA )  + 
  paletteer::scale_fill_paletteer_d("ggpomological::pomological_palette") + 
  geom_sf(data = enp,  fill = NA, linewidth = 1.25) +
  geom_sf(data = bnp, fill = NA , linewidth = 1.25) +
  guides(fill=guide_legend(title="")) + theme_bw()

map.l2 <- ggplot() + 
  geom_sf( data = EVG.L2, 
           aes( fill=L2.new) , col=NA) + 
  paletteer::scale_fill_paletteer_d("ggthemes::Tableau_20")+ 
  geom_sf(data = enp,  fill = NA, linewidth = 1.25, color = "black") +
  geom_sf(data = bnp, fill = NA , linewidth = 1.25, color = "black") +
  guides(fill = guide_legend(title = "Ecosystem")) +
  theme_bw()

# load data for the Florida inlay plot
load(file.path(firehist_folder, "FL_inlay.RData"))

# create the Florida inlay plot
inlay_plot <- ggplot() +
  geom_sf(data = FL_bound, fill = "lightgrey", color = "#3B3A3F") +
  geom_sf(data = EVG_bound, color = "#3B3A3F", fill = "#3B3A3F", size = 1.5) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

map.l2_ver2 <- map.l2 +
  # park labels
  annotate(
    "text",
    x = 454000,
    y = 2882900,
    label = "Big\nCypress\nNational\nPreserve",
    size = 3.5,
    color = 'black',
    fontface = "plain"
  ) +
  annotate(
    "text",
    x = 460000,
    y = 2812900,
    label = "Everglades\nNational\nPark",
    size = 3.5,
    color = "black",
    fontface = "plain"
  ) +
  # FL inlay
  annotation_custom(
    grob = ggplotGrob(inlay_plot),
    xmin = 525731,
    xmax = 569000,
    ymin = 2843900,
    ymax = 2942900
  ) +
  # north arrow
  annotation_north_arrow(
    location = "bl",
    width = unit(1, "cm"),
    height = unit(1.5, "cm"),
    pad_y = unit(1, "cm"),
    style = north_arrow_orienteering()
  ) +
  # scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    height = unit(0.4, "cm"),
    style = "ticks",
    bar_cols = c("black", "white"),
    text_cex = 2
  )

setwd(directory)

png("FIGURES/Vegetation MapsL2.png", width = 2200, height=2000, res=300)
ggarrange( map.l2_ver2, labels="a.")
dev.off()

png("FIGURES/Vegetation MapsL1.png", width = 2000, height=2000, res=300)
ggarrange( map.l1, labels="a.")
dev.off()

# Fire History Layers:

EVG.L1.df <- EVG.L1 %>% as.data.frame() %>% mutate( area = as.numeric(area))
EVG.L2.df <- EVG.L2 %>% as.data.frame()


# Summary by Veg: 
p.area <- EVG.L1.df %>% ggplot() + 
  geom_col( aes( x =  L1.new, y = area/1000000) ) + 
  coord_polar() + xlab('')  + 
  ylab(expression('Area( km'^2*')') )+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) + 
  theme_minimal() + coord_radial(inner.radius = 0, 
                            r_axis_inside = TRUE,
                            rotate_angle = TRUE, expand = FALSE,
                            start = 0.25 * pi, end = 1.6 * pi) +
  guides(theta = guide_axis_theta(angle = 0),
         r     = guide_axis(angle = 0))

p.totalFires <- EVG.L1.df %>% ggplot() + 
  geom_col( aes( x =  L1.new, y = total_fires_1978_2023) ) + 
  coord_polar() + xlab('') + ylim( 0, 8) + 
  ylab('median Fires (1978 - 2023)') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) + 
  theme_minimal() + coord_radial(inner.radius = 0, 
                            r_axis_inside = TRUE,
                            rotate_angle = TRUE, expand = FALSE,
                            start = 0.25 * pi, end = 1.6 * pi) +
  guides(theta = guide_axis_theta(angle = 0),
    r     = guide_axis(angle = 0))

p.tsf <-EVG.L1.df %>% ggplot() + 
  geom_col( aes( x =  L1.new ,y = time_since) ) + 
  coord_polar() + xlab('') + ylim( 0, 8) + 
  ylab('Time Since Fire (Years)') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) + 
  theme_minimal() + coord_radial(inner.radius = 0, 
                            r_axis_inside = TRUE,
                            rotate_angle = TRUE, expand = FALSE,
                            start = 0.25 * pi, end = 1.6 * pi) +
  guides(theta = guide_axis_theta(angle = 0),
         r     = guide_axis(angle = 0))+ theme(text = element_text(size = 18))

# L2 Summary:
 
EVG.L2 <- zonal(x = total_fires_1978_2023, z= vect(EVG.L2) , 
                fun = "median", as.polygons=T,  na.rm=TRUE)

EVG.L2$SD.tf <- zonal(x = total_fires_1978_2023, z= EVG.L2 , 
                fun = "sd", as.polygons=F,  na.rm=TRUE)$total_fires_1978_2023

EVG.L2 <- zonal(x = time_since, z= EVG.L2 , 
                fun = "median", as.polygons=T,  na.rm=TRUE)

EVG.L2$SD.tsf <- zonal(x = time_since, z= EVG.L2 , 
                       fun = "sd", as.polygons=F,  na.rm=TRUE)$time_since

EVG.L2.df <- EVG.L2 %>%  as.data.frame %>% mutate( upper.tf = total_fires_1978_2023 + SD.tf,
                                                   lower.tf = total_fires_1978_2023 - SD.tf,
                                                   upper.tsf = time_since + SD.tsf,
                                                   lower.tsf = time_since - SD.tsf,
                                                   area = as.numeric(area)) %>% 
  mutate( lower.tf.adj = case_when(lower.tf < 0 ~ 0, 
                                   lower.tf > 0 ~ lower.tf),
          lower.tsf.adj = case_when(lower.tsf < 0 ~ 0, 
                                   lower.tsf > 0 ~ lower.tsf))
 

# Percentage of the landscape that burned only once:

landscape.frac <- function(raster, value){
  percent <- ((length(raster[ raster == value]) / length(raster[ !is.na(raster)])) *100 ) %>% round(4)
  return( percent)
}

total.fires.summary <- data.frame( value = seq(0, 17) %>% as.numeric  ) 

for ( i in 1:length(total.fires.summary$value )){
  print(i)
  total.fires.summary$frac[i] <- landscape.frac(total_fires_1978_2023.cm, total.fires.summary$value[i])
  print(total.fires.summary$frac[i])
}

p.totalFires <- EVG.L1.df %>% ggplot() + 
  geom_col( aes( x =  L1.new, y = total_fires_1978_2023) ) + 
  coord_polar() + xlab('') + ylim( 0, 8) + 
  ylab('Number of Fires (1978–2023)') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) + 
  theme_minimal() + coord_radial(inner.radius = 0, 
                                 r_axis_inside = TRUE,
                                 rotate_angle = TRUE, expand = FALSE,
                                 start = 0.25 * pi, end = 1.6 * pi) +
  guides(theta = guide_axis_theta(angle = 0),
         r     = guide_axis(angle = 0))+ theme(text = element_text(size = 18))

p.tf.L2 <- total.fires.summary %>% ggplot() + 
                       geom_col( aes(x=value, y= frac )) +
  ylab( 'Landscape Coverage (%)')  + xlab( 'Number of Fires (1978–2023)') +
  theme_minimal() + theme(text = element_text(size = 18))

p.totalFires.l2 <- EVG.L2.df %>% ggplot() + 
  geom_col( aes( x = fct_reorder( L2.new, total_fires_1978_2023), 
                 y = total_fires_1978_2023)) + theme_bw() +
  theme(text = element_text(size = 18), 
        axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab( "Number of Fires\n(1978–2023)") + xlab("") +
  geom_errorbar(data= EVG.L2.df, 
                aes( x = fct_reorder( L2.new, total_fires_1978_2023),
                     ymin = lower.tf.adj, ymax = upper.tf))  

map.total.fires <- ggarrange(ggplot( ) + 
  geom_spatraster(data =total_fires_1978_2023.cm ) + 
  paletteer::scale_fill_paletteer_c("ggthemes::Sunset-Sunrise Diverging",na.value = "transparent") + theme_minimal()+
  geom_sf(data = aoi,  fill = NA, linewidth = 1.25, color = "black") + 
    labs(fill = "Fires") +
    theme(text = element_text(size = 20), 
          legend.title =  element_text(size = 15),
          legend.text =  element_text(size = 13),
          axis.text.x = element_text(angle = 45))) 



png("FIGURES/Total_Fires_MapsL2.png", width = 3000, height=3300, res=300)
ggarrange(map.total.fires, p.tf.L2 , p.totalFires, p.totalFires.l2,
          ncol=2, nrow=2, labels=c("a.", "b.", "c.", "d."),
          font.label=list(color="black",size=20))

dev.off()

p.TSF.l2 <-EVG.L2.df %>% ggplot() + 
  geom_col( aes( x = fct_reorder( L2.new, time_since), y =time_since )) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  xlab("")  +  ylab("Time Since Fire (Years)") +
  geom_errorbar(data= EVG.L2.df, 
                aes( x = fct_reorder( L2.new, time_since),
                     ymin = lower.tsf.adj, ymax = upper.tsf)) + theme_bw() + 
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust=1)) 


map.time_since <- ggarrange(ggplot( ) + 
                               geom_spatraster(data = time_since.cm ) + 
                               paletteer::  scale_fill_paletteer_c("grDevices::Purple-Yellow", na.value='transparent') +
                               theme_minimal() +
                               geom_sf(data = aoi,  fill = NA, linewidth = 1.25, color = "black") + 
                               labs(fill = "Years") +
                               theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45),
                                     legend.title =  element_text(size = 18)), labels="a.",
                            font.label=list(size=20))



plots <- ggarrange( p.tsf,p.TSF.l2,
                   ncol=2, nrow=1, labels=c("b.", "c."),
                   font.label=list(size=20))

png("FIGURES/TSF_Maps.png", width = 3000, height=3400, res=300)
ggarrange(map.time_since,plots, ncol=1, nrow=2)

dev.off()

# Evaluation of the Number of fires:
total_fires_1978_2023.cm %>% hist

