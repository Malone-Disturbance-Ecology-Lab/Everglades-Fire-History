# Upland Vegetation Layers 
# Simple Features of Interest:
library(dplyr)
library(sf)

rm(list=ls())

setwd("//Volumes/malonelab/Research/ENP")
load(file = "ENP_BCNP_vegetation.RDATA")

ENP_sf <- ENP %>% st_as_sf 
eBICY_sf <- eBICY %>% st_as_sf 
wBICY_sf <- wBICY %>% st_as_sf 

ENP_df$L2_name %>% unique
wBICY$L2_name %>% unique
eBICY$L2_name %>% unique

L1.list <- list("Scrub","Dune", "Forest", "Marsh", "Shrubland",  "Woodland" )

L2.list <- list( "Wetland Scrub"  ,   "Upland Scrub" ,     "Upland Forest"    ,"Wetland Forest" ,   "Freshwater Marsh" , "Wetland Shrubland", "Upland Shrubland"  ,"Wetland Woodland" , "Upland Woodland",  "Salt Marsh" , "Mangrove Scrub"  , "Swamp Scrub" ,"Upland Scrub", "Hammock Forest", "Mangrove Forest"  , "Swamp Forest", "Freshwater Marsh", "Salt Marsh" ,  "Mangrove Shrubland" , "Upland Shrubland","Mangrove Woodland" ,"Swamp Woodland",  "Upland Woodland"  ,"Graminoid Dune",  "Herbaceous Dune" ,  "Australian Pine",   "Giant Grasses"   , "Upland Forest"  ,    "Wetland Forest",    "Freshwater Marsh"  ,"Salt Marsh" ,  "Wetland Shrubland" ,"Upland Shrubland",  "Wetland Woodland" , "Upland Woodland")


ENP_sf_sub <-ENP_sf%>% filter( L2_name %in% L2.list,
                               L1_name %in% L1.list ) %>% select("L1_name", "L2_name") 


eBICY_sf_sub <-eBICY_sf%>% filter( L2_name %in% L2.list,
                               L1_name %in% L1.list  ) %>% select("L1_name", "L2_name") 


wBICY_sf_sub <-wBICY_sf%>% filter( L2_name %in% L2.list,
                               L1_name %in% L1.list ) %>% select("L1_name", "L2_name") 

# Level 1 names: 
Everglades_veg_l1 <- rbind( ENP_sf_sub, eBICY_sf_sub, wBICY_sf_sub) 


forest <- c("Forest","Woodland")
scrub<- c("Shrubland", "Scrub")

Everglades_veg_l1 <- Everglades_veg_l1 %>% mutate(L1.new = L1_name) %>% 
  mutate(L1.new = case_when(L1_name %in% forest ~ "Forest",
                            L1_name %in% scrub ~ "Shrubland",
                            is.na(L1.new) ~ L1_name)) 

Everglades_veg_l1$L1.new[ is.na(Everglades_veg_l1$L1.new) ] <- Everglades_veg_l1$L1_name[ is.na(Everglades_veg_l1$L1.new)]
Everglades_veg_l1$L1.new %>% unique

Everglades_veg_l1n <- Everglades_veg_l1 %>% group_by(L1.new) %>% summarize() 

# Level 2 names: 
Everglades_veg_l2 <- rbind( ENP_sf_sub, eBICY_sf_sub, wBICY_sf_sub)  


swamp.forest <- c("Swamp Forest", "Swamp Woodland")
w.forest <- c("Wetland Forest","Wetland Woodland")
forest <- c("Upland Woodland", "Upland Forest", "Australian Pine")
w.shrub <- c("Swamp Shrubland" , "Wetland Shrubland", "Swamp Scrub", "Wetland Scrub","Mangrove Scrub", "Mangrove Shrubland")
u.shrub <- c( "Upland Scrub", "Upland Shrubland","Mangrove Forest", "Mangrove Woodland")
dune <- c("Graminoid Dune", "Herbaceous Dune")


Everglades_veg_l2 <- Everglades_veg_l2 %>% mutate(L2.new = L2_name) %>% 
  mutate(L2.new = case_when(L2_name %in% w.forest ~ "Wetland Forest",
                            L2_name %in% forest ~ "Upland Forest",
                            L2_name %in% dune ~ "Herbaceous-Graminoid Dune",
                            L2_name %in% swamp.forest ~ "Swamp Forest",
                            L2_name %in% w.shrub ~ "Wetland Shrubland",
                            L2_name %in% u.shrub ~ "Upland Shrubland",
                            is.na(L2.new) ~ L2_name)) 

Everglades_veg_l2$L2.new[ is.na(Everglades_veg_l2$L2.new) ] <- Everglades_veg_l2$L2_name[ is.na(Everglades_veg_l2$L2.new)]
Everglades_veg_l2$L2.new %>% unique

Everglades_veg_l2n<- Everglades_veg_l2%>% group_by(L2.new) %>% summarize() 

save(Everglades_veg_l2n , Everglades_veg_l1n, file= '//Volumes/malonelab/Research/ENP/EVERGLADES_VEGETATION_L1_L2.RDATA' )
