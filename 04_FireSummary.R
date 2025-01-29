library(units)
library(ggplot2)
library(dplyr)
library(zoo)
summary_calculations %>% names

summary_calculations %>%  ggplot( aes(x= Year, y = total_area_burned)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y=rollmean(total_area_burned, 10, na.pad=TRUE)))

summary_calculations %>%  ggplot( aes(x= Year, y = total_WF_burned)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y=rollmean(total_WF_burned, 10, na.pad=TRUE)))

summary_calculations %>%  ggplot( aes(x= Year, y = total_RX_burned)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y=rollmean(total_RX_burned, 10, na.pad=TRUE)))

summary_calculations %>%  ggplot( aes(x= Year, y = mean_fire_size)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y=rollmean(mean_fire_size, 10, na.pad=TRUE)))

summary_calculations %>%  ggplot( aes(x= Year, y = mean_area_over_perim)) + 
  geom_col( ) + theme_bw( ) + 
  geom_line(aes(y=rollmean(mean_area_over_perim, 10, na.pad=TRUE)))
