#### Introduction ####
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(car)
library(stringr)

load("./WUndergroundHourly.RData")

# Remove all coloumns without data (only NA columns)
clima <- select(WG,!c("wind_gust","wind_chill","heat_index","precip","precip_rate","precip_total"))

# Remove columns with fixed values
clima <- select(clima, !c("hail","thunder","tornado"))

#formatting data
clima[c('date','time')] <- str_split_fixed(clima$date,' ',2)

# Picking the correct datatype for the columns
clima <- mutate(clima,
                dir  = factor(dir),
                cond = factor(cond),
                fog  = factor(fog),
                rain = factor(rain),
                snow = factor(snow),
                date = factor(date),
                time = factor(time)
)

### Finding mode and mean for each date ###
clima_by_date <- group_by(clima, date)

mode <- function(factors){
factors %>% 
  table() %>% 
  which.max() %>% 
  as.data.frame() %>% 
  rownames() %>% 
  factor()
}

clima_by_date %>% 
  summarise(
    across(c(dir, cond, fog, rain, snow),mode),
    across(c(temp, dew_pt, hum, wind_spd, vis, pressure), ~mean(.,na.rm=T))        
    )




