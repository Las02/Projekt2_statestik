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


clima_by_date <- group_by(clima, date)
summarise(clima_by_date, 
          mean(temp, na.rm = T),
          mean(dew_pt, na.rm = T),
          mean(hum, na.rm = T),
          mean(wind_spd, na.rm = T),
          mean(vis, na.rm = T),
          mean(pressure, na.rm = T))



getmode <- function(v) {
  groupv <- unique(v) 
  tab <- tabulate(match(v,groupv))
  groupv[tab == max(tab)]
}

factor = "dir"
all_dates <- levels(clima$date)
for (i in 1:1){
  date <- filter(clima, date == all_dates[i]) %>% 
    select(factor)
  getmode(date)
}  




