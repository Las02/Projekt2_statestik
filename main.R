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

# Assign the modes of all
all_factors = c("dir","cond","fog")

for (i in 1:length(all_factors)){
factor = all_factors[i]
all_dates <- levels(clima$date)
put_in_date <- c()
for (i in 1:length(all_dates)){
  factor_for_date <- filter(clima, date == all_dates[i]) %>% 
    select(factor) 
  max <- rownames(as.data.frame(which.max(table(factor_for_date))))
  put_in_date <- c(put_in_date,max)
} 
print(put_in_date)
}




