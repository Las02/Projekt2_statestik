#### Introduction ####
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(car)
library(stringr)
library(xtable)

load("./WUndergroundHourly.RData")

# Remove all columns without data (only NA columns)
clima <- select(WG,!c("wind_gust"  , "wind_chill",
                      "heat_index" , "precip",
                      "precip_rate", "precip_total"))

# Remove columns with fixed values
clima <- select(clima, !c("hail","thunder","tornado"))

# making seperate column for date and time
clima[c('date','time')] <- str_split_fixed(clima$date,' ',2)

# Picking the correct datatype for the columns
clima <- mutate(clima,
  across(c(dir, cond, fog, rain, snow, date, time),factor)
)

### Finding mode and mean for each date ###
clima_by_date <- group_by(clima, date)

# Define function to calculate mode
mode <- function(factors){
  max <- factors %>% 
    table() %>% 
    which.max() %>% 
    as.data.frame() %>% 
    rownames() %>% 
    factor()
  
# If it does not have a value set it to None for clarity
  if (max == ""){
    return(factor("None"))}
  else {
    return(max)
  }  
}

# Assign the mode and mean using aggregate functions
clima_mean_mode <- clima_by_date %>% 
  summarise(
    across(c(dir, cond, fog, rain, snow),mode),
    across(c(temp, dew_pt, hum, wind_spd, vis, pressure), ~mean(.,na.rm=T))        
    )

#### Read in the energy performance of the building ####
# Find all the files in ./data
data_files <- dir("./data", full.names=T)

# Read them into a single dataframe
# \x00 is set to be ignored  since it for some reason is at the end of each file
energy <- NULL
for (i in seq_along(data_files)){
  data <- read.table(data_files[i], sep=";", skipNul=TRUE) %>% 
    select(V1, V2, V4)
  energy <- bind_rows(energy, data)
}

# Renaming the dataframe
energy <- energy %>% 
  rename(id=V1, time=V2, reading=V4)

## Exclude meters with less than 121 records
# find the records with 121 records
id_to_keep <- group_by(energy, id) %>% 
  summarise(n=n()) %>% 
  filter(n==121)
# only keep these
energy <- filter(energy, id %in% id_to_keep$id)

# set the correct datatypes
energy <- mutate(energy, 
                 reading = as.numeric(gsub(",", ".", reading)))
# Date CET/CEST refers to winter-/summer-time
energy$time <- as.POSIXct(strptime(energy$time,"%d-%m-%Y %H.%M"))

# Make data to approximate new values at 11:59:00
days <- unique(as.Date(energy$time))
time <- "11:59:00"
new_time_date <- NULL
for(i in seq_along(days)){
  days_time <- paste(days,time) %>% 
      as.POSIXct()
  new_time_date <- append(new_time_date, days_time)
}

## Approximate new values at 11:59:00
id_time_cons <- NULL
all_id <- unique(energy$id)

for (i in seq_along(all_id)){
# select values with the correct id
energy_for_id <- filter(energy, id == all_id[i])

# Approximate new values
approx <- approx(energy_for_id$time, 
                 energy_for_id$reading, xout=new_time_date)

# Assign the new values to a temp df
time <- as.Date(approx$x)
consumption <- approx$y
id <- rep(all_id[5],length(time))
temp_df <- data.frame(time,id,consumption)

# add them to the id_time_cons df
id_time_cons <- bind_rows(temp_df,id_time_cons)
}


