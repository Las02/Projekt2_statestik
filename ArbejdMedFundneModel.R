
#### Loading and mutating/formating the data ####
## Reading in the libraries
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(car)
library(stringr)
library(xtable)
library(lubridate)

## Reading in the data
D <- read.csv("merged_data.csv", header=TRUE)

## Setting the datatypes
# Setting the factors
D <- mutate(D, across(c(dir, cond, fog, rain, ID),factor))
# Setting the dates
D$date <- as.POSIXct(D$date, tz = "UTC")

## Mutating the data
# Adding temp difference column to the dataframe 
D$tempdif <- 21 - D$temp

# Adding the weekends as a column
is_weekend <- function(date){
  number_day_df <- wday(date, label=T)
  number_day_char <- as.character(number_day_df)
  return(number_day_char)
}
D <- mutate(D, weekday=is_weekend(date)) %>% 
  mutate(weekend = ifelse(weekday %in% c("lø","sø"),"weekend","workingday")) %>% 
  mutate(weekend = factor(weekend))

# getting start/end of month int (mutating the date variable)
D <- mutate(D, dag=str_split_fixed(date,"-",3)[ ,3])
D <- mutate(D, start_or_end = ifelse(as.integer(dag)<15, "START","END")) %>% 
  mutate(start_or_end = factor(start_or_end))

# find the normalised data
mean_each <- group_by(D, ID) %>% 
  summarise(mean_each = mean(consumption))
D_with_mean <- inner_join(mean_each, D, "ID") 
D <- mutate(D_with_mean, ncons = consumption/mean_each)

## Removing non important columns
D <- select(D,!c("temp","mean_each","dag","weekday"))



#### Removing outliers #### 