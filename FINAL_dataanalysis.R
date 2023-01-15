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
  mutate(weekend = ifelse(weekday %in% c("lø","sø"),"weekend","workingday"))

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


#### Analyzing the different models ####
## Basic
D_basic <- select(D,ID, ncons, tempdif)
fit_basic <- lm(ncons~. ,D_basic)
f_basic <- step(fit_basic, scope = ~.^3 , k=log(nrow(D_basic)), test = "F")
AIC(fit_basic)

## +Start or end
D_startend <- select(D,ID, ncons, tempdif,  start_or_end)
fit_startend <- lm(ncons~. ,D_startend)
f_startend <- step(fit_startend, scope = ~.^3 , k=log(nrow(D_startend)), test = "F")
AIC(f_startend)

## +weekday
D_weekday <- select(D,ID, ncons, tempdif, weekend)
fit_weekday <- lm(ncons~. ,D_weekday)
f_weekday <- step(fit_weekday, scope = ~.^3 , k=log(nrow(D_weekday)), test = "F")

## +weekday +startend
D_weekday_startend <- select(D,ID, ncons, tempdif, weekend, start_or_end)
fit_weekday_startend <- lm(ncons~. ,D_weekday_startend)
f_weekday_startend <- step(fit_weekday_startend, scope = ~.^3 , k=log(nrow(D_weekday_startend)), test = "F")

## Comparing the 4
AIC(fit_basic, f_startend, f_weekday,f_weekday_startend)


#### Important plots ####

## START/END IMPORTANT
# It looks like end/start date has an effect
ggplot(D,aes(x=date, y=ncons,col=ID)) + 
  geom_point(size=0.8) + theme(legend.position = "none") 
# But it is explained by the temperature fluxating 
ggplot(D,aes(x=date, y=tempdif,col=ID)) + 
  geom_point(size=0.8) + theme(legend.position = "none") 

