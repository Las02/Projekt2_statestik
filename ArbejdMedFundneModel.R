
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

# Setting the found model up
fit_before <- lm(ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
  pressure + weekend + ID:tempdif + tempdif:weekend + wind_spd:hum + 
  tempdif:dew_pt + wind_spd:dew_pt + tempdif:hum + hum:dew_pt + 
  ID:weekend + dew_pt:weekend + hum:weekend + dew_pt:pressure, 
  data = D)

# Removing outliers
par(mfrow=c(2,2))
plot(fit_before)
outliers <- c(9841, 1639,9478,9477)
D <- filter(D, !row_number() %in% outliers)
fit_before <- lm(ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
            pressure + weekend + ID:tempdif + tempdif:weekend + wind_spd:hum + 
            tempdif:dew_pt + wind_spd:dew_pt + tempdif:hum + hum:dew_pt + 
            ID:weekend + dew_pt:weekend + hum:weekend + dew_pt:pressure, 
          data = D)
par(mfrow=c(2,2))
plot(fit_before)

# Fitting the model again after
D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure, weekend)
fit_scope <- lm(ncons~. ,D_scope)
fit <- step(fit_scope, scope = ~.^2 , k=log(nrow(D_scope)), test = "F")
# lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
#      pressure + weekend + ID:tempdif + tempdif:weekend + wind_spd:hum + 
#      ID:weekend + tempdif:dew_pt + wind_spd:dew_pt + tempdif:hum + 
#      hum:dew_pt + wind_spd:weekend + dew_pt:weekend + hum:weekend + 
#      dew_pt:pressure, data = D_scope)

AIC(fit, fit_before)
## RESULTS
# The AIC is lower (-3816.148) vs before (-3808.309)
# The model we after removing outliers is the same

#### Can we remove nonsensical interactions #### 
# Based on drop1
drop1(fit, test ="F")
# The nonsensical are eg this one 
AIC(update(fit, .~. -wind_spd:weekend), fit)
anova(update(fit, .~. -wind_spd:weekend), fit)
# but we cannot remove them 


