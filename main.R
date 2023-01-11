#### Introduction ####
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(car)

load("./WUndergroundHourly.RData")
summary(WG)

# Remove all coloumns without data (only NA columns)
clima <- select(WG,!c("wind_gust","wind_chill","heat_index","precip","precip_rate","precip_total"))

# Remove columns with fixed values
clima <- select(clima, !c("hail","thunder","tornado"))

#formatting data



# Picking the correct datatype for the columns
clima <- mutate(clima,
                dir  = factor(dir),
                cond = factor(cond),
                fog  = factor(fog),
                rain = factor(rain),
                snow = factor(snow)
)