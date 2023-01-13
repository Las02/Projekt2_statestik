#### Introduction ####
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(car)
library(stringr)
library(xtable)

#### Loading the data ####
D <- read.csv("merged_data.csv", header=TRUE)

#### Changing the datatable #####
str(D)
D <- mutate(D,
                across(c(dir, cond, fog, rain, ID),factor),
            date=as.Date(date)
)

D$tempdiff <- 21-D$temp

# Removing outliers
D1 <- D[!(row.names(D) %in% c(3357, 3282, 3440, 8946)),]
# Creating a linear fit
slm1 <- lm(consumption ~ ID*tempdiff, data=D1)
par(mfrow=c(2,2))
plot(slm1)
