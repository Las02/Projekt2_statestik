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
                across(c(dir, cond, fog, rain, ID),factor)
)

D$tempdiff <- 21-D$temp

# Removing outliers
D1 <- D[!(row.names(D) %in% c(3357, 3282, 3440, 8946, 4682, 7112, 1460, 8266, 4375, 2533, 2535)),]
# Creating a linear fit
lm1 <- step(lm(consumption ~ tempdiff*ID, data=D), k=log(nrow(D)), test="F")
#lm2 <- step(lm(consumption ~ tempdiff*ID*date, data=D1), k=log(nrow(D1)), test="F")
#lm3 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt, data=D1), k=log(nrow(D1)), test="F")
#lm4 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum, data=D1), k=log(nrow(D1)), test="F")
#lm5 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum*wind_spd, data=D1), k=log(nrow(D1)), test="F")
#lm6 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum*wind_spd*dir, data=D1), k=log(nrow(D1)), test="F")
#lm7 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum*wind_spd*dir*vis*pressure, data=D1), k=log(nrow(D1)), test="F")
#lm8 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum*wind_spd*dir*vis*pressure*cond, data=D1), k=log(nrow(D1)), test="F")
#lm9 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum*wind_spd*dir*vis*pressure*cond*fog, data=D1), k=log(nrow(D1)), test="F")
#lm9 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum*wind_spd*dir*vis*pressure*cond*fog*rain, data=D1), k=log(nrow(D1)), test="F")



#Our maximum model
#lm9 <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum*wind_spd*pressure*rain, data=D1), k=log(nrow(D1)), test="F")
