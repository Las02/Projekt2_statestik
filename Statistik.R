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

D$Date <- as.POSIXct(D$date)
str(D)

# Adding to temp difference column to the dataframe 
D$tempdif <- 21 - D$temp


#### Creating initalizing plots ####
# Pairplot of all variables 
#plot(D,col=D$ID,main="Pairsplot of dataframe")

# Histogram of consumptions in general 
hist(D$consumption,col=D$ID)

par(mfrow=c(1,1))
# Consumption as a function of temp ( slopes seem to depend on Ua (isolation))
#plot(D$consumption ~ D$temp, col=D$ID, main = "Consumption as a function of tempature")
plot(D$consumption ~ D$tempdif, col=D$ID, main = "Consumption as a function of tempaturedifference")
plot(D$consumption ~ D$date, ylim=c(0,2), col=D$ID)
# Consumption as a function of hum 
#plot(D$consumption ~ D$hum, col=D$ID, main = "Consumption as a function of humidity")


# Box plot af consumption as a function of ID
#plot(D$consumption ~ D$ID, col=D$ID, main="Boxplot of consumption as a function of ID") 
#plot(D$consumption ~ D$ID, col=D$ID, main="Boxplot of consumption as a function of ID",ylim=c(0,1)) 

# Removing outliers
D1 <- D[!(row.names(D) %in% c(3357, 3282, 3440, 8946, 4682, 7112, 1460, 8266, 4375, 2533, 2535, 7478, 7178, 87, 1)),]




#### Model selection ####

par(mfrow=c(2,2))
lm_max <- lm(consumption ~ tempdif+ID+dew_pt+hum+wind_spd+pressure, data=D1)
plot(lm_max)

lm_max1 <- step((lm_max), scope = ~.^2, k=log(nrow(D1)), test="F")
summary(lm_max1)
Anova(lm_max1)
alias(lm_max)
plot(lm_max)
# We can't decide to remove anything from just those plots we will have to use a diagnostics plot

summary(D)








