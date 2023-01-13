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

as.POSIXct(D$date, tz = "UTC")
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


#diagnostics plot

D2 <- D[!D$ID %in% c(78185925),]

# Histogram of consumptions in general 
#hist(D2$consumption,col=D2$ID)

# Consumption as a function of temp ( slopes seem to depend on Ua (isolation))
#plot(D2$consumption ~ D2$temp, col=D2$ID, main = "Consumption as a function of tempature")
plot(D2$consumption ~ D2$tempdif, col=D2$ID, main = "Consumption as a function of tempaturedifference")

# Consumption as a function of hum 
#plot(D2$consumption ~ D2$hum, col=D2$ID, main = "Consumption as a function of humidity")


# Box plot af consumption as a function of ID
#plot(D2$consumption ~ D2$ID, col=D2$ID, main="Boxplot of consumption as a function of ID") 
#plot(D2$consumption ~ D2$ID, col=D2$ID, main="Boxplot of consumption as a function of ID",ylim=c(0,1)) 





D3 <- D[!D$ID %in% c(78185925,69429582),]
#hist(D3$consumption,col=D2$ID)

# Consumption as a function of temp ( slopes seem to depend on Ua (isolation))
#plot(D3$consumption ~ D3$temp, col=D3$ID, main = "Consumption as a function of tempature")
plot(D3$consumption ~ D3$tempdif, col=D3$ID, main = "Consumption as a function of tempaturedifference")

# Consumption as a function of hum 
#plot(D3$consumption ~ D3$hum, col=D3$ID, main = "Consumption as a function of humidity")


# Box plot af consumption as a function of ID
#plot(D3$consumption ~ D3$ID, col=D3$ID, main="Boxplot of consumption as a function of ID") 
#plot(D3$consumption ~ D3$ID, col=D3$ID, main="Boxplot of consumption as a function of ID",ylim=c(0,1)) 



D34 <- D[!D$ID %in% c(78185925,69585544),]

#hist(D34$consumption,col=D2$ID)

# Consumption as a function of temp ( slopes seem to depend on Ua (isolation))
#plot(D34$consumption ~ D34$temp, col=D34$ID, main = "Consumption as a function of tempature")
plot(D34$consumption ~ D34$tempdif, col=D34$ID, main = "Consumption as a function of tempaturedifference")

# Consumption as a function of hum 
#plot(D34$consumption ~ D34$hum, col=D34$ID, main = "Consumption as a function of humidity")


# Box plot af consumption as a function of ID
#plot(D34$consumption ~ D34$ID, col=D34$ID, main="Boxplot of consumption as a function of ID") 
#plot(D34$consumption ~ D34$ID, col=D34$ID, main="Boxplot of consumption as a function of ID",ylim=c(0,1)) 



D4 <- D[!D$ID %in% c(78185925,69429582,69585544),]
#hist(D4$consumption,col=D2$ID)

# Consumption as a function of temp ( slopes seem to depend on Ua (isolation))
#plot(D4$consumption ~ D4$temp, col=D4$ID, main = "Consumption as a function of tempature")
plot(D4$consumption ~ D4$tempdif, col=D4$ID, main = "Consumption as a function of tempaturedifference")

# Consumption as a function of hum 
#plot(D4$consumption ~ D4$hum, col=D4$ID, main = "Consumption as a function of humidity")


# Box plot af consumption as a function of ID
#plot(D4$consumption ~ D4$ID, col=D4$ID, main="Boxplot of consumption as a function of ID") 
#plot(D4$consumption ~ D4$ID, col=D4$ID, main="Boxplot of consumption as a function of ID",ylim=c(0,1)) 


 

#### Analysis Model ####

# Simple model

# for dataframe 0
lm1 <- step(lm(consumption ~ ID*tempdif, data= D),test="F", k=log(nrow(D)))
lm2 <- lm(consumption ~ ID*tempdif, data= D)
AIC(lm1, lm2)
par(mfrow=c(2,2))
plot(lm1)
Anova(lm1)

# Dataframe 3
lm3 <- step(lm(consumption ~ ID*tempdif, data= D3),test="F", k=log(nrow(D3)))
par(mfrow=c(2,2))
plot(lm3)
Anova(lm3)

# for dataframe 4
lm4 <- step(lm(consumption ~ ID*tempdif, data= D4),test="F", k=log(nrow(D4)))
par(mfrow=c(2,2))
plot(lm4)
Anova(lm4)



#### Model selection ####
#Our maximum model
#lm_max <- step(lm(consumption ~ tempdiff*ID*date, data=D1), k=log(nrow(D1)), test="F")
#lm_max <- step(lm(consumption ~ tempdiff*ID*date*dew_pt, data=D1), k=log(nrow(D1)), test="F")
#lm_max <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum, data=D1), k=log(nrow(D1)), test="F")

par(mfrow=c(2,2))
lm_max <- lm(consumption ~ tempdif+ID+dew_pt+hum+wind_spd+pressure, data=D1)

par(mfrow=c(1,1))
lm3 <- lm(consumption ~ temp+ID, data=D)
Anova(lm3)
AIC(lm3)
plot(D$consumption~D$tempdif, col=D$ID)

lm_max1 <- step(lm(consumption ~ temp+dew_pt+wind_spd+hum+pressure+ID+date, data=D1), scope = ~.^2, k=log(nrow(D1)), test="F")
summary(lm_max)
Anova(lm_max1)
alias(lm_max)
plot(lm_max)
# We can't decide to remove anything from just those plots we will have to use a diagnostics plot

summary(D)








