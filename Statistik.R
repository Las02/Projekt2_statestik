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

as.POSIXct(D$date)
str(D)

# Adding to temp difference column to the dataframe 
D$tempdif <- 21 - D$temp


#### Creating initalizing plots ####
# Pairplot of all variables 
plot(D,col=D$ID,main="Pairsplot of dataframe")

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


#diagnostics plot


#### Analysis Model ####

# Simple model



#### Model selection ####
# Removing outliers
D1 <- D[!(row.names(D) %in% c(3357, 3282, 3440, 8946, 4682, 7112, 1460, 8266, 4375, 2533, 2535, 7478, 7178, 87, 1)),]


#Our maximum model
#lm_max <- step(lm(consumption ~ tempdiff*ID*date, data=D1), k=log(nrow(D1)), test="F")
#lm_max <- step(lm(consumption ~ tempdiff*ID*date*dew_pt, data=D1), k=log(nrow(D1)), test="F")
#lm_max <- step(lm(consumption ~ tempdiff*ID*date*dew_pt*hum, data=D1), k=log(nrow(D1)), test="F")

par(mfrow=c(2,2))
lm_max <- lm(consumption ~ tempdif+ID+dew_pt+hum+wind_spd+pressure+date, data=D1)


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


# getting start/end_date int (mutating the date variable)
date <- mutate(D, dag=str_split_fixed(date,"-",3)[ ,3])
date <- mutate(date, start_or_end = ifelse(as.integer(dag)<15, "START","END")) %>% 
  mutate(start_or_end = factor(start_or_end))
f
# Trying one fit
it <- lm(consumption~tempdif+ID+tempdif:start_or_end,date)
Anova(fit)
coplot(consumption ~ tempdif | start_or_end,date,col=date$ID)

# And another
fit <- lm(consumption~tempdif+ID+tempdif:start_or_end +ID:tempdif:start_or_end,date)
Anova(fit)
par(mfrow=c(2,2))
plot(fit)

# Minimum model
mean_cons<- mean(D$consumption)
D <- mutate(date, std_cons=consumption/mean_cons)
D_no_clima <- select(D, ID,start_or_end, tempdif, consumption)
fit <- lm(consumption ~ .^4, D_no_clima)
drop1(fit,test="F")
new_fit <- step(fit, test="F",correlation = TRUE)
Anova(new_fit,correlation=TRUE)


