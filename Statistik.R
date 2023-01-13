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

D$date <- as.POSIXct(D$date, tz = "UTC")
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
plot(D$tempdif ~ D$date)
plot(D$consumption ~ D$date, col=D$ID)

ggplot(D,aes(x=date, y=consumption,col=ID)) + 
  geom_point(size=0.8) + theme(legend.position = "none") 
# shows date needs to be factorized in some sort of way


# Consumption as a function of hum 
#plot(D$consumption ~ D$hum, col=D$ID, main = "Consumption as a function of humidity")


# Box plot af consumption as a function of ID
#plot(D$consumption ~ D$ID, col=D$ID, main="Boxplot of consumption as a function of ID") 
#plot(D$consumption ~ D$ID, col=D$ID, main="Boxplot of consumption as a function of ID",ylim=c(0,1)) 

D <- select(D,!c("temp"))


#### Analysis Model ####
par(mfrow=c(1,1))
lm1 <- lm(consumption ~ . - date - dir - vis -cond - fog -rain, data=D )
plot(lm1) # looks not very good 

D1 <- D[!(row.names(D) %in% c(3357,3282,7178,8829)),]

lm2 <- lm(consumption ~ . - date - dir - vis -cond - fog -rain, data=D1 )
plot(lm2) # looks not very good 

D2 <- D1[!(row.names(D1) %in% c(3357,3282,7178,8829,9440,7082,7112,7081,8859, 9453, 4, 97)),]
lm3 <- lm(consumption ~ . - date - dir - vis -cond - fog -rain, data=D2 )
plot(lm3) 

lm4 <- lm(consumption ~ . - date - dir - vis -cond - fog -rain, data=D2 )
plot(lm4) 

lm4_step <- step(lm4, scope = ~.^2, k=log(nrow(D1)), test="F")
Anova(lm4_step)

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


