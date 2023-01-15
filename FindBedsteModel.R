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


#### Analyzing the different models NO CLIMA MODELS ####
## Basic
D_basic <- select(D,ID, ncons, tempdif)
fit_basic <- lm(ncons~. ,D_basic)
f_basic <- step(fit_basic, scope = ~.^3 , k=log(nrow(D_basic)), test = "F")
AIC(fit_basic)
# lm(formula = ncons ~ ID + tempdif + ID:tempdif, data = D_basic)

## +Start or end
D_startend <- select(D,ID, ncons, tempdif,  start_or_end)
fit_startend <- lm(ncons~. ,D_startend)
f_startend <- step(fit_startend, scope = ~.^3 , k=log(nrow(D_startend)), test = "F")
AIC(f_startend)
# lm(formula = ncons ~ ID + tempdif + start_or_end + ID:tempdif + 
#      tempdif:start_or_end, data = D_startend)

## +weekday
D_weekday <- select(D,ID, ncons, tempdif, weekend)
fit_weekday <- lm(ncons~. ,D_weekday)
f_weekday <- step(fit_weekday, scope = ~.^3 , k=log(nrow(D_weekday)), test = "F")
#lm(formula = ncons ~ ID + tempdif + weekend + ID:tempdif + tempdif:weekend, 
#   data = D_weekday)

## +weekday +startend
D_weekday_startend <- select(D,ID, ncons, tempdif, weekend, start_or_end)
fit_weekday_startend <- lm(ncons~. ,D_weekday_startend)
f_weekday_startend <- step(fit_weekday_startend, scope = ~.^3 , k=log(nrow(D_weekday_startend)), test = "F")
# lm(formula = ncons ~ ID + tempdif + weekend + start_or_end + 
#      ID:tempdif + tempdif:weekend + tempdif:start_or_end + weekend:start_or_end + 
#      tempdif:weekend:start_or_end, data = D_weekday_startend


## Comparing the 4
AIC(fit_basic, f_startend, f_weekday,f_weekday_startend)
# They are sig different
anova(f_weekday, f_weekday_startend)

## Results
# weekday + startend is the best model
# but startend does not make sense
# so we are going with second best
# ncons ~ ID + tempdif + weekend + ID:tempdif + tempdif:weekend

#### Analyzing the different models WITH CLIMA THIRD ORDER ####

D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure, start_or_end)
fit_scope <- lm(ncons~. ,D_scope)
f1 <- step(fit_scope, scope = ~.^3 , k=log(nrow(D_scope)), test = "F")
#lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
#pressure + start_or_end + ID:tempdif + tempdif:dew_pt + dew_pt:start_or_end + 
#  hum:pressure + tempdif:wind_spd + wind_spd:pressure + hum:start_or_end + 
#  hum:dew_pt + tempdif:hum + tempdif:start_or_end + tempdif:hum:dew_pt + 
#  hum:dew_pt:start_or_end + tempdif:hum:start_or_end, data = D_scope)

D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure, weekend)
fit_scope <- lm(ncons~. ,D_scope)
f2 <- step(fit_scope, scope = ~.^3 , k=log(nrow(D_scope)), test = "F")
# lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
# pressure + weekend + ID:tempdif + tempdif:weekend + wind_spd:hum + 
#   tempdif:dew_pt + wind_spd:dew_pt + tempdif:hum + hum:dew_pt + 
#   ID:weekend + tempdif:pressure + wind_spd:weekend + dew_pt:weekend + 
#   hum:weekend + tempdif:hum:dew_pt + wind_spd:hum:weekend + 
#   tempdif:hum:weekend + hum:dew_pt:weekend, data = D_scope)


D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure)
fit_scope <- lm(ncons~. ,D_scope)
f3 <- step(fit_scope, scope = ~.^3 , k=log(nrow(D_scope)), test = "F")
# lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
#      pressure + ID:tempdif + tempdif:wind_spd + tempdif:dew_pt + 
#      hum:pressure + wind_spd:pressure + tempdif:pressure + dew_pt:pressure + 
#      tempdif:wind_spd:pressure + tempdif:dew_pt:pressure, data = D_scope)

D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure, start_or_end,weekend)
fit_scope <- lm(ncons~. ,D_scope)
f4 <- step(fit_scope, scope = ~.^3 , k=log(nrow(D_scope)), test = "F")
# lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
#      pressure + start_or_end + weekend + ID:tempdif + tempdif:weekend + 
#      wind_spd:hum + tempdif:dew_pt + dew_pt:start_or_end + pressure:start_or_end + 
#      tempdif:pressure + wind_spd:weekend + dew_pt:weekend + hum:weekend + 
#      ID:weekend + tempdif:hum + hum:dew_pt + wind_spd:dew_pt + 
#      pressure:weekend + wind_spd:hum:weekend + tempdif:hum:dew_pt + 
#      tempdif:hum:weekend + hum:dew_pt:weekend, data = D_scope)

AIC(f1,f2,f3,f4)

## RESULTS
# The AIC values are high
# but a lot of the interactions does not make sense for the eg. hum:dew_pt:weekend

#### Analyzing the different models WITH CLIMA SECOND ORDER ####
D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure, start_or_end)
fit_scope <- lm(ncons~. ,D_scope)
f5 <- step(fit_scope, scope = ~.^2 , k=log(nrow(D_scope)), test = "F")
# lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
#      pressure + start_or_end + ID:tempdif + tempdif:dew_pt + dew_pt:start_or_end + 
#      hum:pressure + tempdif:wind_spd + wind_spd:pressure + hum:start_or_end + 
#      hum:dew_pt + tempdif:hum + wind_spd:dew_pt, data = D_scope)

D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure, weekend)
fit_scope <- lm(ncons~. ,D_scope)
f6 <- step(fit_scope, scope = ~.^2 , k=log(nrow(D_scope)), test = "F")
# lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
#      pressure + weekend + ID:tempdif + tempdif:weekend + wind_spd:hum + 
#      tempdif:dew_pt + wind_spd:dew_pt + tempdif:hum + hum:dew_pt + 
#      ID:weekend + dew_pt:weekend + hum:weekend + dew_pt:pressure, 
#    data = D_scope)

D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure)
fit_scope <- lm(ncons~. ,D_scope)
f7 <- step(fit_scope, scope = ~.^2 , k=log(nrow(D_scope)), test = "F")
# lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
#      pressure + ID:tempdif + tempdif:wind_spd + tempdif:dew_pt + 
#      hum:pressure + tempdif:pressure + dew_pt:pressure, data = D_scope)

D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, dew_pt, pressure, start_or_end,weekend)
fit_scope <- lm(ncons~. ,D_scope)
f8 <- step(fit_scope, scope = ~.^2 , k=log(nrow(D_scope)), test = "F")
# lm(formula = ncons ~ ID + tempdif + wind_spd + hum + dew_pt + 
#      pressure + start_or_end + weekend + ID:tempdif + tempdif:weekend + 
#      wind_spd:hum + tempdif:dew_pt + dew_pt:start_or_end + pressure:start_or_end + 
#      tempdif:pressure + wind_spd:weekend + dew_pt:weekend + hum:weekend + 
#      ID:weekend + tempdif:hum + hum:dew_pt + tempdif:wind_spd, 
#    data = D_scope)

# Trying without dew_pt. But the model is better with it
D_scope <- select(D, ID, ncons, tempdif, wind_spd, hum, pressure, weekend)
fit_scope <- lm(ncons~. ,D_scope)
f9 <- step(fit_scope, scope = ~.^2 , k=log(nrow(D_scope)), test = "F")
#lm(formula = ncons ~ ID + tempdif + wind_spd + hum + pressure + 
#   weekend + ID:tempdif + tempdif:weekend + tempdif:hum + tempdif:wind_spd + 
#   hum:pressure + wind_spd:weekend + wind_spd:hum + ID:weekend, 
# data = D_scope)

AIC(f5,f6,f7,f8,f9)
### RESULTS
# The one with both start or end + weekend is again the best
# with the one with only weekend coming in second
# Sorting after AIC
arrange(as.data.frame(AIC(f1,f2,f3,f4,f5,f6,f7,f8,f9)), AIC)
#f4 < f2 < f8 < f6 < f9 < f1 < f5 < f3 < f7
# f5 - f9 are with second order interactions
# f1 - f4 are with third order interactions

models <- AIC(fit_basic, f_startend, f_weekday,f_weekday_startend,f1,f2,f3,f4,f5,f6,f7,f8)
models <- models %>% 
  as.data.frame() 
models$max_order <- c(rep(3,8), rep(2,4))
models$type <- c("Basic","start_end","weekend","weekend+start_end","start_end","weekend","basic","weekend+start_end","start_end","weekend","basic","weekend+start_end")
models <- arrange(models, AIC) %>% 
  select(AIC, type, max_order, df)
xtable(models)


#### Important plots ####

## START/END IMPORTANT
Dplot <- select(D,!c("date", "consumption"))

str(Dplot)
## Pairsplot ##
plot(Dplot, col=Dplot$ID, main = "Pairsplot of data")

## Histogram of consumption ##
hist(Dplot$ncons, col="orange", border = "brown", main = "Normalized consumpution", xlab="Normalized consumption")

## Scatterplot ##
#It looks like end/start date has an effect
ggplot(D,aes(x=date, y=ncons,col=ID)) + 
  geom_point(size=0.8) + theme(legend.position = "none") +
  labs(y="Normalized consumption", x= "Date", title ="Date as a function of normalized consumption")

#nconc ~ tempdif
ggplot(D, aes(x=tempdif, y=ncons, col=ID))+
  geom_point(size=0.8)+ theme(legend.position = "none")+
  labs(y="Normalized consumption", x= "Temperature difference", title ="Temperature difference as a function of normalized consumption")


ggplot(D, aes(x=tempdif, y=ncons, col=ID))+
  geom_point(size=0.8)+ theme(legend.position = "none")+geom_smooth(method=lm)+
  labs(y="Normalized consumption", x= "Temperature difference", title ="Temperature difference as a function of normalized consumption")


# But it is explained by the temperature fluxating. Maybe we are modelling the noise
ggplot(D,aes(x=date, y=tempdif,col="black")) + 
  geom_point(size=1, col="black") + theme(legend.position = "none") +
  labs(y="Temperature difference", x= "Date", title ="Date as a function of Temperature difference")

plot(Dplot$ncons ~ Dplot$ID, col=c(1:83), main="Boxplot of normalized consumption as a function of ID", ylab = "Normalized consumption", xlab="ID") 

