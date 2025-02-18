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
  mutate(weekend = ifelse(weekday %in% c("l�","s�"),"weekend","workingday"))

# getting start/end of month int (mutating the date variable)
D <- mutate(D, dag=str_split_fixed(date,"-",3)[ ,3])
D <- mutate(D, start_or_end = ifelse(as.integer(dag)<15, "START","END")) %>% 
  mutate(start_or_end = factor(start_or_end))

## Removing non important columns

# find the normalised data
mean_each <- group_by(D, ID) %>% 
  summarise(mean_each = mean(consumption))
D_with_mean <- inner_join(mean_each, D, "ID") 
D <- mutate(D_with_mean, ncons = consumption/mean_each)

#### Plots for descriptive analysis ####

## START/END IMPORTANT


# PLOT Tempdif and mean normalised consumption
# (both divided by their means)
# against date
cons_mean <- summarise(D, mean(ncons)) %>% as.double()
D_by_date <- group_by(D, date)
D_plot <- summarise(D_by_date, mncons = mean(ncons)/cons_mean)
temp_mean <- summarise(D, mean(tempdif)) %>% as.double()
D2_plot <- mutate(D, mtempdif = tempdif/temp_mean)

ggplot(D_plot) +
  geom_line(aes(x=date, y=mncons), col="Red", size=0.4) +
  geom_line(data = D2_plot, aes(x=date, y=mtempdif),size=0.4) +
  theme(legend.box.background = element_rect(color="red", size=2)+
  labs(x="Date", y="Normalized temperature difference/ mean normalised consumption \n (divided by their means)") +
  ggtitle("Tempdif and mean normalised consumption
(both divided by their means)
against date")



## Pairsplot ##
#plot(Dplot, col=Dplot$ID, main = "Pairsplot of data")

## Histogram of consumption ##
hist(Dplot$ncons, col="orange", border = "brown", main = "Normalized consumpution", xlab="Normalized consumption")

## Scatterplot ##
#It looks like end/start date has an effect
ggplot(D,aes(x=date, y=ncons,col=ID)) + 
  geom_point(size=0.8) + theme(legend.position = "none") +
  labs(y="Normalized consumption", x= "Date", title ="Date as a function of normalized consumption")



######## Dem her ##########
#nconc ~ tempdif
ggplot(D, aes(x=temp, y=ncons, col=ID))+
  geom_point(size=0.8)+ theme(legend.position = "none")+
  labs(y="Normalized consumption", x= "Temperature", title ="Normalized consumption as a function of temperature")

ggplot(D, aes(x=temp, y=consumption, col=ID))+
  geom_point(size=0.8)+ theme(legend.position = "none")+
  labs(y="Consumption", x= "Temperature", title ="Consumption as a function of temperature")

ggplot(filter(D,ID %in% c("78185925")),aes(x=temp, y=consumption,col=ID)) + 
  geom_point(size=0.8)+ 
  labs(y="Consumption", x= "Temperature", title ="Consumption as a function of temperature for building 78185925", legend= D$ID) + 
  geom_smooth(method=lm,alpha=0)

ggplot(D,aes(x=temp, y=dew_pt,col="black")) + 
  geom_point(size=1, col="black") + theme(legend.position = "none") +
  labs(y="Dew point", x= "Temperature", title ="Dew point as a function of temperature")        


###############

ggplot(D, aes(x=tempdif, y=ncons, col=ID))+
  geom_point(size=0.8)+ theme(legend.position = "none")+geom_smooth(method=lm, size=0.5, alpha=0)+
  labs(y="Normalized consumption", x= "Temperature difference", title ="NormalisedTemperature difference as a function of normalized consumption")


# But it is explained by the temperature fluxating. Maybe we are modelling the noise
ggplot(D,aes(x=date, y=tempdif,col="black")) + 
  geom_point(size=1, col="black") + theme(legend.position = "none") +
  labs(y="Temperature difference", x= "Date", title ="Date as a function of Temperature difference")

plot(Dplot$ncons ~ Dplot$ID, col=c(1:83), main="Boxplot of normalized consumption as a function of ID", ylab = "Normalized consumption", xlab="ID") 




# PLOT Tempdif and mean normalised consumption
# (both divided by their means)
# against date
cons_mean <- summarise(D, mean(ncons)) %>% as.double()
D_by_date <- group_by(D, date)
D_plot <- summarise(D_by_date, mncons = mean(ncons)/cons_mean)
temp_mean <- summarise(D, mean(tempdif)) %>% as.double()
D2_plot <- mutate(D, mtempdif = tempdif/temp_mean)

ggplot(D_plot) +
  geom_line(aes(x=date, y=mncons), col="Red", size=0.4) +
  geom_line(data = D2_plot, aes(x=date, y=mtempdif),size=0.4) +
  labs(x="Date", y="Normalized mean temperature difference/
       normalized consumption") +
  ggtitle("Normalized mean temperature difference and mean normalised 
          consumption as a function of date                                               = Normalized mean temperature difference
                                                                                                               = Normalized heat consumption")
