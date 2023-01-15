#### Reading in the libaries ####
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(car)
library(stringr)
library(xtable)

#### Loading the data ####
D <- read.csv("merged_data.csv", header=TRUE)
str(D)
D <- mutate(D,
            across(c(dir, cond, fog, rain, ID),factor)
)
D$date <- as.POSIXct(D$date, tz = "UTC")
str(D)
D$tempdif <- 21 - D$temp
D <- select(D,!c("temp"))
date <- mutate(D, dag=str_split_fixed(date,"-",3)[ ,3])
date <- mutate(date, start_or_end = ifelse(as.integer(dag)<15, "START","END")) %>% 
  mutate(start_or_end = factor(start_or_end))
mean_cons<- mean(D$consumption)
D <- mutate(date, std_cons=consumption/mean_cons)
remove <- c(3357,3282,3440)
D <- D[!(row.names(D) %in% remove ),]

#### Finding the standard deviation of the parameters ####
fit <- lm(std_cons ~ ID + start_or_end + tempdif + D$hum + 
     D$wind_spd + ID:start_or_end + ID:tempdif + start_or_end:tempdif + 
     D$hum:D$wind_spd + ID:start_or_end:tempdif, data = D)

### Intercepts_model
##start_or_end
#1 OR 2
# 2 of them, .1 in intercept 
## ID
#83 different -1 in in intercept
## ID:start_or_end
# + 82 ekstra en for hver ID:START
# Therefore in total 83-1 + 2-1 + 82 = 165
# And then +1 for intercept = 166
A <- cbind(diag(165))
# Then setting intercept
A[,1] <- rep(1,length(A[,1]))
A





### Slopes model
##tempdif
#humidity
#wind_spd
#
#ID:tempdif  <- for tempdif
#
#

ggplot(D, aes(x=date, y=std_cons, col=ID)) +
  geom_point() +
  theme(legend.position="none")
