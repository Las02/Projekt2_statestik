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

#### Creating initalizing plots ####
plot(D)

ggplot(D, aes(x=date, y=temp, col=ID)) + 
  geom_point(size=1)+
  geom_smooth(method=lm,alpha=0.2, size=0.8) 

ggplot(D,aes(x=date, y=temp)) +
  geom_boxplot() + ggtitle("Temp")

