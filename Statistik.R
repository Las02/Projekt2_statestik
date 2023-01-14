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
#plot(D,col=D$ID,main="Pairsplot of dataframe")

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



# getting start/end_date int (mutating the date variable)
date <- mutate(D, dag=str_split_fixed(date,"-",3)[ ,3])
date <- mutate(date, start_or_end = ifelse(as.integer(dag)<15, "START","END")) %>% 
  mutate(start_or_end = factor(start_or_end))

# Trying one fit
#it <- lm(consumption~tempdif+ID+tempdif:start_or_end,date)
#Anova(fit)
#coplot(consumption ~ tempdif | start_or_end,date,col=date$ID)

# And another
#fit <- lm(consumption~tempdif+ID+tempdif:start_or_end +ID:tempdif:start_or_end,date)

#par(mfrow=c(2,2))
#plot(fit)

# Minimum model
mean_cons<- mean(D$consumption)
D <- mutate(date, std_cons=consumption/mean_cons)
D_no_clima <- select(D, ID,start_or_end, tempdif, std_cons)
fit <- lm(std_cons ~ .^4, D_no_clima)
drop1(fit,test="F")
new_fit <- step(fit, test="F",correlation = TRUE)
Anova(new_fit,correlation=TRUE)
drop1(new_fit)

# Looking for outliers
par(mfrow=c(2,2))
plot(new_fit)

# Removing outliers
remove <- c(3357,3282,3440)
D <- D[!(row.names(D) %in% remove ),]
D_no_clima<- D_no_clima[!(row.names(D_no_clima) %in% remove),]

fit <- lm(std_cons ~ (ID + start_or_end + tempdif)^4, D_no_clima)
par(mfrow=c(2,2))
plot(fit)
summary(fit)

# adding some clima
fit_clima <- step(update(fit, .~. + D$hum*D$wind_spd), test ="F")
anova(fit_clima, fit) # They are sig dif
AIC(fit_clima, fit) # fit_clima er bedre 
Anova(fit_clima)
plot(fit_clima)


########################################################
# Gammel stuff herfra
########################################################

# Adding MORE clima to the party
# with a maximum model
D_scope <- select(D, !c("date", "consumption","dag"))


fit_scope <- lm(std_cons~. ,D_scope)
new_fit <- step(fit_scope, scope = ~.^4 , k=log(nrow(D_scope)), test = "F")
#formula = std_cons ~ ID + hum + wind_spd + dir + vis + pressure + 
#  cond + tempdif + ID:tempdif + vis:tempdif + dir:pressure + 
#  dir:cond + pressure:cond + pressure:tempdif + hum:tempdif

# Makes no sense (but is significant)
f1 <- update(new_fit, .~. -hum:start_or_end)
Anova(new_fit)
f2 <- update(new_fit, .~. -tempdif:start_or_end )
Anova(new_fit)


summary(new_fit, correlation = T)

# dropping aliased coeff
nf <- update(.~.-dew_pt:tempdif,new_fit)



##########################################################ECTS

# Vi tager vis med da den er resultat af cond,fog og rain
par(mfrow=c(1,1))
Anova(lm(vis ~ cond + fog + rain,D))

f <- update(fit, .~. + D$wind_spd*D$dir)
Anova(f)

# Fog i not significant
f <- update(f, .~. + D$fog*D$wind_spd)
Anova(f)
f <- update(f, .~. -D$fog:D$wind_spd)
Anova(f)

# Rain
f <- update(f, .~. +D$rain)
Anova(f)

#
f <- update(f, .~. +D$vis)
Anova(f)

f <- update(f, .~. +D$rain*D$vis)
Anova(f)
