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
D <- mutate(date, start_or_end = ifelse(as.integer(dag)<15, "START","END")) %>% 
  mutate(start_or_end = factor(start_or_end))

# Trying one fit
#it <- lm(consumption~tempdif+ID+tempdif:start_or_end,date)
#Anova(fit)
#coplot(consumption ~ tempdif | start_or_end,date,col=date$ID)

# And another
#fit <- lm(consumption~tempdif+ID+tempdif:start_or_end +ID:tempdif:start_or_end,date)

#par(mfrow=c(2,2))
#plot(fit)
mean_each <- group_by(D, ID) %>% 
  summarise(mean_each = mean(consumption))
D_with_mean <- inner_join(mean_each, D, "ID") 
D <- mutate(D_with_mean, ncons = consumption/mean_each)


# Minimum model
D_no_clima <- select(D, ID,start_or_end, tempdif, ncons)
fit <- lm(ncons ~ .^4, D_no_clima)
drop1(fit,test="F")
new_fit <- step(fit, test="F",correlation = TRUE, k=log(nrow(D)))
Anova(new_fit,correlation=TRUE)
drop1(new_fit)

# Looking for outliers
par(mfrow=c(2,2))
plot(new_fit)

# Removing outliers
remove <- c(9481,1622,1639)
D <- D[!(row.names(D) %in% remove ),]
D_no_clima<- D_no_clima[!(row.names(D_no_clima) %in% remove),]

fit <- lm(ncons ~ .^4, D_no_clima)
par(mfrow=c(2,2))
plot(fit)
summary(fit)

# adding some clima
fit_clima <- step(update(fit, .~.*D$hum*D$wind_spd), test ="F",k=log(nrow(D)))
anova(fit_clima, fit) # They are sig dif
AIC(fit_clima, fit) # fit_clima er bedre 
Anova(fit_clima)
plot(fit_clima)
drop1(fit_clima)

# Remove start_or_end:tempdif:D$hum , start_or_end:D$hum:D$wind_spd
f1 <- update(fit_clima, .~. -start_or_end:tempdif:D$hum -start_or_end:D$hum:D$wind_spd)
f2 <- step(f1, test="F", k=log(nrow(D)))

#Remove start_or_end:D$hum  Does not makes sense
f3 <- update(f2, .~. -start_or_end:D$hum)
drop1(f3, test ="F")
# Remove ID
f4 <- update(f3, .~. -ID)
# We cannot drop more
Anova(f4)
AIC(f4, fit_clima)
anova(f4, fit_clima)

# Trying model selection again without the ones which does not make sense
f5 <- step(update(fit, .~.*D$hum*D$wind_spd -start_or_end:tempdif:D$hum -start_or_end:D$hum:D$wind_spd -ID -start_or_end:D$hum) , test ="F")


# Adding MORE clima to the party
# with a maximum model
D_scope <- select(D, !c("date", "consumption","fog","rain","cond","dag", "vis", "dew_pt","dir"))





# Vi tager vis med da den er resultat af cond,fog og rain
par(mfrow=c(1,1))
Anova(lm(vis ~ cond + fog + rain,D))

fit_scope <- lm(ncons~. ,D_scope)
new_fit <- step(fit_scope, scope = ~.^3 , k=log(nrow(D_scope)), test = "F")
#lm(formula = ncons ~ ID + hum + wind_spd + pressure + tempdif + 
#start_or_end + ID:tempdif + tempdif:start_or_end + wind_spd:tempdif + 
#  hum:start_or_end + hum:pressure + wind_spd:pressure, data = D_scope)

# Makes no sense (but is significant)
f1 <- update(new_fit, .~. -hum:start_or_end)
Anova(new_fit)
f2 <- update(new_fit, .~. -tempdif:start_or_end )
Anova(new_fit)


summary(new_fit, correlation = T)

# dropping aliased coeff
nf <- update(.~.-dew_pt:tempdif,new_fit)


##########################################################ECTS


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

lm5 <-update(fit_clima,.~.-start_or_end:tempdif)


lm6 <- step(lm5,test="F",k=log(nrow(D)))
AIC(lm6,fit_clima)

anova(lm6,fit_clima)

