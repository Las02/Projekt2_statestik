

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