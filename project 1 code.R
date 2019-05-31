
# importing data ----------------------------------------------------------

library(readxl)
FAA1<-read_xlsx("D:/MSBA/Spring Sem/statistical modeling/FAA1(1).xlsx",)
FAA2<-read_xlsx("D:/MSBA/Spring Sem/statistical modeling/FAA2(1).xlsx",)

#### step 2: structure
str(FAA1)
str(FAA2)

#### step 3: merging
library(dplyr)
FAA1_2 <- select(FAA1, aircraft, no_pasg, speed_ground, speed_air, height, pitch, distance) 

merged<- rbind(FAA1_2,FAA2)

sum(duplicated(merged))

new <- unique(merged)
summary(new)

# adding duration back in

final <- left_join(new, FAA1)

#### step 4: checking the final dataset
str(final)
summary(final)
#standard deviation
sd_vector <- c("distance","duration","no_pasg", "height", "speed_ground", "speed_air", "pitch")
sapply(final[sd_vector], sd, na.rm=T)

# data cleaning -----------------------------------------------------------

a <- filter(final, speed_ground >= 30, speed_ground <= 140, height >= 6, distance <= 6000 )
#remove duration <40 but keep NAs
clean <- a[(a$duration >= 40 | is.na(a$duration)),]

summary(clean)

data <- filter (clean, distance > 140)           

####step 7
str(data)
summary(data)
#standard deviation
sd_vector <- c("distance","duration","no_pasg", "height", "speed_ground", "speed_air", "pitch")
sapply(data[sd_vector], sd, na.rm=T)

hist(data$distance)
boxplot(data$distance)            

# boxplot(data$distance)
# boxplot(data$duration)
# boxplot(data$pitch)
# boxplot(data$no_pasg)
# boxplot(data$speed_air)
# boxplot(data$speed_ground)
# boxplot(data$height)
 
#### histogram
par(mfrow=c(3,3))  
hist(data$distance)
hist(data$duration)
hist(data$pitch)
hist(data$no_pasg)
hist(data$speed_air)
hist(data$speed_ground)
hist(data$height)

             
### correlation
library (Hmisc)

# cordata <- select(data, distance, duration, no_pasg, speed_ground, speed_air, height, pitch)
# pairs(cordata)
# cor(cordata)
#### Correlation table
cor(data[,-1],use = "complete.obs")[,6]

#### scatter
par(mfrow=c(2,3))  
plot(data$duration, data$distance)
plot(data$pitch, data$distance)
plot(data$no_pasg,data$distance)
plot(data$speed_air,data$distance)
plot(data$speed_ground,data$distance)
plot(data$height,data$distance)

### dummies
library(fastDummies)
table(data$aircraft)
FAA_dummy <- data %>% 
  dummy_cols(select_columns = "aircraft")
table(FAA_dummy$aircraft_airbus)
table(FAA_dummy$aircraft_boeing)

#### regression
model1 <- lm( distance ~ duration, data = FAA_dummy)
summary(model1)
model2 <- lm( distance ~ no_pasg, data=FAA_dummy)
summary(model2)
model3 <- lm( distance ~ speed_air, data=FAA_dummy)
summary(model3)
model4 <- lm( distance ~ speed_ground, data=FAA_dummy)
summary(model4)
model5 <- lm( distance ~ pitch, data=FAA_dummy)
summary(model5)
model6 <- lm( distance ~ height, data=FAA_dummy)
summary(model6)
model7 <- lm( distance ~ aircraft_airbus, data=FAA_dummy)
summary(model7)
model8 <- lm( distance ~ aircraft_boeing, data=FAA_dummy)
summary(model8)


#### creating standardized variables

no_pasg1 <- {(FAA_dummy$no_pasg-mean(FAA_dummy$no_pasg))/sd(FAA_dummy$no_pasg)}
speed_ground1 <- {(FAA_dummy$speed_ground-mean(FAA_dummy$speed_ground))/sd(FAA_dummy$speed_ground)}
speed_air1 <- {(FAA_dummy$speed_air-mean(FAA_dummy$speed_air, na.rm=T))/sd(FAA_dummy$speed_air, na.rm=T)}
height1 <- {(FAA_dummy$height-mean(FAA_dummy$height))/sd(FAA_dummy$height)}
pitch1 <- {(FAA_dummy$pitch-mean(FAA_dummy$pitch))/sd(FAA_dummy$pitch)}
duration1 <- {(FAA_dummy$duration-mean(FAA_dummy$duration, na.rm=T))/sd(FAA_dummy$duration, na.rm=T)}

together<- cbind(FAA_dummy, no_pasg1,speed_ground1,speed_air1,height1,pitch1,duration1)
standard <- select(together, aircraft,no_pasg1,speed_ground1,speed_air1,height1,pitch1,distance, duration1, aircraft_boeing, aircraft_airbus)

summary(standard)

models1 <- lm( distance ~ duration1, data = standard)
summary(models1)
models2 <- lm( distance ~ no_pasg1, data=standard)
summary(models2)
models3 <- lm( distance ~ speed_air1, data=standard)
summary(models3)
models4 <- lm( distance ~ speed_ground1, data=standard)
summary(models4)
models5 <- lm( distance ~ pitch1, data=standard)
summary(models5)
models6 <- lm( distance ~ height1, data=standard)
summary(models6)
models7 <- lm( distance ~ aircraft_airbus, data=standard)
summary(models7)

cor(FAA_dummy$distance, FAA_dummy$aircraft_airbus)


####step 16

model_1 <- lm(distance~speed_ground, data=FAA_dummy)
summary(model_1)
model_2 <- lm(distance~speed_air, data=FAA_dummy)
summary(model_2)
model_3 <- lm(distance~speed_ground + speed_air, data=FAA_dummy)
summary(model_3)

cor(FAA_dummy$speed_ground,FAA_dummy$speed_air,use = "complete.obs")

#### step 17

mod1 <- lm(distance ~ speed_ground, data=FAA_dummy)
r1 <- summary(mod1)$r.squared
mod2 <- lm(distance ~ speed_ground + aircraft_airbus, data=FAA_dummy )
r2 <- summary(mod2)$r.squared
mod3 <- lm(distance ~ speed_ground + aircraft_airbus + height, data=FAA_dummy )
r3 <- summary(mod3)$r.squared
mod4 <- lm(distance ~ speed_ground + aircraft_airbus + height + pitch, data=FAA_dummy )
r4 <- summary(mod4)$r.squared
mod5 <- lm(distance ~ speed_ground + aircraft_airbus + height + pitch + duration, data=FAA_dummy )
r5 <- summary(mod5)$r.squared
mod6 <- lm(distance ~ speed_ground + aircraft_airbus + height + pitch + duration + no_pasg, data=FAA_dummy )
r6 <- summary(mod6)$r.squared

rsquared <- c(r1,r2,r3,r4,r5,r6)
p <- c(1,2,3,4,5,6)
par(mfrow=c(1,1))
plot(p, rsquared)
lines.default(x=p, y=rsquared)


##### adjusted r squared
adj1 <- summary(mod1)$adj.r.squared
adj2 <- summary(mod2)$adj.r.squared
adj3 <- summary(mod3)$adj.r.squared
adj4 <- summary(mod4)$adj.r.squared
adj5 <- summary(mod5)$adj.r.squared
adj6 <- summary(mod6)$adj.r.squared

adj_rsquared <- c(adj1,adj2,adj3,adj4,adj5,adj6)
plot(p,adj_rsquared)
lines.default(x=p, y=adj_rsquared)

r_diff <- cbind(rsquared,adj_rsquared)
print(r_diff)

#### AIC 
a1 <- AIC(mod1)
a2 <- AIC(mod2)
a3 <- AIC(mod3)
a4 <- AIC(mod4)
a5 <- AIC(mod5)
a6 <- AIC(mod6)
aic <- c(a1,a2,a3,a4,a5,a6)
plot(p, aic)
lines.default(x=p, y=aic)

allmodels <- cbind(adj_rsquared,aic)
print(allmodels)

####21 step AIC
model_algorithm <- lm (distance ~.,data = FAA_dummy[,-c(1,9)])
stepAIC(model_algorithm)
