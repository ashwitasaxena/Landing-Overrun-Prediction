# importing data ----------------------------------------------------------

library(readxl)
FAA1<-read_xlsx("D:/MSBA/Spring Sem/statistical modeling/FAA1(1).xlsx",)
FAA2<-read_xlsx("D:/MSBA/Spring Sem/statistical modeling/FAA2(1).xlsx",)

#### structure
str(FAA1)
str(FAA2)

#### merging
library(dplyr)
FAA1_2 <- select(FAA1, aircraft, no_pasg, speed_ground, speed_air, height, pitch, distance) 

merged<- rbind(FAA1_2,FAA2)

sum(duplicated(merged))

new <- unique(merged)
summary(new)

#adding duration back in

final <- left_join(new, FAA1)

#### checking the final dataset
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

data <- clean   

data$aircraft <- as.factor(data$aircraft)          

str(data)
summary(data)
#standard deviation
sd_vector <- c("distance","duration","no_pasg", "height", "speed_ground", "speed_air", "pitch")
sapply(data[sd_vector], sd, na.rm=T)

hist(data$distance)
boxplot(data$distance)            

# Discretization of landing distance (creating multinomial variable )----------------------------------

for (i in 1:831) {  
  if (data$distance[i] < 1000) {
    data$distance[i] = 1
  } else if (data$distance[i] >= 1000 & data$distance[i] < 2500){
    data$distance[i] = 2
  } else data$distance[i] = 3
}

table(data$distance)

data$distance <- as.factor(data$distance)

names(data)[7] <- "Y"

str(data)

## visualization

par(mfrow=c(1,1))

library(ggplot2)
ggplot(data = data, aes(x = Y)) +
  geom_bar(stat ="count", fill = 'turquoise2')


# significance of individual models ---------------------------------------


### single variable models
library(nnet)

## speed_ground
model1 <- multinom (Y ~ speed_ground,data)
summary(model1)
sig_CI_1 <- c(summary(model1)$coefficients[,2]-1.96*(summary(model1)$standard.errors[,2]), summary(model1)$coefficients[,2]+1.96*(summary(model1)$standard.errors[,2]))
sig_CI_1

## height
model2 <- multinom (Y ~ height,data)
summary(model2)
sig_CI_2 <- c(summary(model2)$coefficients[,2]-1.96*(summary(model2)$standard.errors[,2]), summary(model2)$coefficients[,2]+1.96*(summary(model2)$standard.errors[,2]))
sig_CI_2

## duration
model3 <- multinom (Y ~ duration,data)
summary(model3)
sig_CI_3 <- c(summary(model3)$coefficients[,2]-1.96*(summary(model3)$standard.errors[,2]), summary(model3)$coefficients[,2]+1.96*(summary(model3)$standard.errors[,2]))
sig_CI_3

## pitch
model4 <- multinom (Y ~ pitch,data)
summary(model4)
sig_CI_4 <- c(summary(model4)$coefficients[,2]-1.96*(summary(model4)$standard.errors[,2]), summary(model4)$coefficients[,2]+1.96*(summary(model4)$standard.errors[,2]))
sig_CI_4

## aircraft
model5 <- multinom (Y ~ aircraft,data)
summary(model5)
sig_CI_5 <- c(summary(model5)$coefficients[,2]-1.96*(summary(model5)$standard.errors[,2]), summary(model5)$coefficients[,2]+1.96*(summary(model5)$standard.errors[,2]))
sig_CI_5

## speed air
model6 <- multinom (Y ~ speed_air,data)
summary(model6)
sig_CI_6 <- c(summary(model6)$coefficients[2]-1.96*(summary(model6)$standard.errors[2]), summary(model6)$coefficients[2]+1.96*(summary(model6)$standard.errors[2]))
sig_CI_6


## no_pasg
model7 <- multinom (Y ~ no_pasg,data)
summary(model7)
sig_CI_7 <- c(summary(model7)$coefficients[,2]-1.96*(summary(model7)$standard.errors[,2]), summary(model7)$coefficients[,2]+1.96*(summary(model7)$standard.errors[,2]))
sig_CI_7


# # finding z values through the resutl
# z <- summary(model1)$coefficients/summary(model1)$standard.errors
# # using 2 tailed wald test to find p value
# p <- (1 - pnorm(abs(z), 0, 1)) * 2


# for (i in c(1,2,3,4,5,6,8)){ 
#   model <- vglm (Y ~ data1[,i],
#                 family=cumulative(parallel=TRUE),
#                 data=data1) 
#   print(summary(model)) 
# } 



# create multivariate model manually -----------------------------------------------

model_manual <- multinom(Y ~ speed_ground + height + aircraft, data=data)
summary(model_manual)
sig_CI_manual <- data.frame(summary(model_manual)$coefficients[,2:4]-1.96*(summary(model_manual)$standard.errors[,2:4]), summary(model_manual)$coefficients[,2:4]+1.96*(summary(model_manual)$standard.errors[,2:4]))
sig_CI_manual

# create automated variable selected model --------------------------------

## model selection based on AIC
data_step <- data[,-c(4,8)]
null_model <- multinom(Y ~ 1,data = data_step)
full_model <- multinom(Y ~ .,data = data_step)

model_step <- step(object = null_model,scope = list(lower=null_model,upper=full_model),direction = 'forward',k = 2)
summary(model_step)
sig_CI_step <- data.frame(summary(model_step)$coefficients[,2:5]-1.96*(summary(model_step)$standard.errors[,2:5]), summary(model_step)$coefficients[,2:5]+1.96*(summary(model_step)$standard.errors[,2:5]))
sig_CI_step

## model comparison based on significance

deviance(model_step) - deviance(model_manual)
model_step$edf - model_manual$edf
pchisq(deviance(model_step)-deviance(model_manual),-model_manual$edf+model_step$edf,lower=F)


# Prediction -------------------------------------------------------------

xtabs(~predict(model_step)+data$Y)

(35+37+5+6)/(232+419+97+35+37+5+6)

# presentation ------------------------------------------------------------


ggplot(data <- data,aes(x=speed_ground,fill=factor(Y)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(Y)),alpha = 0.5)

ggplot(data <- data,aes(x=height,fill=factor(Y)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(Y)),alpha = 0.5)

plot(jitter(as.numeric(Y),0.1)~jitter(as.numeric(aircraft)),data,xlab = "Airbus vs Boeing",ylab="Y")



# Number of passengers ----------------------------------------------------

library(readxl)
FAA1<-read_xlsx("D:/MSBA/Spring Sem/statistical modeling/FAA1(1).xlsx",)
FAA2<-read_xlsx("D:/MSBA/Spring Sem/statistical modeling/FAA2(1).xlsx",)

#### structure
str(FAA1)
str(FAA2)

#### merging
library(dplyr)
FAA1_2 <- select(FAA1, aircraft, no_pasg, speed_ground, speed_air, height, pitch, distance) 

merged<- rbind(FAA1_2,FAA2)

sum(duplicated(merged))

new <- unique(merged)
summary(new)

#adding duration back in

final <- left_join(new, FAA1)

#### checking the final dataset
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

data <- clean   

###########################

hist(data$no_pasg)

#removing speed air due to multicollinearity and null observations
data <- select(data, -speed_air) %>% na.omit()

model_poisson <- glm(no_pasg ~ . , family=poisson, data)
summary(model_poisson)


## variable selection using stepwise AIC
step_model_poisson <- step(model_poisson)
summary(step_model_poisson)

mod1 <- glm(no_pasg~aircraft, family=poisson,data)
summary(mod1)
mod1 <- glm(no_pasg~speed_ground, family=poisson,data)
summary(mod1)
mod1 <- glm(no_pasg~duration, family=poisson,data)
summary(mod1)
mod1 <- glm(no_pasg~distance, family=poisson,data)
summary(mod1)
