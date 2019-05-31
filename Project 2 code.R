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

data <- clean   

data$aircraft <- as.factor(data$aircraft)


# Project part 2 ----------------------------------------------------------

##########
# step 1 #
##########

data$long.landing = 0

for (i in 1:831) {
  if (data$distance[i] > 2500) data$long.landing[i] = 1
}
sum(data$long.landing)

data$risky.landing = 0 

for (i in 1:831) {
  if (data$distance[i] > 3000) data$risky.landing[i] = 1
}
sum(data$risky.landing)

data <- data[,(-7)]

str(data)


##########
# step 2 #
##########
par(mfrow=c(1,1))  
hist(data$long.landing,ylim = range(0,800))

##########
# step 3 #
##########

data1 <- as.data.frame(data)
for (i in c(1:7)){ 
  model <- glm (long.landing ~ data1[,i],
                family = binomial(link='logit'),
                data=data1) 
  print(summary(model)) 
} 


##########
# step 4 #
##########
# install.packages("PerformanceAnalytics")
# library("PerformanceAnalytics")
# my_data <- data[, c(2:8)]
# chart.Correlation(my_data, histogram=TRUE, pch=19)
library(ggplot2)
plot(data$long.landing ~ data$speed_ground)
plot(jitter(long.landing,0.1)~jitter(speed_ground),data,xlab="speed ground",ylab="long landing")
ggplot(data <- data,aes(x=speed_ground,fill=factor(long.landing)))+
  geom_histogram(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(long.landing)),alpha = 0.5)

plot(data$long.landing ~ data$speed_air)
plot(jitter(long.landing,0.1)~jitter(speed_air),data,xlab="speed air",ylab="long landing")
ggplot(data <- data,aes(x=speed_air,fill=factor(long.landing)))+
  geom_histogram(position="dodge",binwidth=5,aes(y=..density..,
                                                 colour=factor(long.landing)),alpha = 0.5)

plot(jitter(long.landing,0.1)~jitter(as.numeric(aircraft)),data,xlab="aircraft Boeing",ylab="long landing")

plot(data$long.landing ~ data$pitch)
plot(jitter(long.landing,0.1)~jitter(pitch),data,xlab="pitch",ylab="long landing")
ggplot(data <- data,aes(x=pitch,fill=factor(long.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                                 colour=factor(long.landing)),alpha = 0.5)

##########
# step 5 #
##########

step5 <- glm(long.landing ~ speed_ground + aircraft+ pitch, data = data, family=binomial(link='logit'))
summary(step5)

##########
# step 6 #
##########

nullmodel <- glm(long.landing ~ 1, data = data, family=binomial(link='logit'))
fullmodel <- glm(long.landing ~ speed_ground + pitch+ height + no_pasg+ duration + aircraft, data=data, family= binomial(link = 'logit'))

#forward selection
model_step_f <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward')
summary(model_step_f)
BIC(model_step_f)


##########
# step 7 #
##########

model_bic <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward', k=log(nrow(data)))
summary(model_bic)
BIC(model_bic)

##########
# step 8 #
##########





##########
# step 9 #
##########

par(mfrow=c(1,1))  
hist(data$risky.landing,ylim = range(0,800))

data1 <- as.data.frame(data)
for (i in c(1:7)){ 
  model <- glm (risky.landing ~ data1[,i],
                family = binomial(link='logit'),
                data=data1) 
  print(summary(model)) 
} 

t(names(data1))
data1$aircraft <- as.factor(data1$aircraft)
var_name <- rep('',7)
coeff <- rep(0,7)
odds_ratio <- rep(0,7)
direction <- rep('+',7)
p_val <- rep(0,7)
j <- 1

for(i in c(1,2,3,4,5,6,7)) {
  fit <- glm(risky.landing ~ data1[,i],family=binomial(link='logit'),data=data1)
  var_name[j] <- names(data1)[i]
  coeff[j] <- abs(summary(fit)$coefficients[2,1])
  odds_ratio[j] <- exp(fit$coefficients[2])
  if(summary(fit)$coefficients[2,1] < 0) {direction[j] <- '-'}  
  p_val[j] <- summary(fit)$coefficients[2,4]
  j <- j+1
}
tt <- cbind(1:7,var_name,coeff,odds_ratio,direction,p_val)

### ggpubr package to arrange plots in the same place

plot(jitter(risky.landing,0.1)~jitter(speed_ground),data,xlab="speed ground",ylab="risky landing")
ggplot(data <- data,aes(x=speed_ground,fill=factor(risky.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                                 colour=factor(risky.landing)),alpha = 0.5)

plot(jitter(risky.landing,0.1)~jitter(speed_air),data,xlab="speed air",ylab="risky landing")
ggplot(data <- data,aes(x=speed_air,fill=factor(risky.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                                 colour=factor(risky.landing)),alpha = 0.5)

plot(jitter(risky.landing,0.1)~jitter(as.numeric(aircraft)),data,xlab="aircraft Boeing",ylab="risky landing")

# important variable model
step5_r <- glm(risky.landing ~ speed_ground + aircraft, data = data, family=binomial(link='logit'))
summary(step5_r)

nullmodel_r <- glm(risky.landing ~ 1, data = data, family=binomial(link='logit'))
fullmodel_r <- glm(risky.landing ~ speed_ground + pitch+ height + no_pasg+ duration + aircraft, data=data, family= binomial(link = 'logit'))

#forward AIC
model_step_f_r <- step(nullmodel_r, scope=list(lower=nullmodel_r, upper=fullmodel_r), direction='forward')
summary(model_step_f_r)
BIC(model_step_f_r)

# forward BIC
model_bic_r <- step(nullmodel_r, scope=list(lower=nullmodel_r, upper=fullmodel_r), direction='forward', k=log(nrow(data1)))
summary(model_bic_r)
BIC(model_bic_r)



###########
# step 12 #
###########

## Long landing model
pred <- ifelse(predict(model_bic,type = 'response') < 0.5,0,1)
pred_r <- ifelse(predict(model_bic_r,type = 'response') < 0.5,0,1)

thresh <- seq(0.01,0.5,0.01)
sensitivity <- specificity <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(model_bic,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~data1$long.landing+pp)
  specificity[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}
par(mfrow=c(1,2))
matplot(thresh,cbind(sensitivity,specificity),type="l",xlab="Thr
        eshold",ylab="Proportion",lty=1:2)
plot(1-specificity,sensitivity,type="l");abline(0,1,lty=2)

### risky landing model

pred <- ifelse(predict(model_bic,type = 'response') < 0.5,0,1)
pred_r <- ifelse(predict(model_bic_r,type = 'response') < 0.5,0,1)

thresh <- seq(0.01,0.5,0.01)
sensitivity_r <- specificity_r <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(model_bic_r,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~data1$risky.landing+pp)
  specificity_r[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity_r[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}
par(mfrow=c(1,2))
matplot(thresh,cbind(sensitivity_r,specificity_r),type="l",xlab="Thr
        eshold",ylab="Proportion",lty=1:2)
plot(1-specificity_r,sensitivity_r,type="l");abline(0,1,lty=2)

par(mfrow=c(1,1))
plot(1-specificity,sensitivity, type="l", col="green")
points(1-specificity_r,sensitivity_r,type="l",col="orange")
lines(1-specificity_r,sensitivity_r, col="red",lty=2)

###########
# step 13 #
###########


airplane <- data.frame(aircraft="boeing",duration=200,no_pasg=80,
                      speed_ground=115,speed_air=120,height=40,pitch=4)
pred1 <- predict(model_bic,newdata=airplane,type = 'response',se.fit = T)
pred2 <- predict(model_bic_r,newdata=airplane,type='response' ,se.fit = T)

c(pred1$fit,pred1$fit-1.96*pred1$se.fit[1],pred1$fit+1.96*pred1$se.fit[1])

c(pred2$fit,pred2$fit-1.96*pred2$se.fit[1],pred2$fit+1.96*pred2$se.fit[1])


###########
# step 14 #
###########

model_logit <- glm(risky.landing ~ speed_ground + aircraft, data = data, family=binomial(link='logit'))
summary(model_logit) 

model_probit <- glm(risky.landing ~ speed_ground + aircraft, data = data, family=binomial(link='probit'))
summary(model_probit) 
BIC(model_probit)

model_loglog <- glm(risky.landing ~ speed_ground + aircraft, data = data, family=binomial(link='cloglog'))
summary(model_loglog) 
BIC(model_loglog)

round(coef(model_logit),3)
round(coef(model_probit),3)
round(coef(model_loglog),3)

###########
# step 15 #
###########

thresh <- seq(0.01,0.5,0.01)
sensitivity_r <- specificity_r <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(model_logit,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~data1$risky.landing+pp)
  specificity_r[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity_r[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}

thresh <- seq(0.01,0.5,0.01)
sensitivity_probit <- specificity_probit <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(model_probit,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~data1$risky.landing+pp)
  specificity_probit[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity_probit[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}

thresh <- seq(0.01,0.5,0.01)
sensitivity_loglog <- specificity_loglog <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(model_loglog,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~data1$risky.landing+pp)
  specificity_loglog[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity_loglog[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}

par(mfrow=c(1,1))
plot(1-specificity_r,sensitivity_r, type ="p", col="orange")
points(1-specificity_r,sensitivity_r,type="p",col="orange", pch =19)
lines(1-specificity_r,sensitivity_r, col="orange",lty=2)
points(1-specificity_probit,sensitivity_probit,type="b",col="green", pch = 22)
lines(1-specificity_probit,sensitivity_probit, col="green",lty=2)
lines(1-specificity_loglog,sensitivity_loglog, col="blue",lty=2)




par(mfrow=c(1,1))
# plot(1-specificity,sensitivity, type="l", col="green")
plot(1-specificity_r,sensitivity_r, type ="p", col="orange")
points(1-specificity_r,sensitivity_r,type="p",col="orange", pch =19)
lines(1-specificity_r,sensitivity_r, col="orange",lty=2)
points(1-specificity_probit,sensitivity_probit,type="b",col="green", pch = 22)
lines(1-specificity_probit,sensitivity_probit, col="green",lty=2)
#points(1-specificity_loglog,sensitivity_loglog,type="o",col="blue", pch = 25)
lines(1-specificity_loglog,sensitivity_loglog, col="blue",lty=2)

###########
# step 16 #
###########

pred_logit <- predict(model_logit,type = 'response')
summary(model_logit)
pred_probit <- predict(model_probit,type = 'response')
summary(model_probit)
pred_loglog <- predict(model_loglog,type = 'response')
summary(model_loglog)


data1[as.numeric(names(tail(sort(pred_logit),5))),]
data1[as.numeric(names(tail(sort(pred_probit),5))),]
data1[as.numeric(names(tail(sort(pred_loglog),5))),]

###########
# step 17 #
###########
model_logit_l <- glm(long.landing ~ speed_ground + aircraft + height, data = data1, family=binomial(link='logit'))
summary(model_logit_l) 
BIC(model_logit_l)

model_probit_l <- glm(long.landing ~ speed_ground + aircraft + height, data = data1, family=binomial(link='probit'))
summary(model_probit_l) 
BIC(model_probit_l)

model_loglog_l <- glm(long.landing ~ speed_ground + aircraft + height, data = data1, family=binomial(link='cloglog'))
summary(model_loglog_l) 
BIC(model_loglog_l)

model_logit_r <- model_logit
model_probit_r <- model_probit
model_loglog_r <- model_loglog


airplane <- data.frame(aircraft="boeing",duration=200,no_pasg=80,
                       speed_ground=115,speed_air=120,height=40,pitch=4)

## logit for long and risky
pred_logit_l <- predict(model_logit_l,newdata=airplane,type = 'response',se.fit = T)
pred_logit_r <- predict(model_logit_r,newdata=airplane,type='response' ,se.fit = T)

c(pred_logit_l$fit,pred_logit_l$fit-1.96*pred_logit_l$se.fit[1],pred_logit_l$fit+1.96*pred_logit_l$se.fit[1])

c(pred_logit_r$fit,pred_logit_r$fit-1.96*pred_logit_r$se.fit[1],pred_logit_r$fit+1.96*pred_logit_r$se.fit[1])

## probit for long and risky
pred_probit_l <- predict(model_probit_l,newdata=airplane,type = 'response',se.fit = T)
pred_probit_r <- predict(model_probit_r,newdata=airplane,type='response' ,se.fit = T)

c(pred_probit_l$fit,pred_probit_l$fit-1.96*pred_probit_l$se.fit[1],pred_probit_l$fit+1.96*pred_probit_l$se.fit[1])

c(pred_probit_r$fit,pred_probit_r$fit-1.96*pred_probit_r$se.fit[1],pred_probit_r$fit+1.96*pred_probit_r$se.fit[1])

## Hazard for long and risky

pred_loglog_l <- predict(model_loglog_l,newdata=airplane,type = 'response',se.fit = T)
pred_loglog_r <- predict(model_loglog_r,newdata=airplane,type='response' ,se.fit = T)

c(pred_loglog_l$fit,pred_loglog_l$fit-1.96*pred_loglog_l$se.fit[1],pred_loglog_l$fit+1.96*pred_loglog_l$se.fit[1])

c(pred_loglog_r$fit,pred_loglog_r$fit-1.96*pred_loglog_r$se.fit[1],pred_loglog_r$fit+1.96*pred_loglog_r$se.fit[1])
