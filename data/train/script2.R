#Notes: 
# average time to failure: 
# timesteps: mean 10 minutes, deviance 2 minutes
# 

library(ggplot2)
library(dplyr)
library(lubridate)


setwd("/Users/dojo/data/tagup/data-science-challenge/data/train/")
#clean data
source("./noiseclean.R")

unit00 <- clean_noise("./unit0000_rms.csv")
unit01 <- clean_noise("./unit0001_rms.csv")
unit02 <- clean_noise("./unit0002_rms.csv")
unit03 <- clean_noise("./unit0003_rms.csv")
unit04 <- clean_noise("./unit0004_rms.csv")
unit05 <- clean_noise("./unit0005_rms.csv")
unit06 <- clean_noise("./unit0006_rms.csv")
unit07 <- clean_noise("./unit0007_rms.csv")
unit08 <- clean_noise("./unit0008_rms.csv")
unit09 <- clean_noise("./unit0009_rms.csv")
unit10 <- clean_noise("./unit0010_rms.csv")
unit11 <- clean_noise("./unit0011_rms.csv")
unit12 <- clean_noise("./unit0012_rms.csv")
unit13 <- clean_noise("./unit0013_rms.csv")
unit14 <- clean_noise("./unit0014_rms.csv")
unit15 <- clean_noise("./unit0015_rms.csv")
unit16 <- clean_noise("./unit0016_rms.csv")
unit17 <- clean_noise("./unit0017_rms.csv")
unit18 <- clean_noise("./unit0018_rms.csv")
unit19 <- clean_noise("./unit0019_rms.csv")

myfiles <- grep("_rms", dir(), value = TRUE)
unitDF <- lapply(myfiles, clean_noise)
ttf <- lapply(unitDF, function(x) head(x$TTF, 1))
ttf <- unlist(ttf)
summary(ttf)

#combine? 
fit1names <- c("rpm", "motor_voltage", "motor_current", 
               "motor_temp", "inlet_temp")
fit2names <- c("is3DY", "rpm", "motor_voltage", "motor_current", 
               "motor_temp", "inlet_temp")
fit3names <- c("is3WK", "rpm", "motor_voltage", "motor_current", 
               "motor_temp", "inlet_temp")
#test 
df1 <- unitDF[[1]]
unitKmns <- kmeans(df1[,fit1names], 7, algorithm = "Lloyd", iter.max=100)
df1$cluster <- as.factor(unitKmns$cluster)
ggplot(data=df1, aes(x=time, y=motor_voltage, color=cluster)) + 
    geom_point(shape=23)


model1 <- glm(expTTF~cluster,family=binomial, data=df1)

summary(model1)
newdat <- data.frame(cluster=sub.df$cluster)
sub.df$pred <- predict(model1, newdata = newdat, type = "response")
ggplot(data=sub.df, aes(x=unit00$time, y=pred)) + 
    geom_point(shape=2) 

sub.df <- unit00[,fit2names]
unitKmns <- kmeans(sub.df[,-1], 5, algorithm = "Lloyd", iter.max = 100)
sub.df$cluster <- as.factor(unitKmns$cluster)
centers <- unitKmns$centers


#make model to predict TTF? 


ggplot(data = unit09, aes(x = motor_voltage, y = rpm, color = TTF)) +
    geom_point(shape = 23) +
    scale_color_gradientn(colors = rainbow(4))
    

unit_end <- tail(unit00[,c(2:6,8)], 100)
unit_end <- rbind(unit_end, tail(unit01[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit02[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit03[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit04[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit05[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit06[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit07[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit08[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit09[,c(2:6,8)], 100))
unit_end <- rbind(unit_end, tail(unit10[,c(2:6,8)], 100))


model1 <- randomForest(is3DY ~ 
rpmstate + motor_voltagestate + motor_currentstate + motor_tempstate + 
    inlet_tempstate, data = unit00)

