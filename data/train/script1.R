library(lubridate)
library(forecast)
library(tidyr)


alrm0 <- read.csv("unit0000_alarms.csv", stringsAsFactors = FALSE, 
                  header = FALSE)

alrm0 <- alrm0 %>% mutate(date = as.POSIXct(substring(V1, 1, 10)))
alrm0 <- alrm0 %>% mutate(time = ymd_hms(substring(V1, 12, 19)))

U0.df <- read.csv("unit0000_rms.csv", stringsAsFactors = FALSE, 
                  header = TRUE)
U0.df <- U0.df %>% mutate(time = ymd_hms(substring(timestamp, 1, 19)))

# clean up the noise
# replace outliers with a rollingmedian from zoo
# outlier groups (9 in a row) are persistent, increase k

U0.df <- U0.df %>% mutate(ma_rpm = 
                    stats::filter(rpm, c(0.25, 0.25, 0, 0.25, 0.25)))

U0.df <- U0.df %>% mutate(md_rpm = rollmedian(rpm, k = 25, fill = NA))


U0.df[U0.df$rpm < -1000, "rpm"] <- U0.df[U0.df$rpm < -1000, "md_rpm"]
U0.df[U0.df$rpm > 1500, "rpm"] <- U0.df[U0.df$rpm > 1500, "md_rpm"]

#motor_voltage
#U0.df <- U0.df %>% mutate(ma_rpm = 
#                  stats::filter(rpm, c(0.25, 0.25, 0, 0.25, 0.25)))
U0.df <- U0.df %>% mutate(md_vlt = rollmedian(motor_voltage, 
                                    k = 25, fill = NA))

U0.df[U0.df$motor_voltage < -50, "motor_voltage"] <- U0.df[U0.df$motor_voltage 
                                                 < -50, "md_vlt"]
U0.df[U0.df$motor_voltage > 360, "motor_voltage"] <- U0.df[U0.df$motor_voltage 
                                                 > 360, "md_vlt"]

#inlet_temp
quantile(U0.df$inlet_temp, c(0.01, 0.05, 0.95, 0.99))
U0.df <- U0.df %>% mutate(md_inl = rollmedian(inlet_temp, 
                                              k = 25, fill = NA))
U0.df[U0.df$inlet_temp < 0, "inlet_temp"] <- U0.df[U0.df$inlet_temp 
                                                   < 0, "md_inl"]
U0.df[U0.df$inlet_temp > 250, "inlet_temp"] <- U0.df[U0.df$inlet_temp 
                                                     > 250, "md_inl"]
qplot(U0.df$time, U0.df$inlet_temp)

#motor_temp
quantile(U0.df$motor_temp, c(0.01, 0.05, 0.95, 0.99))
U0.df <- U0.df %>% mutate(md_mot = rollmedian(motor_temp, 
                                              k = 25, fill = NA))
U0.df[U0.df$motor_temp < 0, "motor_temp"] <- U0.df[U0.df$motor_temp 
                                                   < 0, "md_mot"]
U0.df[U0.df$motor_temp > 400, "motor_temp"] <- U0.df[U0.df$motor_temp 
                                                     > 400, "md_mot"]
qplot(U0.df$time, U0.df$motor_temp)

#motor_current
quantile(U0.df$motor_current, c(0.01, 0.05, 0.95, 0.99))
U0.df <- U0.df %>% mutate(md_cur = rollmedian(motor_current, 
                                              k = 25, fill = NA))
U0.df[U0.df$motor_current < -10, "motor_current"] <- 
    U0.df[U0.df$motor_current < -10, "md_cur"]
U0.df[U0.df$motor_current > 100, "motor_current"] <- 
    U0.df[U0.df$motor_current > 100, "md_cur"]
qplot(U0.df$time, U0.df$motor_current)



#time to failure
omega <- tail(U0.df$time, 1) + minutes(15)
U0.df <- U0.df %>% mutate(TTF = difftime(omega, time, units = "hours"))
U0.df$TTF <- as.integer(U0.df$TTF)

smpl.df <- U0.df[sample(1:91715, size = 2000),]
ggplot(large, aes(x=motor_voltage, y=rpm, color=TTF)) + geom_point(shape = 23) + scale_color_gradientn(colours = rainbow(4))

