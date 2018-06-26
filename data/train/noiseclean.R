#function for cleaning rms data

library(lubridate)
library(dplyr)
library(zoo)

clean_noise <- function(fname){
    #readcsv
    df <- read.csv(fname, stringsAsFactors = FALSE, 
                      header = TRUE)
    df <- df %>% mutate(time = ymd_hms(substring(timestamp, 1, 19)))
    
    for(cname in colnames(df)[2:6]){
        x = quantile(df[,cname], c(0.01, 0.99))
        x1 <- x[1] - 0.30*(x[2] - x[1])
        x2 <- x[2] + 0.30*(x[2] - x[1])
        rmed = rollmedian(df[,cname], k = 25, fill = 0)
        df[df[,cname] < x1, cname] <- rmed[df[,cname] < x1]
        df[df[,cname] > x2, cname] <- rmed[df[,cname] > x2]
    }
    
    df$xc <- cos(2*pi*as.numeric(
        difftime(df$time, df$time[1], units = "mins"))/525600)
    df$xs <- sin(2*pi*as.numeric(
        difftime(df$time, df$time[1], units = "mins"))/525600)
    
    for(cname in colnames(df)[2:6]){
        fit.lm <- lm(df[,cname] ~ xc + xs, data = df)
        df[,cname] <- fit.lm$residuals
    }

#        clstr <- kmeans(df[,cname],5)
#        i <- dim(df)[2] + 1
#        df[,i] <- clstr$cluster
#        colnames(df)[i] <- paste(cname, "state", sep="")
    
    omega <- tail(df$time, 1) + minutes(15)
    df <- df %>% mutate(TTF = difftime(omega, time, units = "secs"))
    df$TTF <- as.integer(df$TTF) / 3600
    #is3HRleft is3DAY is3WEEK
    df$expTTF <- exp(-df$TTF / 504)
#    df <- df %>% mutate(is3HR = (TTF < 3.00), 
#                        is3DY = (TTF < 72.00),
#                        is3WK = (TTF < 504.00))
    
    return(df)
}

