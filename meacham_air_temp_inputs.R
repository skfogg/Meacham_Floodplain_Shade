##
## Data from Meacham 2021: sunny and shady air temp
##
library(xts)
library(zoo)
library(lubridate)
source("includeNATimes.R")

## read in data ##
shadyair <- read.csv("shadeatm.csv")
sunnyair <- read.csv("sunnyatm.csv")

#### xts, zoo, includeNATimes ####
shadyairx <- includeNATimes(xts(zoo(shadyair$Temp, order.by = mdy_hms(shadyair$DateTime))))
sunnyairx <- includeNATimes(xts(zoo(sunnyair$Temp, order.by = mdy_hms(sunnyair$DateTime))))

#### remove incomplete last/first days ####
# remove: shadyairx['2021-08-17']
shadyairx <- shadyairx['2020-07-25/2021-08-16']

# remove: sunnyair['2021-07-24']
sunnyairx <- sunnyairx['2020-07-25/2021-08-15']

#### plot it ####
plot.zoo(sunnyairx, type = "l", col = "goldenrod")
lines(as.zoo(shadyairx), col = "brown")

#### includeNATimes to non-xts/zoo formatted data ####
shadyair <- includeNATimes(shadyairx, dropTS = T)
colnames(shadyair) <- c("DateTime", "Temp")
sunnyair <- includeNATimes(sunnyairx, dropTS = T)
colnames(sunnyair) <- c("DateTime", "Temp")

#### compile list of data ####
rawdata <- list(shadyair = shadyair, sunnyair = sunnyair)

#### add rows to data.frames ####
for (i in 1:2){
  rawdata[[i]]$DateTime <- as.character(rawdata[[i]]$DateTime)
  rawdata[[i]]$PosixTime <- ymd_hms(rawdata[[i]]$DateTime)
  rawdata[[i]]$NumTime <- as.numeric(rawdata[[i]]$PosixTime)
  rawdata[[i]]$NumTimeOfYr <- rawdata[[i]]$NumTime - as.numeric(mdy_hms("01/01/20 00:00:00"))
}

#### compile list of xts/zoo data ####
zoodata <- list(shadyairx = shadyairx, sunnyairx = sunnyairx)

#### calc daily means of xts/zoo data ####
shadyairdailymeanx <- apply.daily(shadyairx, mean)
sunnyairdailymeanx <- apply.daily(sunnyairx, mean)


#### compile daily mean data into list ####
rawdatameans <- list(shadyairdailymean = data.frame(DateTime = as.character(index(shadyairdailymeanx)),
                                                     PosixTime = index(shadyairdailymeanx),
                                                     NumTime = as.numeric(index(shadyairdailymeanx)),
                                                     NumTimeOfYr = as.numeric(index(shadyairdailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                                                     DailyMean = coredata(shadyairdailymeanx)), 
                     sunnyairdailymean = data.frame(DateTime = as.character(index(sunnyairdailymeanx)),
                                                     PosixTime = index(sunnyairdailymeanx),
                                                     NumTime = as.numeric(index(sunnyairdailymeanx)),
                                                     NumTimeOfYr = as.numeric(index(sunnyairdailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                                                     DailyMean = coredata(sunnyairdailymeanx)))
for (i in 1:2){
  names(rawdatameans[[i]])[5] <- "DailyMean"
}

#### add daily mean data to rawdata list ####
for (i in 1:2){
  rawdata[[i]]$DailyMean <- rep(coredata(rawdatameans[[i]]$DailyMean), each = 24) 
}

#### create a meaningless list to put the model data into ####
meachammodels <- list(shadyair = 1, sunnyair = 2)


for (i in 1:2){
  ### --------------------------- DAILY MEAN MODEL --------------------------- ###
  ### ------------------------------------------------------------------------ ###
  # start val for m:
  meanstart <- mean(rawdatameans[[i]]$DailyMean, na.rm = T)
  # start val for a:
  ampstart <- (max(rawdatameans[[i]]$DailyMean, na.rm = T)-min(rawdatameans[[i]]$DailyMean, na.rm = T))/2
  # fucking guess the radian shift for p
  
  dailymeanmodel <- nls(DailyMean ~ m + a*sin(2*pi*(1/31536000)*NumTimeOfYr + p),
                        rawdatameans[[i]],
                        start = list(m = meanstart,
                                     a = ampstart,
                                     p = 0.1))
  summary(dailymeanmodel)
  plot(DailyMean ~ I(NumTimeOfYr/86400), rawdatameans[[i]], type = "l")
  points(rawdatameans[[i]][complete.cases(rawdatameans[[i]]$DailyMean),4]/86400, predict(dailymeanmodel), col = "blue")
  
  
  t <- seq(0, 31536000*2, by = 86400)
  plot(coef(dailymeanmodel)[1] + coef(dailymeanmodel)[2]*sin(2*pi*(1/31536000)*t + coef(dailymeanmodel)[3]),
       type = "l", 
       lwd = 2,
       ylab = "Daily Mean Temperture")
  lines(DailyMean ~ I(NumTimeOfYr/86400), rawdatameans[[i]], col = "blue")
  
  ### -------------------- DAILY AMPLITUDE MODEL -------------------------- ###
  ### --------------------------------------------------------------------- ###
  amps <- apply.daily(zoodata[[i]], function(x) (max(x)-min(x))/2)
  plot.zoo(amps)
  
  rawdatameans[[i]]$DailyAmp <- coredata(amps)
  plot(rawdatameans[[i]]$DailyAmp, type = "l")
  
  # start val for m:
  meanstart <- mean(rawdatameans[[i]]$DailyAmp, na.rm = T)
  # start val for a:
  ampstart <- (max(rawdatameans[[i]]$DailyAmp, na.rm = T)-min(rawdatameans[[i]]$DailyAmp, na.rm = T))/2
  # fucking guess the radian shift for p
  
  dailyampmodel <- nls(DailyAmp ~ m + a*sin(2*pi*(1/31536000)*NumTimeOfYr + p),
                       rawdatameans[[i]],
                       start = list(m = meanstart,
                                    a = ampstart,
                                    p = 0.1))
  summary(dailyampmodel)
  plot(coef(dailyampmodel)[1] + coef(dailyampmodel)[2]*sin(2*pi*(1/31536000)*t + coef(dailyampmodel)[3]),
       type = "l", 
       lwd = 2,
       ylab = "Daily Amplitude",
       ylim = c(0,3))
  lines(DailyAmp ~ I(NumTimeOfYr/86400), rawdatameans[[i]], col = "violet")
  
  ## --------------------- DAILY FREQUENCY MODEL ------------------- ##
  ## --------------------------------------------------------------- ##
  
  dailyfreqmodel <- nls(I(Temp-DailyMean) ~ a*sin(2*pi*(1/86400)*NumTimeOfYr + p),
                        rawdata[[i]],
                        start = list(a = 2,
                                     p = 0.1))
  summary(dailyfreqmodel)
  t2 <- seq(0, 31536000*2, by = 3600)
  plot(coef(dailyfreqmodel)[1]*sin(2*pi*(1/86400)*t2 + coef(dailyfreqmodel)[2]),
       type = "l")
  lines(rawdata[[i]]$Temp - rawdata[[i]]$DailyMean, col = "yellow")
  
  plot(rawdata[[i]]$Temp - rawdata[[i]]$DailyMean, type = "l")
  lines(predict(dailyfreqmodel), col = "yellow")
  
  ## --------------------- COMPOUND MODEL -------------------------- ##
  ## --------------------------------------------------------------- ##
  t10 <- seq(0, 31536000*10, by = 3600)
  means <- coef(dailymeanmodel)[1] + coef(dailymeanmodel)[2]*sin(2*pi*(1/31536000)*t10 + coef(dailymeanmodel)[3])
  amps <- coef(dailyampmodel)[1] + coef(dailyampmodel)[2]*sin(2*pi*(1/31536000)*t10 + coef(dailyampmodel)[3])
  
  meachammodels[[i]] <- means + amps*sin(2*pi*(1/86400)*t10 + coef(dailyfreqmodel)[2])
}

plot(meachammodels[[1]] ~ t10, type = "l",
     ylim = c(-10,50),
     xlim = c(0, 80000000))
lines(rawdata[[1]]$Temp ~ rawdata[[1]]$NumTimeOfYr,
      col = "brown")
plot(meachammodels[[2]] ~ t10, type = "l",
     ylim = c(-10,50),
     xlim = c(0, 80000000))
lines(rawdata[[2]]$Temp ~ rawdata[[2]]$NumTimeOfYr,
      col = "orange")

plot(meachammodels[[2]] ~ t10, type = "l",
     ylim = c(-10,50),
     xlim = c(0, 80000000), col = "gold")
lines(meachammodels[[1]] ~ t10, col = "brown")


#### MAKE 10 YEARS OF DATA ####
#meachamshadyair <- meachammodels[[1]]
meachamsunnyair <- meachammodels[[2]]

#meachamshadydf <- data.frame(s = t10, e = t10+3600, temp = meachamshadyair)
meachamsunnydf <- data.frame(s = t10, e = t10+3600, temp = meachamsunnyair)

#write.csv(meachamshadydf, "meacham_creek_data/final_meacham_hgs_input/meachamshadydf.csv")
write.csv(meachamsunnydf, "meachamsunnydf.csv")


