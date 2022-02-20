#### 
#### MODEL FOR HGS INPUT 
#### OCTOBER 2021
####

library(xts)
library(zoo)
library(lubridate)
source("includeNATimes.R")

#### Read in Data ####
shadysoil <- read.csv("~/Floodplain_Shade/meacham_creek_data/shadysoil.csv")
sunnysoil <- read.csv("~/Floodplain_Shade/meacham_creek_data/sunnysoil.csv")
watertemp <- read.csv("~/Floodplain_Shade/meacham_creek_data/watertemp.csv")

#### xts, zoo, includeNATimes ####
shadysoilx <- includeNATimes(xts(zoo(shadysoil$Temp, order.by = mdy_hms(shadysoil$DateTime))))
sunnysoilx <- includeNATimes(xts(zoo(sunnysoil$Temp, order.by = mdy_hms(sunnysoil$DateTime))))
watertempx <- includeNATimes(xts(zoo(watertemp$Temp, order.by = ymd_hms(watertemp$DateTime))))

#### plot raw data ####
par(mfrow = c(1,1), mar = c(3,3,1,1))
plot.zoo(watertempx, 
         xlim = c(mdy_hms("06-18-20 00:00:00"), mdy_hms("09-01-21 00:00:00")),
         ylim = c(0,33),
         col = "dodgerblue")
lines(as.zoo(shadysoilx), col = "brown")
lines(as.zoo(sunnysoilx), col = "orange")

#### includeNATimes to non-xts/zoo formatted data ####
shadysoil <- includeNATimes(shadysoilx, dropTS = T)
colnames(shadysoil) <- c("DateTime", "Temp")
sunnysoil <- includeNATimes(sunnysoilx, dropTS = T)
colnames(sunnysoil) <- c("DateTime", "Temp")
watertemp <- includeNATimes(watertempx, dropTS = T)
colnames(watertemp) <- c("DateTime", "Temp")

#### compile list of data ####
rawdata <- list(shadysoil = shadysoil, sunnysoil = sunnysoil, watertemp = watertemp)

#### add rows to data.frames ####
for (i in 1:3){
rawdata[[i]]$DateTime <- as.character(rawdata[[i]]$DateTime)
rawdata[[i]]$PosixTime <- ymd_hms(rawdata[[i]]$DateTime)
rawdata[[i]]$NumTime <- as.numeric(rawdata[[i]]$PosixTime)
rawdata[[i]]$NumTimeOfYr <- rawdata[[i]]$NumTime - as.numeric(mdy_hms("01/01/20 00:00:00"))
}

#### compile list of xts/zoo data ####
zoodata <- list(shadysoilx = shadysoilx, sunnysoilx = sunnysoilx, watertempx = watertempx)

#### calc daily means of xts/zoo data ####
shadysoildailymeanx <- apply.daily(shadysoilx, mean)
sunnysoildailymeanx <- apply.daily(sunnysoilx, mean)
watertempdailymeanx <- apply.daily(watertempx, mean)

#### plot daily means ####
plot.zoo(watertempdailymeanx, 
         xlim = c(mdy_hms("06-18-20 00:00:00"), mdy_hms("09-01-21 00:00:00")),
         ylim = c(0,33),
         col = "dodgerblue")
lines(as.zoo(shadysoildailymeanx),
      col = "brown")
lines(as.zoo(sunnysoildailymeanx),
      col = "orange")

#### compile daily mean data into list ####
rawdatameans <- list(shadysoildailymean = data.frame(DateTime = as.character(index(shadysoildailymeanx)),
                                                     PosixTime = index(shadysoildailymeanx),
                                                     NumTime = as.numeric(index(shadysoildailymeanx)),
                                                     NumTimeOfYr = as.numeric(index(shadysoildailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                                                     DailyMean = coredata(shadysoildailymeanx)), 
                     sunnysoildailymean = data.frame(DateTime = as.character(index(sunnysoildailymeanx)),
                                                     PosixTime = index(sunnysoildailymeanx),
                                                     NumTime = as.numeric(index(sunnysoildailymeanx)),
                                                     NumTimeOfYr = as.numeric(index(sunnysoildailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                                                     DailyMean = coredata(sunnysoildailymeanx)), 
                     watertempdailymean = data.frame(DateTime = as.character(index(watertempdailymeanx)),
                                                     PosixTime = index(watertempdailymeanx),
                                                     NumTime = as.numeric(index(watertempdailymeanx)),
                                                     NumTimeOfYr = as.numeric(index(watertempdailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                                                     DailyMean = coredata(watertempdailymeanx)))
for (i in 1:3){
  names(rawdatameans[[i]])[5] <- "DailyMean"
}

#### add daily mean data to rawdata list ####
for (i in 1:3){
  rawdata[[i]]$DailyMean <- rep(coredata(rawdatameans[[i]]$DailyMean), each = 24) 
}

#### create a meaningless list to put the model data into ####
meachammodels <- list(shadysoil = 1, sunnysoil = 2, watertemp = 3)



for (i in 1:3){
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



plot(meachammodels[[1]] ~ t2, type = "l",
     ylim = c(0,35))
lines(rawdata[[1]]$Temp ~ rawdata[[1]]$NumTimeOfYr,
      col = "brown")
plot(meachammodels[[2]] ~ t2, type = "l",
     ylim = c(0,35))
lines(rawdata[[2]]$Temp ~ rawdata[[2]]$NumTimeOfYr,
      col = "orange")
plot(meachammodels[[3]] ~ t2, type = "l",
     ylim = c(0,35))
lines(rawdata[[3]]$Temp ~ rawdata[[3]]$NumTimeOfYr,
      col = "dodgerblue")

par(mfrow = c(2,1))
plot(rawdata[[3]]$Temp ~ rawdata[[3]]$NumTimeOfYr,
     col = "dodgerblue",
     type = "l",
     ylim = c(0,35),
     xlim = c(min(t2), max(t2)))
lines(rawdata[[1]]$Temp ~ rawdata[[1]]$NumTimeOfYr,
      col = "brown")
lines(rawdata[[2]]$Temp ~ rawdata[[2]]$NumTimeOfYr,
      col = "orange")
plot(meachammodels[[3]] ~ t10, type = "l",
     ylim = c(0,35),
     col = "dodgerblue")
lines(meachammodels[[1]] ~ t10,
      col = "brown")
lines(meachammodels[[2]] ~ t10,
      col = "orange")

#### MAKE 10 YEARS OF DATA ####
meachamshadysoil <- meachammodels[[1]]
meachamsunnysoil <- meachammodels[[2]]
meachamwatertemp <- meachammodels[[3]]

meachamshadydf <- data.frame(s = t10, e = t10+3600, temp = meachamshadysoil)
meachamsunnydf <- data.frame(s = t10, e = t10+3600, temp = meachamsunnysoil)
meachamwaterdf <- data.frame(s = t10, e = t10+3600, temp = meachamwatertemp)

write.csv(meachamshadydf, "meacham_creek_data/final_meacham_hgs_input/meachamshadydf.csv")
write.csv(meachamsunnydf, "meacham_creek_data/final_meacham_hgs_input/meachamsunnydf.csv")
write.csv(meachamwaterdf, "meacham_creek_data/final_meacham_hgs_input/meachamwaterdf.csv")


#### PLOTS FOR PAPER ####
riverT <- xts(zoo(rawdata$watertemp$Temp, order.by = rawdata$watertemp$PosixTime)) 
sunnyT <- xts(zoo(rawdata$sunnysoil$Temp, order.by = rawdata$sunnysoil$PosixTime))
shadyT <- xts(zoo(rawdata$shadysoil$Temp, order.by = rawdata$shadysoil$PosixTime))

modelriverT <- xts(zoo(meachammodels$watertemp, order.by = seq(mdy_hms("01-01-2019 00:00:00"), by = 3600, length.out = 87601)))
modelsunnyT <- xts(zoo(meachammodels$sunnysoil, order.by = seq(mdy_hms("01-01-2019 00:00:00"), by = 3600, length.out = 87601)))
modelshadyT <- xts(zoo(meachammodels$shadysoil, order.by = seq(mdy_hms("01-01-2019 00:00:00"), by = 3600, length.out = 87601)))


png("plots/meacham_updated_inputs.png", width = 900*5, height = 1200*5, res = 72*5)
par(mfrow = c(3,1),
    oma = c(4,4,0,0),
    mar = c(1,2,0,0),
    cex.axis = 1.3,
    cex.lab = 1.3,
    cex = 1.3,
    bty = "l")
plot.zoo(modelriverT,
         xlim = c(index(modelriverT["2020"][1]), index(last(modelriverT["2021"]))),
         ylim = c(-1,35),
         col = "gray",
         xaxt = "n",
         lwd = 2)
lines(as.zoo(riverT), lwd = 2)
axis(side = 1, at = c(ymd_hms("2020-01-01 00:00:00", "2021-01-01 00:00:00", "2022-01-01 00:00:00")), labels = F)
text(ymd_hms("2020-01-01 00:00:00"), 34, labels = "River", cex = 2, pos = 4)

plot.zoo(modelsunnyT,
         xlim = c(index(modelriverT["2020"][1]), index(last(modelriverT["2021"]))),
         ylim = c(-1,35),
         col = "gray",
         xaxt = "n", lwd = 2)
lines(as.zoo(sunnyT), lwd = 2)
axis(side = 1, at = c(ymd_hms("2020-01-01 00:00:00", "2021-01-01 00:00:00", "2022-01-01 00:00:00")), labels = F)
text(ymd_hms("2020-01-01 00:00:00"), 34, labels = "Sunny Soil", cex = 2, pos = 4)


plot.zoo(modelshadyT,
         xlim = c(index(modelriverT["2020"][1]), index(last(modelriverT["2021"]))),
         ylim = c(-1,35),
         col = "gray",
          lwd = 2)
lines(as.zoo(shadyT), lwd = 2)
axis(side = 1, at = c(ymd_hms("2020-01-01 00:00:00", "2021-01-01 00:00:00", "2022-01-01 00:00:00")), 
     labels = c("2020", "2021", "2022"))
text(ymd_hms("2020-01-01 00:00:00"), 34, labels = "Shady Soil", cex = 2, pos = 4)

mtext(expression(paste("Temperature ( ", degree, "C)")), side = 2, outer = T, line = 1, cex = 2)
mtext("Time (y)", side = 1, outer = T, line = 2, cex = 2)
dev.off()

