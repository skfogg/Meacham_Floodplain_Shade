##
## Data from Meacham 2021: sunny and shady lux
##
library(xts)
library(zoo)
library(lubridate)
source("includeNATimes.R")

## read in data ##
shadyair <- read.csv("shadeatm.csv")
sunnyair <- read.csv("sunnyatm.csv")

#### xts, zoo, includeNATimes ####
shadyairx <- includeNATimes(xts(zoo(shadyair$Lux, order.by = mdy_hms(shadyair$DateTime))))
sunnyairx <- includeNATimes(xts(zoo(sunnyair$Lux, order.by = mdy_hms(sunnyair$DateTime))))

#### remove incomplete last/first days ####
# remove: shadyairx['2021-08-17']
shadyairx <- shadyairx['2020-07-25/2021-08-16']

# remove: sunnyair['2021-07-24']
sunnyairx <- sunnyairx['2020-07-25/2021-08-15']

#### plot it ####
plot.zoo(sunnyairx, type = "l", col = "goldenrod")
lines(as.zoo(shadyairx), col = "brown")
