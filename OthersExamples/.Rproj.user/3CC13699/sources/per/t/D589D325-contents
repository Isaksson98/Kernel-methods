library("geosphere")
setwd("~/TDDE01/lab3/")
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "Latin1")
temps <- read.csv("temps50k.csv")
station.numbers <- unique(subset(stations, select = c(station_number)))

dist_h <- 350000 
date_h <- 60
time_h <- 6

gaussian_kernel <-function(u){
  return(exp(-(u^2)))
}

distance_kernel <- function(station, target){
  station.long.lat <- c(stations[station_number == station, 'longitude'], stations[station_number == station, 'latitude'])
  distance <- distHaversine(station.long.lat, target)
  
  return(gaussian_kernel(distance)/dist_h)
}

time_kernel <- function(station, target){
  s.substring <- substring(station, 1,2)
  s.int <- strtoi(s.substring, base = 0L)
  t.substring <- substring(target, 1,2)
  t.int <- strtoi(t.substring, base = 0L)
  time.diff <- (abs(s.int - t.int) %% 24)
  if(time.diff > 12)
    return(gaussian_kernel((24 - time.diff)/time_h))
  return(gaussian_kernel(time.diff/time_h))
}

date_kernel <- function(station, target){
  date.years <- as.double.difftime(abs(difftime(time1 = target, time2 = station, units = "days")))
  leap.years <- floor(floor(date.years/365)/4)
  date.days <- (date.years - leap.years) %% 365
  if(date.days > 182)
    return(gaussian_kernel((365 - date.days)/date_h))
  return((date.days)/date_h)
}

st <- merge(stations,temps,by="station_number")

target.longitude <- 58.4274 # The point to predict (up to the students)
target.latitude <- 14.826
target.pos <- c(target.longitude, target.latitude)
target.time <- "24:00:00"
target.date <- "2015-02-01" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", ..., "24:00:00")
temp <- vector(length=length(times))
target.posix <- as.POSIXct(paste(target.date, target.time))

# Students’ code here
plot(temp, type="o")


station.posix <- as.POSIXct(paste(station.date, station.time))

for (measurment in st) {
  
}

hour.diff <- time_kernel(station.time, target.time)
time.diff <- time_kernel(station = station.time, target = target.time)
date.diff <- floor(date_kernel(station.posix, target.posix))