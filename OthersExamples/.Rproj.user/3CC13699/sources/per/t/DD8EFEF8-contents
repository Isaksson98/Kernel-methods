set.seed(1234567890)
library(geosphere)

# Reading and formatting data
stations <- read.csv("stations.csv", header=TRUE, sep=",", dec=".", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by="station_number")
st = st[,c("date", "time", "latitude", "longitude", "air_temperature")]
st$date = as.Date(st$date)

# Prediction information
date <- as.Date("2013-12-04") #The date to predict
a <- 58.4274 # The latitude of point to predict
b <- 14.826 # The longitude of point to predict

# The times for prediction
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00",
           "18:00:00", "20:00:00", "22:00:00", "24:00:00")
# Change format of times to predict
times <- strptime(times, "%H:%M:%S")

# Remove dates after the date to predict
filtered_st = st[st$date < date, ]
# Change format of times in filtered data
filtered_st$time = strptime(filtered_st$time, "%H:%M:%S") 

# Predicted temperatures
temp <- vector(length = length(times))

# Calculates distance in days between a reference date and a date
dist_date <- function(refDate, date){
  return (as.numeric(difftime(date, refDate, units = "days"))%%365)
}

# Calculates distance in minutes between a reference time and a time
dist_time <- function(refTime, time) {
  return(as.numeric(difftime(time, refTime, units = "mins")))
}

# Set smoothing factors
h_distance <- sd(distHaversine(c(0,0), cbind(filtered_st$longitude, filtered_st$latitude)))
h_date <- sd(dist_date(as.Date("2000-01-01"), filtered_st$date))
h_time <- sd(dist_time(strptime("00:00:00", "%H:%M:%S"), filtered_st$time))

# General formula for Gaussian Kernel
gaussianKernel <- function(xDiff, h) {
  u <- xDiff/h
  return (exp(-(u)^2)) 
}

# Prediction with summation of kernels
k1 <- gaussianKernel(distHaversine(c(b,a), cbind(filtered_st$longitude, filtered_st$latitude)), h_distance)

k2 <- gaussianKernel(dist_date(date, filtered_st$date), h_date)

for (i in 1:length(times)) {
  k3 <- gaussianKernel(dist_time(times[i], filtered_st$time), h_time)
  
  temp[i] <-  sum((k1 + k2 + k3)*filtered_st$air_temperature) / sum(k1+k2+k3)
}

plot(times, temp, type="o", main="k1 + k2 + k3", col="red")

# Validate if h-values are reasonable
close_date = filtered_st[(dist_date(date, filtered_st[,'date']) == 0 ) | 
                           (dist_date(date, filtered_st[,'date']) == 364 )|
                           (dist_date(date, filtered_st[,'date']) == 1 ),]
geo_distances = distHaversine(c(b,a), cbind(close_date$longitude, close_date$latitude)) 

rbPal <- colorRampPalette(c('red','yellow'))
col <- rbPal(10)[as.numeric(cut(geo_distances,breaks = 40))]

plot(times, temp, type="o", ylim=c(min(temp)-8,max(temp)+8), main="Comparison to training data k1+k2+k3", col="red")
points(close_date$time, close_date$air_temperature, col=col)

# Prediction with multiplication of kernels
for (i in 1:length(times)) {
  k3 <- gaussianKernel(dist_time(times[i], filtered_st$time), h_time)
  
  temp[i] <-  sum((k1 * k2 * k3)*filtered_st$air_temperature) / sum(k1*k2*k3)
}
plot(times, temp, type="o", main="k1 * k2 * k3", col="red")

# Validate if h-values are reasonable
plot(times, temp, type="o", ylim=c(min(temp)-8,max(temp)+8), main="Comparison to training data k1*k2*k3", col="red")
points(close_date$time, close_date$air_temperature, col=col)