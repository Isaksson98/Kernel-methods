set.seed(1234567890)
library(geosphere)

#Functions####
gaussianKernel <- function(x_diff, h) {
  u = x_diff/h
  kernal = exp(-(u)^2)
  return (kernal)
}

dist_date <- function(date1, date2){
    x = (as.numeric(difftime(date1, date2, units = "days"))%%365)
    for (i in 1:length(x)){
      if(x[i] > 182)
        x[i] = 365 - x[i]
    }
    return (x)
}

dist_time <- function(time1, time2){
  x = as.numeric(difftime(time1, time2, units = "hours") )
  for (i in 1:length(x)){
    if(x[i] > 12)
      x[i] = 24 - x[i]
  }
  return (x)
}

#Initialize: #####

stations = read.csv("stations.csv", header = TRUE)
temps = read.csv("temps50k.csv", header = TRUE)
st <- merge(stations,temps,by="station_number")


# The point to predict (up to the students)
a <- 58.4274 
b <- 14.826

# The date to predict (up to the students)
date <- "2015-07-10" 
times <- c("04:00:00", "06:00:00", "08:00:00",
           "10:00:00","12:00:00","14:00:00",
           "16:00:00","18:00:00","20:00:00",
           "22:00:00","24:00:00")
times <- strptime(times, "%H:%M:%S")


temperature <- vector(length=length(times))

#Filter out dates that come after chosen date:
st_filtered = st[as.Date(st$date) < as.Date(date),]
st_filtered$time = strptime(st_filtered$time, "%H:%M:%S") 


#Smoothing factors: ####
h_distance <- 200000 #meters
h_date <- 15 #days
h_time <- 4 #hours

h=c(h_distance,h_date, h_time)


distance = seq(0, 500)
kern_dist=gaussianKernel(distance, h_distance/1000)
plot(distance, kern_dist, xlab = "diff", ylab="Weight", main="Distance" )

date_diff = seq(0, 50, 1)
kern_dist=gaussianKernel(date_diff, h_date)
plot(date_diff, kern_dist, xlab = "date_diff", ylab="Weight", main="Date" )

time_diff = seq(0, 20, 1)
kern_dist=gaussianKernel(time_diff, h_time)
plot(time_diff, kern_dist, xlab = "time_diff", ylab="Weight", main="Time" )

#Predictions with summation: ####

dist_diff = distHaversine(c(b,a), cbind(st_filtered$longitude, st_filtered$latitude))
k_dist <- gaussianKernel(dist_diff, h_distance)

date_diff = dist_date(date, st_filtered$date)
k_date <- gaussianKernel(date_diff, h_date)

for (i in 1:length(times)) {
  time_distance = dist_time(times[i], st_filtered$time)
  k_time = gaussianKernel(time_distance, h_time)
  
  temperature[i] = sum((k_dist + k_date + k_time)*st_filtered$air_temperature) / sum(k_dist + k_date + k_time)

  }

plot(times, temperature, type="o", main="Addition", col="green")


#Predictions with multiplication: ####

dist_diff = distHaversine(c(b,a), cbind(st_filtered$longitude, st_filtered$latitude))
k_dist <- gaussianKernel(dist_diff, h_distance)

date_diff = dist_date(date, st_filtered$date)
k_date <- gaussianKernel(date_diff, h_date)

for (i in 1:length(times)) {
  time_distance = dist_time(times[i], st_filtered$time)
  k_time = gaussianKernel(time_distance, h_time)

  temperature[i] = sum((k_dist * k_date * k_time)*st_filtered$air_temperature) / sum(k_dist * k_date * k_time)
}

plot(times, temperature, type="o", main="Multiplication", col="orange")



