---
  title: "Lab_3"
author: "Alice Velander"
date: "12/16/2019"
output:
  pdf_document: default
html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Assignment 1 - Kernel Methods
#Show that your choice for the kernels’ width is sensible, i.e. that it gives more weight to closer points. Discuss why your of definition of closeness is reasonable.

#When selecting smoothing constants the swedish weather was taking into consideration. Since Sweden have relatively shifting weather, we didn't want the constant too large. Looking at the map also gave a sense of how long the distance constant should be. 

#The following smoothing constants where chosen; 

* h_distance = 110 (km)
* h_date = 10 
* h_time = 2


```{r, echo=TRUE}
setwd("~/Studier+Studentliv/HT19/TDDE01/Labb3")
set.seed(1234567890)
RNGversion('3.5.1')
library(geosphere)
stations=read.csv("stations.csv", header=TRUE, fileEncoding = "ISO-8859-1")
temps=read.csv("temps50k.csv", header=TRUE, fileEncoding = "ISO-8859-1")
st <- merge(stations,temps,by="station_number") #merge into one file after stations_number!
h_distance = 110 #km bort
h_date = 10 #dagar
h_time = 2
h=c(h_distance,h_date, h_time)
a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00","12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")
temp <- vector(length=length(times))
# Filter the data so we only have data that is before selected date:)
st = st[as.Date(st$date) < as.Date(date),]
#----show that closer points are  given heavier weights ------------------------------
x_dist = seq(-400, 400)
kern_dist=exp(-(x_dist/h_distance)^2)
x_days = seq(-180, 180)
kern_days=exp(-(x_days/h_date)^2)
x_time = seq(-12, 12, 0.2)
kern_time=exp(-(x_time/h_time)^2)
par(mfrow = c(1,3))
plot(x_dist, kern_dist, xlab = "Diff", ylab="Weight", main="Distance" )
plot(x_days, kern_days, xlab = "Diff", ylab="Weight", main="Days")
plot(x_time, kern_time, xlab = "Diff", ylab="Weight", main="Hours" )
```

We can clearly see that the kernals have smaller weights on the datapoints that have larger distance. This means that the Kernel is sensible. 

Implement a kernel method to predict the hourly temperatures for a date and place in Sweden. The forecast should consist of the predicted temperatures from 4 am to 24 pm in an interval of 2 hours. Use a kernel that is the sum of three Gaussian kernels. 

Chosen date = 2013-11-04

```{r, echo=TRUE}
n=length(st[,1])
dist=c()
days_diff=c()
time_diff=c()
mat_time_diff= matrix()
#----------Get diff in distance & days ----------------
for (i in 1:n){
  #------------------DISTANCE ------------
  dist_tmp=(distHaversine(c(a,b), c(st[["latitude"]][i],st[["longitude"]][i])))
  # Divide by 1000 to get distance in km
  dist=c(dist, dist_tmp/1000)
  #--------------------DAYS---------------
  diff_tmp=abs(as.numeric(as.Date(date)- as.Date(st[["date"]][i])))
  diff_tmp= diff_tmp %% 365
  if (diff_tmp > 182) {
    diff_tmp=365 - diff_tmp
  }
  days_diff=c(days_diff, diff_tmp)
}
#Save diffs in a matrix
matrix_dist_days=cbind(dist, days_diff)
#For each hour (loop) - get kernel value
add_kernel=c()
mult_kernel=c()
rows=NROW(matrix_dist_days)-1
for (hour in 1:length(times)){
  now_tmp=times[hour]
  #Variables to add
  tmp_add_t=0
  tmp_add_n=0
  tmp_multi_t=0
  tmp_multi_n=0
  #For each hour - we get 2 predictions for addition and multiplication... save in two seperate arrays.
  for (i in 1:rows) {
    # Computing the different specific kernels
    pos_Kernel = (exp(-(matrix_dist_days[i,1]/h[1])^2))
    date_Kernel = (exp(-(matrix_dist_days[i,2]/h[2])^2))
    # time kernel. First Compute time difference.
    hours_tmp = abs(as.numeric(substr(st[["time"]][i],1,2)) - as.numeric(substr(now_tmp,1,2)))
    if (hours_tmp > 12) {
      hours_tmp = 24 - hours_tmp
    }
    time_Kernel = (exp(-((hours_tmp)/h[3])^2))
    add = time_Kernel+pos_Kernel+date_Kernel
    multi= time_Kernel*pos_Kernel*date_Kernel
    tmp_add_t = tmp_add_t + add*st[["air_temperature"]][i]
    tmp_add_n = tmp_add_n + add
    tmp_multi_t = tmp_multi_t + multi*st[["air_temperature"]][i]
    tmp_multi_n = tmp_multi_n + multi
  }
  add_kernel=c(add_kernel, tmp_add_t/tmp_add_n)
  mult_kernel=c(mult_kernel, tmp_multi_t/tmp_multi_n)
}
```



```{r}
par(mfrow = c(1,2))
plot(seq(4,24,2), add_kernel, type="o", xlab="Hour", ylab = "Temperature", main = "Addition")
plot(seq(4,24,2), mult_kernel, type="o", xlab="Hour", ylab = "Temperature", main = "Multiplication")
```
Temperatures seems accurate for a day in november. Conclusion is that chosen smoothing factors was well made. Testing for different months over the years seems to give reasonable values/resluts. 

For the multiplication kernel, the predicition is much more sensible if one kernel goes torwards zero and the difference between points are large. This means that all kernel needs to be somewhat close, otherwise the other values will not be added for the prediction. This also means that the adding predicition evaluates datapoints that for one parameter is far away, meanwhile other parameters are close. This can make the prediction for the addition-kernel much worse. 

# Assignment 2. Neural Networks

```{r}
library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
training <- trva[1:25,] # Training
valid <- trva[26:50,] # Validation
error_Vector=c()
error_Vector_train=c()
#We have 31 edges - give random start weights [-1,1] for each 
weights <- runif(31, -1, 1)
  for(i in 1:10) {
    nn <- neuralnet(Sin~Var, data=training, hidden = 10 , 
                    threshold = i/1000, startweights = weights)   
    fit = compute(nn, valid) #Gives overall result from the training with net.result
    fit_train = compute(nn, training)
    squared_error = sum((valid$Sin - fit$net.result)^2)
    squared_error_train = sum((training$Sin - fit_train$net.result)^2)
    error_Vector = c(error_Vector, squared_error)
    error_Vector_train = c(error_Vector_train, squared_error_train)
  }
par(mfrow = c(1,2))
plot(x=seq(1,10), y=error_Vector, type="o", xlab="Threshold divided by 1000", ylab="Error", main = "Validation", col="red")
plot(x=seq(1,10), y=error_Vector_train, type="o", xlab="Threshold divided by 1000", ylab="", main ="Training", col="blue")
```
In the network we have 31 edges with a single hidden layer of 10 units, so a vector with length 31 with values between -1 and 1 was given for initial weights. 

We can see that the lowest error was with the threshold i = 4 (4/1000) for the Validation data. This might be because this does not give us an overfitted model. It's not a surprise that the error for the training data increase with larger threshold since the model is fitted after the training data. Smaller threshold gives more accurate results for a "perfect" model. 


```{r}
opti=which.min(error_Vector)
#For the best threshold
plot(nn <- neuralnet(Sin~Var, data=training, hidden = 10 , threshold = opti/1000, startweights = weights)) 
plot(nn)
# Plot of the predictions (black dots) and the data (red dots)
first=plot(prediction(nn)$rep1, col="blue")
second=points(valid, col = "red")

```

The predicted and validation data is plotted below. The predicition are in blue and data in red. 