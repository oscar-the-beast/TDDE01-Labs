set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "latin1")

temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

#Smoothing coefficients
h_distance <- 100000
h_date <- 30
h_time <- 5


a <- 58.4274 # The point to predict (up to the students)
b <- 14.826

p_date <- "2013-11-02" # The date to predict (up to the students)
p_times <- c("04:00:00", "06:00:00", "08:00:000", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
sum_temp <- c()
mult_temp = c()

#Prediction coordinates and date
x = 58.710 #latitude
y = 15.564 #longitude


for(i in 1:11){

  p_time = p_times[i] #set the time
  prior_measurements = st[paste(st$date, st$time) <= paste(p_date, p_time),] #check which measurements that should be included

  #Distance between sample and point of interest
  distance_diff = distHaversine(cbind(prior_measurements[, 4:5]), cbind(x, y))#calculate the physical distance between samples and points in meters
  distance_kernel = exp(-(distance_diff/h_distance)^2) #Calculate the kernel value
  #plot(distance_diff, distance_kernel, type = "p", xlab = "Distance in meters", ylab = "Kernel value") #plot the kernel values
  
  #Kernel which account for day of measurement and day of interest
  day_diff = as.numeric(difftime(p_date, prior_measurements$date, unit = "days")) %% 365 #difftime calculates the number of days between the sample and target date, stores the rest after dividing with 365 to obtain yearly patterns in the weather
  day_kernel = exp(-(day_diff/h_date)^2) #calculate the kernel value
  #plot(day_diff, day_kernel, type = "p", xlab = "Days", ylab = "Kernel value") #plot the kernel values
  
  #Kernel that account for the time the sample was observed and the hour of interest
  hour_diff = abs(as.numeric(difftime(as.POSIXct(p_time, format = "%H:%M:%S"), as.POSIXct(prior_measurements$time, format = "%H:%M:%S"), unit = "hours"))) #calculates the number of hours between sample and target hour
  hour_kernel = exp(-(hour_diff/h_time)^2) #calculate the kernel value
  #plot(hour_diff, hour_kernel, type = "p", xlab = "Hours", ylab = "Kernel value") #plot the kernel values
  
  sum_kernels = distance_kernel + day_kernel + hour_kernel
  sum_temp[i] = sum(sum_kernels %*% prior_measurements$air_temperature)/sum(sum_kernels)
  
  mult_kernels = distance_kernel *day_kernel * hour_kernel
  mult_temp[i] = sum(mult_kernels %*% prior_measurements$air_temperature)/sum(mult_kernels) 
}

#Determine plot limits for the y-axis
y_max = max(c(sum_temp, mult_temp))
y_min = min(c(sum_temp, mult_temp))
ylim = c(y_min*0.9, y_max*1.1)


# Plot both predictors predictions in one graph
plot(mult_temp, ylim=ylim, type="p", col="blue", pch=16, xlab="Time", ylab="Degrees celsius", xaxt="n")

axis(1, at=1:length(p_times), labels=p_times)

points(sum_temp, type="p", col="red", pch=16)

legend("topright", legend=c("mult_temp", "sum_temp"), col=c("blue", "red"), pch=16)

