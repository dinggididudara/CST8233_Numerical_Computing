# Soomin Lee 040899389 CST8233 304
# Lab 5

library(readxl)

# plot data, time and altitude
ascent_data <- readxl::read_excel("Lab/Ascent_STS121_Data.xlsx")

# Time vs Altitude
plot(ascent_data$`Time\r\n(seconds)`, ascent_data$`Altitude\r\n(meters)`,
     xlab="Time", ylab="Altitude", main="Time vs Altitude", pch=4)

# functions
student.numDiff.fdd <- function(t,a){
  n <- length(t)
  velocity <- NA
  for(i in 1:(n-1)){
    velocity[i] <- (a[i+1] - a[i] ) / (t[i+1] - t[i])
  }
  velocity[n] <- NA
  return (velocity)
}

student.numDiff.bdd <- function(t,a){
  n <- length(t)
  velocity <- NA
  for(i in 2:n){
    velocity[i] <- (a[i] - a[i-1] ) / (t[i] - t[i-1])
  }
  velocity[1] <- NA
  return (velocity)
}


student.numDiff.cdd <- function(t,a){
  n <- length(t)
  velocity <- NA
  for(i in 2:n){
    velocity[i] <- (a[i+1] - a[i-1] ) / (t[i+1] - t[i-1]) * 0.5
  }
  return (velocity)
}


student.numDiff.secDer <- function(t,a){
  n <- length(t)
  acceleration <- NA
  for(i in 2:(n-2)){
    acceleration[i] <- (a[i+1] - 2*a[i] + a[i-1] ) /
      ((t[i+1] - t[i])*(t[i]-t[i-1]))^2
  }
  acceleration[n] <- NA
  acceleration[n-1] <- NA
  acceleration[n-2] <- NA
  return (acceleration)
}

# plot the velocity using 3 functions, overlaying data points
velocity.fdd <- student.numDiff.fdd(ascent_data$`Time\r\n(seconds)`, 
                                    ascent_data$`Altitude\r\n(meters)`)
velocity.bdd <- student.numDiff.bdd(ascent_data$`Time\r\n(seconds)`, 
                                    ascent_data$`Altitude\r\n(meters)`)
velocity.cdd <- student.numDiff.cdd(ascent_data$`Time\r\n(seconds)`, 
                                    ascent_data$`Altitude\r\n(meters)`)

# Time vs Velocity, 3 functions
plot(ascent_data$`Time\r\n(seconds)`, velocity.fdd, col = "red", type="p",
     main = "Time vs Velocity", pch=1, xlab="Time(seconds)", ylab="Velocity")
points(ascent_data$`Time\r\n(seconds)`, velocity.bdd, col = "blue", pch=2)
points(ascent_data$`Time\r\n(seconds)`, velocity.cdd, col = "green", pch=4)

# Time & Acceleration
acceleration <- student.numDiff.secDer(ascent_data$`Time\r\n(seconds)`, ascent_data$`Altitude\r\n(meters)`)
plot(ascent_data$`Time\r\n(seconds)`, acceleration, 
     xlab="Time", ylab="Acceleration", main="Time vs Acceleration")
