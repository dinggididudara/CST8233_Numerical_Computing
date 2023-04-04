# Soomin Lee 040899389 CST8233 304
# Assignment 3

f <- function(t, y) {
  return (cos(2*t) - 0.2*cos(2*y) - 0.4*sin(2*y) - 2.8*exp(-y))
}

#Euler's method
student.ode.euler <- function(a,b, x0, y0, h){
 
  # calculate how many need to calculate
  n <- ceiling(abs(b-a)/h)
 
  # data frame for result
  result <- data.frame(time = numeric(n+1), approx_value = numeric(n+1),
                       true_value = numeric(n+1), 
                    relative_error = numeric(n+1))

  # initial value
  result$time[1] <- a
  result$approx_value[1] <- y0
  result$true_value[1] <- y0
  result$relative_error[1] <- 0
  
  # calculate rest of the values
  for(i in 1:n){
    # ODE
    y_next <- result$approx_value[i]+(h*f(result$time[i],result$approx_value[i]))
    
    # add to result data frame
    result$time[i+1] <- result$time[i] + h
    result$approx_value[i+1] <- y_next
    result$true_value[i+1] <- f(result$time[i+1], result$approx_value[i+1])
result$relative_error[i+1] <- abs((result$true_value[i+1]-result$approx_value[i+1])
                                    /result$true_value[i+1])
  }
  # change zero percent relative error to NA
  result$relative_error[result$relative_error == 0] <- NA
  print(result)
  
  return (result)
}

# Runge-Kutta 4th order method
student.ode.rk4 <- function(a,b, x0, y0, h){
 
  # calculate how many need to calculate
  n <- round(abs(b-a)/h)
  
  # data frame for result
  result <- data.frame(time = numeric(n+1), approx_value = numeric(n+1),
                       true_value = numeric(n+1), 
                       relative_error = numeric(n+1))
  # initial value
  result$time[1] <- a
  result$approx_value[1] <- y0
  result$true_value[1] <- y0
  result$relative_error[1] <- 0
  
  # calculate 4th order method
  for(i in 1:n){
    k1 <- f(result$time[i], result$approx_value[i])
    k2 <- f(result$time[i] + (h/2), result$approx_value[i] + (0.5*k1*h))
    k3 <- f(result$time[i]+(h/2), result$approx_value[i] + (0.5*k2*h))
    k4 <- f(result$time[i]+h, result$approx_value[i] + (k3*h))
    
    y_next <- result$approx_value[i] + (1/6)*(k1+(2*k2)+(2*k3)+k4)
  
    # add to result data frame
    result$time[i+1] <- result$time[i] + h
    result$approx_value[i+1] <- y_next
    result$true_value[i+1] <- f(result$time[i+1], result$approx_value[i+1])
    result$relative_error[i+1] <- abs((result$true_value[i+1]-result$approx_value[i+1])
                                      /result$true_value[i+1])
  }
  # change zero percent relative error to NA
  result$relative_error[result$relative_error == 0] <- NA
  print(result)
  
  return (result)
}


# accept initial values
x0 <- readline("Enter initial data point x: ")
x0 <- as.double(x0)

y0 <- readline(prompt = "Enter initial data point y: ")
y0 <- as.double(y0)

# calculate
result1_e <- student.ode.euler(0,3, x0, y0, 0.8)
result2_e <- student.ode.euler(0,3, x0, y0, 0.2)
result3_e <- student.ode.euler(0,3, x0, y0, 0.05)

# plot
plot(result1_e$time, result1_e$approx_value, col="red", ylim=c(-2,4), 
     type="p", main="Euler's Method", pch=1, xlab="Time", ylab="temperature")
points(result2_e$time, result2_e$approx_value,col="blue", pch=6)
points(result3_e$time, result3_e$approx_value,col="green", pch=3)
# points(result1_e$time, result1_e$true_value,col="brown", pch=1)
# points(result2_e$time, result2_e$true_value,col="brown", pch=6)
points(result3_e$time, result3_e$true_value,col="brown", pch=3)

# calculate
result1_r <- student.ode.rk4(0,3, x0, y0, 0.8)
result2_r <- student.ode.rk4(0,3, x0, y0, 0.2)
result3_r <- student.ode.rk4(0,3, x0, y0, 0.05)

# plot
plot(result1_r$time, result1_r$approx_value, col="red", ylim=c(-7,10), 
       type="p", main="Runge-Kutta 4 Method", pch=1, xlab="Time", ylab="temperature")
points(result2_r$time, result2_r$approx_value,col="blue", pch=6)
points(result3_r$time, result3_r$approx_value,col="green", pch=3)
# points(result1_r$time, result1_r$true_value,col="brown", pch=1)
# points(result2_r$time, result2_r$true_value,col="brown", pch=6)
points(result3_r$time, result3_r$true_value,col="brown", pch=3)