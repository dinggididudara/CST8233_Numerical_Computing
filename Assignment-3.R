# Soomin Lee 040899389 CST8233 304
# Assignment 3

f <- function(t,y) {
  return (cos(2*t) - (0.2*cos(2*y) + 0.4*sin(2*y) + 2.8*exp(-y)))
}
f2 <- function(t){
  return (0.2*cos(2*y) + 0.4*sin(2*t) + 2.8*exp(-t))
}
#Euler's method
student.ode.euler <- function(a,b){
  # accept initial values
  x0 <- readline(prompt = "Enter initial data point x: ")
  x0 <- as.numeric(x0)
  
  y0 <- readline(prompt = "Enter initial data point y: ")
  y0 <- as.numeric(y0)
  
  h <- readline(prompt = "Enter step size h: ")
  h <- as.numeric(h)
  
  # calculate how many need to calculate
  n <- round((b-a)/h)
  
  # data frame for result
  result <- data.frame(time = numeric(n+1), approx_value = numeric(n+1),
                       true_value = numeric(n+1), 
                    relative_error = numeric(n+1))

  # initial value
  result$time[1] <- x0
  result$approx_value[1] <- y0
  result$true_value[1] <- f2(x0)
  result$relative_error[1] <- 0
  
  # calculate rest of the values
  for(i in 1:n){
    y_next <- result$approx_value[i] 
                    + h*f(result$time[i],result$approx_value[i])
    
    # add to result data frame
    result$time[i+1] <- result$time[i] + h
    result$approx_value[i+1] <- y_next
    result$true[i+1] <- f2(result$time[i+1])
    result$relative_error[i+1] <- abs((result$true[i+1]-result$approx[i+1])
                                    /result$true[i+1]) * 100
  }
  # change zero percent relative error to NA
  result$relative_error[result$relative_error == 0] <- NA
  print(result)
  
  # plot
  
  
  return (result)
}

# Runge-Kutta 4th order method
student.ode.rk4 <- function(a,b){
  # accept initial values
  x0 <- readline(prompt = "Enter initial data point x: ")
  x0 <- as.numeric(x0)
  
  y0 <- readline(prompt = "Enter initial data point y: ")
  y0 <- as.numeric(y0)
  
  h <- readline(prompt = "Enter step size h: ")
  h <- as.numeric(h)
  
  # calculate how many need to calculate
  n <- round((b-a)/h)
  
  # data frame for result
  result <- data.frame(time = numeric(n+1), approx_value = numeric(n+1),
                       true_value = numeric(n+1), 
                       relative_error = numeric(n+1))
  # accept initial values
  x0 <- readline(prompt = "Enter initial data point x: ")
  x0 <- as.numeric(x0)
  
  y0 <- readline(prompt = "Enter initial data point y: ")
  y0 <- as.numeric(y0)
  
  h <- readline(prompt = "Enter step size h: ")
  h <- as.numeric(h)
  
  # initial value
  result$time[1] <- x0
  result$approx_value[1] <- y0
  result$true_value[1] <- f2(x0)
  result$relative_error[1] <- 0
  
  # calculate 4th order method
  for(i in 1:n){
    k1 <- f(result$time[i], result$approx[i])
    k2 <- f(result$time[i] + h/2, result$approx[i] + 0.5*k1*h)
    k3 <- f(result$time[i]+h/2, result$approx[i] + 0.5*k2*h)
    k4 <- f(result$time[i]+h, result$approx[i] + k3*h)
    
    y_next <- result$approx[i] + (1/6)*(k1+2*k2+2*k3+k4)
  
    # add to result data frame
    result$time[i+1] <- result$time[i] + h
    result$approx_value[i+1] <- y_next
    result$true[i+1] <- f2(result$time[i+1])
    result$relative_error[i+1] <- abs((result$true[i+1]-result$approx[i+1])
                                      /result$true[i+1]) * 100
  }
  # change zero percent relative error to NA
  result$relative_error[result$relative_error == 0] <- NA
  print(result)
  
  # plot
  
  return (result)
 
}

student.ode.euler(3,0)
student.ode.rk4(3,0)
