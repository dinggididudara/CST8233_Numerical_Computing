# Soomin Lee 040899389 CST8233 304
# Lab 6

# function for composite trapezoid rule
student.trapezoid <- function(x0,xn,n){
  h <- (xn-x0)/n
  xi <- seq(x0, xn, by = h)
  fi <- xi^(2/3)
  integral <- (h/2) * (fi[1]+2*sum(fi[2:n]+fi[n+1]))
  return(integral)
}

#function for simpson's composite rule
student.simpsons <- function(x0,xn,n){
  if (n == 2 || n %% 2 != 0) {
    return(NA)
  }
  h <- (xn - x0) / n
  xi <- seq(x0, xn, by = h)
  fi <- xi^(2/3)
  integral <- (h / 3) * (fi[1] + 4 * sum(fi[2:n-1]) + 2 * sum(fi[3:n-2]) + fi[n+1])
  return(integral)
  
}

n_values <- c(2,2^2,2^3,2^4,2^5,2^6,2^7,2^8,2^9)
trapezoid_values <- sapply(n_values, function(n) student.trapezoid(0, 1, n))
simpsons_values <- sapply(n_values, function(n) student.simpsons(0, 1, n))
exact_value <- 0.6

data.frame(n = n_values, 
           Trapezoid = trapezoid_values, 
           T_rel_error = abs(trapezoid_values - exact_value) / exact_value * 100, 
           Simpsons = simpsons_values, 
           S_rel_error = abs(simpsons_values - exact_value) / exact_value * 100)