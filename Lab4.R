# Soomin Lee 040899389 CST8233 304
# Lab 4
f <- function(x){log(1+x)}
f_6 <- function(x){return(x-x^2/2+x^3/3-x^4/4+x^5/5-x^6/6)}

userInput_f <- function(){
  userInput_x <- readline(prompt = "Enter x: ")
  userInput_x <- as.numeric(userInput_x)
  return (userInput_x)
}

# plot f(x) and f_6(x)
curve(f, from=0, to=2)
curve(f_6, from=0, to=2, col="red", add=TRUE)

# Maclaurin series
f_Maclaurin <- function(x,n){
  temp <- 0
  for(i in 0:(n-1)){
    temp <- temp + (-1)^i * (x^(i+1)/(i+1))
  }
  return(temp)
}

# take the value of x from user
userInput_x <- userInput_f()
# userInput_x <- 0.32
# find absolute and relative error and print
for(n in 0:10){
   x <- userInput_x
  true_value <- f(x)
  approx <- f_Maclaurin(x,n)
  abs_err <- abs(true_value - approx)
  rel_err <- abs_err/ abs(true_value)
  
  if(n==0){
    cat("n     x    true_value    approx_x   Absolute error   Relative error\n")
    cat("-------------------------------------------------------------------\n")
  }
  else {
    cat(sprintf("%-4d %-6.2f %-12.6f %-12.6f %-12.6f %-20.6f\n",
              n, x, true_value, approx, abs_err, rel_err))
  }
}

