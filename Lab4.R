# Soomin Lee 040899389 CST8233 304
# Lab 4
library(dplyr)
f(x) <- log(1+x)

# write first six terms of this series
for(n in 1 : 6){
  f_6(x) <- exp(-1,n) * x^(n+1) / (n+1)
}

# plot f(x) and f_6(x)
plot(f(x))
plot(f_6(x))

# take the value of x from user (using for loop)
# find absolute and relative error and print
count <- 0
print("n   x   approx_x          Absolute error    Relative error")
print("------------------------------------------------------------")
for(i in 1:10){
  count <- count + 1
  userInput_X <- readline('Enter x (xx.x) :')
  userInput_X <- as.numeric(userInput_X)
  userInput_n <- readline('Enter n (x) : ')
  userInput_n <- as.numeric(userInput_n)
  
  print(count + "  " + userInput_X + " " + "  " + " ")
}