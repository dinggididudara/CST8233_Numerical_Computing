# Soomin Lee 040899389 CST8233 304
# Task 2 : Vectors, Functions, Series ---------------------------

# <- function (x){
#   return (cos(0.5)*x * exp(0.1*x))
#  }

library("PolynomF")

# data points
x <- c(pi, 6.678, 3*pi, 12.961, 5*pi, 19.244, 7*pi)
y <- c(0,-1.921, 0, 3.584, 0, -6.718, 0)

# create interpolating function
MyIntCal <- approxfun(x,y)   

# find derivation
lag <- lagrange(x,y)         
deriv(lag)


# create figure
pdf(file="Lab\\MyIntFig.pdf", width=10, height=10 )

plot_lag <- function(a, b, i) {
  lag <- lagrange(a, b)
  plot(lag, xlim = c(min(x), max(x)), ylim = c(min(y), max(y)), 
       main = paste("Lagrange Polynomial", i))
}
par(mfrow=c(4,2)) # add area

for(i in 1:6){    # make multiple plots
  plot_lag(x[i:(i+1)], y[i:(i+1)], i)
}

dev.off()         # close to create the file
 

# find interpolating function
pf_x <- poly_calc(x,y)    

# find the result
MyIntCal(15)
pf_x(15)
MyIntCal(24)
pf_x(24)





 