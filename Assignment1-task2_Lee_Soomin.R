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
MyIntCal <- function(x,y,xi) {
  
}

# find derivation n-1
degree <- length(x)-1
degree

# create figure
# pdf("..\\Lab\\MyIntFig.pdf", width=10, height=10)

# Plot the 7 Lagrange polynomials

for(i in 1:7){
  curve(MyIntCal(x,y,x[i]))
}

par(mfrow=c(4,2))
# dev.off()         # close to create the file

# find interpolating function
pf_x <- poly_calc(x,y)    

# find the result
MyIntCal(15)
pf_x(15)

MyIntCal(24)
pf_x(24)


 