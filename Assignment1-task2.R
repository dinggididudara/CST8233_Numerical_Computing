# Soomin Lee 040899389 CST8233 304
# Task 2 : Vectors, Functions, Series ---------------------------

# <- function (x){
#   return (cos(0.5)*x * exp(0.1*x))
#  }

library("PolynomF")

# data points
x <- c(pi, 6.678, 3*pi, 12.961, 5*pi, 19.244, 7*pi)
y <- c(0,-1.921, 0, 3.584, 0, -6.718, 0)

MyIntCal <- approxfun(x,y)    # create interpolating function

pdf(file="Lab\\MyIntFig.pdf", width=4, height=2) # create 4 x 2 figure and
plot(x, y)      
curve(MyIntCal,add=TRUE)
points(x,y, col="red", pch = 16)  # lot each of these polynomials
dev.off()         # close to create the file
 
pf_x <- poly_calc(x,y)    # find interpolating function
plot(pf_x)
curve(MyIntCal, add=TRUE, col="green")


MyIntCal(15)
MyIntCal(24)    





 