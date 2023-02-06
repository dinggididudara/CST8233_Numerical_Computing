# Soomin Lee 040899389 CST8233 304
# Task 2 : Vectors, Functions, Series ---------------------------
library("PolynomF")

MyIntCal <- function (x){
  return (cos(0.5)*x * exp(0.1*x))
}


# what is degree of interpolating function
MyIntFig.pdf <- plot()# create 4 x 2 figure and ;lot each of these ploynomials
pf_x <- ploy.calc(MyIntCal())# find interpolating function
MyIntCal(15)
MyIntCal(24)