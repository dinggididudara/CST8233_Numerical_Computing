# Soomin Lee 040899389 CST8233 304
# x^3-3*x^2-2*x+7
library("PolynomF")
p <- PolynomF::polynom(c(7,-2,-3,1))
class(p)  # class of the variable p is polynom
coef(p)   # find coefficients 7,-2,-3,1

# y^2+2y
q <- PolynomF::polynom(c(0,2,1))
class(q)

p+q   # 7 - 2*x^2 + x^3 
p-q   # 7 - 4*x - 4*x^2 + x^3 
p*q   # 14*x + 3*x^2 - 8*x^3 - x^4 + x^5 

dpdx <- deriv(p)
dpdy <- deriv(q)

curve(p, from=-2, to=3, ylab="p(x),dpdx")
curve(dpdx, col="red", from=-2, to=3, ylab="p(x),dpdx", add=TRUE)
abline(h=0, col="gray")