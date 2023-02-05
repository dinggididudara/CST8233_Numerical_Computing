# Soomin Lee 040899389 CST8233 304
# Exercise 1 ------------------------------- #
cVec <- function(x)
{
  return(0.1*exp(x)*cos(x)+2*log(abs(x)))
}
temp <- seq(from=3.0,to=6.0, by=0.1)
sum1 <- sum(cVec(temp))

cat("The sum of this vector is : ", sum1)

plot(temp,cVec(temp), xlab="x", ylab="y", main="My First Plot")



