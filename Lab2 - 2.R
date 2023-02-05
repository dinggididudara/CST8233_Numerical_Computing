# Soomin Lee 040899389 CST8233 304
# Exercise 2 -------------------------------- #
f <- function(i)
{
  return ((2^i/i) + (3^i/i^2))
}
sum2 <- sum(f(1:25))

cat("The sum of this summation is: ", sum2)
