# Soomin Lee 040899389 CST8233 304
# Exercise 4 -------------------------------- #
myFun <- function(Vec1){
  ifelse(Vec1<0, (Vec1^2+2*Vec1+3),
         ifelse((Vec1>=0)|(Vec1<2), (Vec1+3), 
                ifelse(Vec1>=2, (Vec1^2+4*Vec1-7), FALSE)))
}
fun_range <- head(-4:4,-1)
plot(fun_range,myFun(fun_range), main = "Exercise 4")