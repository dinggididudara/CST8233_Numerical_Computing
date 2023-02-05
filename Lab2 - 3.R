# Soomin Lee 040899389 CST8233 304
# Exercise 3 -------------------------------- #
set.seed(75)
Vec1 <- sample(0:999,100)
Vec2 <- sample(0:999,100)
print(Vec1)
print(Vec2)
# a ------------------------------------------#
for(i in 1:100){         # check all elements
  if(Vec1[i] > 600){
    Vec2a <- c(Vec1[i])  # save to another vector
  }
}
print(Vec2a)
# b ------------------------------------------#
for(i in 1:100){
  for(j in 1:100){       # save the index number
    Vec2b <- which(Vec2a[i] == Vec2[j])
  }                     
}
print(Vec2b)
# c ------------------------------------------#
Vec1c <- vector("numeric",100)
for(i in 1:100){
  if(Vec2a[i]==Vec1[i]){
     Vec1c <- Vec1[i] 
  }
}
print(Vec1c)
# d ------------------------------------------#
count <- 0
for(i in 1:100){
  if(Vec1[i] %% 2 == 0){   # if remainder is zero
    count <- count + 1
  }
}

print(count)