# Soomin Lee 040899389 CST8233 304
# Task 1 --------------------------------------------------------------
library(dplyr)
library(datasets)

cerealsDF <- read.csv(file="Lab\\assignment1.csv", sep=";")
str(cerealsDF)      # display the structure of data frame
names(cerealsDF)
cerealsDF[1:10,]    # display first ten rows
cerealsDF <- cerealsDF[-1,]      # delete the data type line and print
cerealsDF <- cerealsDF %>% mutate(totalcarbo = as.double(carbo) + as.double(sugars))
                                            # add column of total carbo
length(subset(cerealsDF, type == 'H'))      # find how many cereals are hot
unique(cerealsDF$mfr) # find how many unique manufacturers
cereals_K <- cerealsDF %>% filter(mfr=='K') # extract all cereals that are by K (Kellogg)

# extract all cereals have less than / equal 90 calories and have more than 2 units of fat
new_df <- subset(cerealsDF, (calories) <= 90 & (fat) > 2) 
write.csv(new_df, "Lab\\Assignment1-task1.csv") # save this subset
  
  

