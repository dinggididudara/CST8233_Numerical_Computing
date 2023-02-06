# Soomin Lee 040899389 CST8233 304
# Statistics in R
library(dplyr)
library(datasets)

my_df <- data.frame(airquality)
head(str(airquality))                    # first six lines
names(my_df)                             # list columns' name

my_df_temp <- my_df %>% select(Temp,Month)  # select temperature column

my_df_temp %>%
  group_by(Month) %>% filter(Month==6 | Month==7 | Month==8)%>%      # Temp mean in June, July and August
   summarise(mean = mean(Temp), median = median(Temp), standard_deviation = sd(Temp))     

May_To_Sep <- my_df_temp %>%
  group_by(Month) %>% filter(Month==5 | Month==6 | Month==7 | Month==8 | Month==9)

df_mean <- mean(May_To_Sep$Temp)
df_sd <- sd(May_To_Sep$Temp)
pnorm(70, df_mean, df_sd)                                 # less than 70
pnorm(85, df_mean, df_sd, lower.tail=FALSE)               # greater than 85
pnorm(90, df_mean, df_sd) - pnorm(75, df_mean, df_sd)     # less than 90, greater than 75
