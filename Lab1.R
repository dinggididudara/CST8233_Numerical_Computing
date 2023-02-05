v1 <- c(2,4,6)
v2 <- c(7,9,11)
v3 <- c(13,15,17)
cbind(v1,v2,v3)

student <- data.frame(
            Name = c("Michael", "Jennifer", "Sara", "James"),
            Gender = c("M", "F", "F", "M"),
            Age = c(18, 19, 20, 22),
            Designation = c("CET Student", "CP Student", "SSN Student", "CS Student"),
            NoCourses = c(5, 4, "<NA>", 3)
)
print(student)
