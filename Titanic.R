titanic <- read.csv("H:\\Downloads\\titanic.csv", sep=",")

survived <- table(titanic$Survived)

pie(
  survived,
  labels =  c("Not survived", "Survived"),
  col = c("red", "green")
  )

library(randomForest)
library(rpart)
library(rpart.plot)

pclass_counts <- table(titanic$Pclass)


barplot(
  pclass_counts,
  col = c("blue", "orange", "purple"),
  main = "Distribution of Passenger Classes",
  xlab = "Passenger Class",
  ylab = "Number of Passengers",
  names.arg = c("1st Class", "2nd Class", "3rd Class")
)
