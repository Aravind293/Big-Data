vote <- read.csv("H:\\Downloads\\titanic.csv")

str(vote)


pie(table(vote$Survived),
    col=c("red","green"),
    labels = c("survived", "notsurvived"))
barplot(table(vote$Survived,vote$Pclass),
        col=c("red","green"),
        beside = TRUE)
barplot(table(vote$Sex,vote$Survived),
        col=c("pink","black"),
        beside = TRUE)
# Create a new dataframe for passengers without an Age value
PassengersWithoutAge <- vote[!is.na(vote$Age), ]

str(PassengersWithoutAge)

# Filter passengers who have a non-null Age and Parch == 0
PassengersWithNonNullAgeAndParchZero <- PassengersWithoutAge[PassengersWithoutAge$Parch == 0, ]

# Display the structure of the filtered dataset
str(PassengersWithNonNullAgeAndParchZero)



# Filter to keep only passengers with a non-null Age and Parch == 0
PassengersFiltered <- PassengersWithNonNullAgeAndParchZero[ PassengersWithNonNullAgeAndParchZero$Parch == 0, ]

# Display the structure of the filtered dataset
str(PassengersFiltered)

# Add a new variable AgeGroup: Child if Age < 17, Adult otherwise
PassengersFiltered$AgeGroup <- ifelse(PassengersFiltered$Age >= 17, "Adult", "Child")

# Display the structure of the updated dataset
str(PassengersFiltered)

# Verify by viewing a sample of the dataset
head(PassengersFiltered)

# Summary of AgeGroup to ensure it's correctly assigned
table(PassengersFiltered$AgeGroup)

# Assuming you have already created the AgeGroup variable

# Create a side-by-side barplot to represent the influence of AgeGroup on Survived
barplot(table(PassengersFiltered$AgeGroup, PassengersFiltered$Survived),
        beside = TRUE,                   # For side-by-side bars
        col = c("red", "green"),         # Colors for "Not Survived" (red) and "Survived" (green)
        legend = c("adult", "child"), # Legend labels
        main = "Influence of AgeGroup on Survival", # Title
        xlab = "Age Group",              # X-axis label
        ylab = "Count")                  # Y-axis label

# Add a new variable Family: Alone if SibSp == 0 and Parch == 0, WithAFamily otherwise
PassengersFiltered$Family <- ifelse(PassengersFiltered$SibSp == 0 & PassengersFiltered$Parch == 0, 
                                    "Alone", 
                                    "WithAFamily")

# Display the structure of the updated dataset to verify the new column
str(PassengersFiltered)

# Verify by viewing a sample of the updated dataset
head(PassengersFiltered)

# Summary of the Family variable to ensure it's correctly assigned
table(PassengersFiltered$Family)
# Create a side-by-side barplot to represent the influence of Family on Survived
barplot(table(PassengersFiltered$Family, PassengersFiltered$Survived),
        beside = TRUE,                     # Side-by-side bars
        col = c("red", "green"),           # Colors for Survived and Not Survived
        legend = c("family", "alone"), # Legend labels
        main = "Influence of Family on Survival",   # Title
        xlab = "Family",                   # X-axis label
        ylab = "Count")                    # Y-axis label

# Add a new variable CabinGroup: First letter of Cabin if not empty, "X" otherwise
PassengersFiltered$CabinGroup <- ifelse(!is.na(PassengersFiltered$Cabin) & 
                                          nchar(PassengersFiltered$Cabin) > 0, 
                                        substr(PassengersFiltered$Cabin, 1, 1), 
                                        "X")

# Display the structure of the updated dataset to verify the new column
str(PassengersFiltered)

# Verify by viewing a sample of the updated dataset
head(PassengersFiltered)

# Summary of the CabinGroup variable to ensure it's correctly assigned
table(PassengersFiltered$CabinGroup)

set.seed(123)
train_indices <- sample(2, nrow(PassengersFiltered), prob = c(0.7, 0.3), replace = TRUE)

# Create the training set by subsetting the data
train_set <- PassengersFiltered[train_indices == 1, ]

# Create the test set by excluding the training set rows
test_set <- PassengersFiltered[-train_indices == 2, ]

# Verify the sizes of the training and test sets
dim(train_set)  # Size of the training set
dim(test_set)   # Size of the test set

library(randomForest)

# Create the random forest model using the training set
# Assuming 'Survived' is the target variable and other columns are features
rf_model <- randomForest(Survived ~ ., data = train_set)

# Print the summary of the random forest model
print(rf_model)

# Predict on the test set
rf_predictions <- predict(rf_model, newdata = test_set, type = "classification")

# Evaluate the model by comparing predictions with actual values
confusion_matrix <- table(Predicted = rf_predictions, Actual = test_set$Survived)
print(confusion_matrix)

# Optionally, calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")
# Accuracy Calculation
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

