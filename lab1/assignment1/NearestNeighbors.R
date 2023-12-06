# """""""""""""""""""""""""""""" Assignment 1 """""""""""""""""""""""""""""" #

library(kknn)

#Read data from file
data <- read.csv("optdigits.csv", header=FALSE)

#Create training set (50% of data)
n <- dim(data)[1]
set.seed(12345) 
id <- sample(1:n, floor(n * 0.5))
train <- data[id,] 

#Create validation set (25% of data)
id1 <- setdiff(1:n, id)
set.seed(12345) 
id2 <- sample(id1, floor(n * 0.25)) 
valid <- data[id2,]

#Create test set (25 of data)
id3 <- setdiff(id1, id2)
test <- data[id3,] 


# """""""""""""""""""""""""""""" Assignment 2 """""""""""""""""""""""""""""" #


#Set formula for the models, which is the last column of the training set
formula_string <- as.formula(paste("as.factor(", colnames(train)[ncol(train)], ") ~ ."))

#Create model for train and test (model for test is model and model for train is model1)
model <- kknn(formula_string, train, test, kernel="rectangular", k = 30)
model1 <- kknn(formula_string, train, train, kernel="rectangular", k = 30)

#Predict train and test based on their model
train_predictions <- as.numeric(predict(model1, newdata = train))
test_predictions <- as.numeric(predict(model, newdata = test))

#Find target number for train and test, which is in their last column
trainingTarget <- as.numeric(train$V65)
testTarget <- as.numeric(test$V65)

#Create confusion matrix for train and test based on their target and predictions
confusion_matrix_train <- table(trainingTarget, train_predictions)
confusion_matrix_test <- table(testTarget, test_predictions)
print(confusion_matrix_test)
print(confusion_matrix_train)

#Calculate misclassification for train and test
misclassification_error_train = 1 - sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
misclassification_error_test = 1 - sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)


# """""""""""""""""""""""""""""" Assignment 3 """""""""""""""""""""""""""""" #


#Find all indices of target 8 on the training set
search_num = 8
#extracts row if the last column of the dataset  is 8
indices_8 <- which(train[, ncol(train)] == search_num)

#Get all probabilities of a number being an eight aswell as getting those probabilities where the answer should be 8
probabilities <- model1$prob[, as.character(search_num)]
probabilities_8 <- probabilities[indices_8]

# Find the indices of the easiest and hardest cases in the original dataset 
easiest_cases <- indices_8[order(probabilities_8, decreasing = TRUE)[1:2]]
hardest_cases <- indices_8[order(probabilities_8, decreasing = FALSE)[1:3]]

# Extract features for the easiest and hardest cases
easiest_features <- train[easiest_cases, -ncol(train)]
hardest_features <- train[hardest_cases, -ncol(train)]

reshape_as_matrix <- function(features) {
  matrix(as.numeric(features), nrow = 8, byrow = TRUE)
}

#Matrix is upside down without having this function
reverse_rows <- function(mat) {
  mat[nrow(mat):1, ]
}

#Create a heatmap for each case
for (i in 1:2) {
  heatmap(reverse_rows(reshape_as_matrix(easiest_features[i, ])), Colv = NA, Rowv = NA,
          main = paste("Easiest Case", i), xlab = "", ylab = "")
}

for (i in 1:3) {
  heatmap(reverse_rows(reshape_as_matrix(hardest_features[i, ])), Colv = NA, Rowv = NA,
          main = paste("Hardest Case", i), xlab = "", ylab = "")
}


# """""""""""""""""""""""""""""" Assignment 4 """""""""""""""""""""""""""""" #


vector_nums <- c(1:30)
misclassification_error_train_list <- numeric(length(vector_nums))
misclassification_error_validation_list <- numeric(length(vector_nums))
optimal_k = NULL

# Loop through each value of k in the vector_nums
for (num in vector_nums) {
  # Train a k-nearest neighbors model on the training and validation sets
  model <- kknn(formula_string, train, valid, kernel="rectangular", k = num)
  model1 <- kknn(formula_string, train, train, kernel="rectangular", k = num)
  
  # Make predictions on the training and validation sets
  train_predictions <- as.numeric(predict(model1, newdata = train))
  validation_predictions <- as.numeric(predict(model, newdata = valid))
  
  # Extract target variables
  trainingTarget <- as.numeric(train$V65)
  validationTarget <- as.numeric(valid$V65)

  # Create confusion matrices for training and validation
  confusion_matrix_train <- table(trainingTarget, train_predictions)
  confusion_matrix_validation <- table(validationTarget, validation_predictions)
  
  # Calculate misclassification error and sore it in the missclassification lists
  misclassification_error_validation <- 1 - sum(diag(confusion_matrix_validation)) / sum(confusion_matrix_validation)
  misclassification_error_train_list[num] = 1 - sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
  misclassification_error_validation_list[num] = 
    
    # Update optimal_k if the current k value has lower misclassification error  
  if (is.null(optimal_k) || misclassification_error_validation < misclassification_error_validation_list[optimal_k]) 
    optimal_k <- num
}
print(optimal_k)

# Plot misclassification errors for training and validation sets against k values
plot(vector_nums, misclassification_error_train_list, ylab = "misclassification error", xlab = "k", col="green")
points(vector_nums, misclassification_error_validation_list, col="red")

# Train final models on the optimal k value for training, validation, and test sets
model <- kknn(formula_string, train, valid, kernel="rectangular", k = optimal_k)
model1 <- kknn(formula_string, train, train, kernel="rectangular", k = optimal_k)
model2 <- kknn(formula_string, train, test, kernel="rectangular", k = optimal_k)

# Make predictions on the training, validation, and test sets
train_predictions <- as.numeric(predict(model1, newdata = train))
test_predictions <- as.numeric(predict(model2, newdata = test))
validation_predictions <- as.numeric(predict(model, newdata = valid))

# Extract target variables for training, validation, and test sets
trainingTarget <- as.numeric(train$V65)
testTarget <- as.numeric(test$V65)
validationTarget <- as.numeric(valid$V65)

# Create confusion matrices for training, test, and validation sets
confusion_matrix_train <- table(trainingTarget, train_predictions)
confusion_matrix_test <- table(testTarget, test_predictions)
confusion_matrix_validation <- table(validationTarget, validation_predictions)

# Calculate misclassification errors for training, test, and validation sets
misclassification_error_train = 1 - sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
misclassification_error_test = 1 - sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
misclassification_error_validation = 1 - sum(diag(confusion_matrix_validation)) / sum(confusion_matrix_validation)

print(misclassification_error_test)
print(misclassification_error_train)
print(misclassification_error_validation)


# """""""""""""""""""""""""""""" Assignment 5 """""""""""""""""""""""""""""" #


cross_entropy_errors <- vector("numeric", length = 30)

for (k in 1:30) {
  model <- kknn(formula_string, train, valid, k = k)
  
  # Extract probabilities for all instances in the validation set
  probabilities <- model$prob
  
  # Probability smoothing
  eps <- 1e-15
  probabilities[probabilities == 0] <- eps
  
  # Assuming valid[, target_column] represents the true classes
  true_number <- as.numeric(as.factor(valid$V65))
  
  # Calculate cross-entropy
  cross_entropy_errors[k] <- -sum(log(probabilities[cbind(1:nrow(valid), true_number)]))
  
}

# Plot the dependence of cross-entropy on K
plot(1:30, cross_entropy_errors, type = "l", col = "blue", xlab = "K", ylab = "Cross-Entropy", main = "Cross-Entropy vs. K")

# Find the optimal K based on the minimum cross-entropy
optimal_k <- which.min(cross_entropy_errors)
cat("Optimal K:", optimal_k, "\n")

