library(glmnet)
library(dplyr) 

#Read data from csv file
tecator = read.csv("tecator.csv", header = TRUE)


#Spliting data 50%/50% between training and testing
n <- dim(tecator)[1]
set.seed(12345)
id <- sample(1:n, floor(n * 0.5))
trainingData <- tecator[id, ]
testData <- tecator[-id, ]

#Removing unessecary columns from the training & test data such as sample and others
trainingData = trainingData  %>% select(2:102)
testData = testData  %>% select(2:102)


##### Task 1 #####
#################

#Create a linear model 
m1 = lm(Fat~., data=trainingData)
summary(m1) #displays the model coefficients

#make predictions on training and test data
pred_training = predict(m1, trainingData)
pred_test = predict(m1, testData)

#Calculate Mean Square Error, by comparing predicted values with real values
MSE_training = mean((trainingData$Fat - pred_training)^2)
MSE_test = mean((testData$Fat - pred_test)^2)


##### Task 2 #####
#################


#Cost function = RSS + lambda * sum (|beta|)
#cost = Mean((trainingData$Fat - pred_training)^2) + lambda * sum(abs(coef(m1)))

##### Task 3 #####
#################

#Splitting the training data in x (predictors) and y (target)
x =as.matrix(trainingData %>% select(1:100))
y = as.matrix(trainingData$Fat)

#fitting a lasso regression model to the test data
m_lasso=glmnet(x, y,family="gaussian", alpha=1)



#plotting the fitted lasso regression model
plot(m_lasso, xvar = "lambda", label = TRUE)



# It looks from the plot that lambda can have values from 0.4 to 0.9 

##### Task 4 #####
#################

m_ridge=glmnet(x, y,family="gaussian", alpha=0)

plot(m_ridge, xvar = "lambda", label = TRUE)

##### Task 5 #####
#################
#creating a lasso regression model using cross validation
cv_lasso = cv.glmnet(x, y, family = "gaussian", alpha = 1)

#Plotting the Mean Square Error for different values of lambdas 
plot(cv_lasso, xvar = "lamba", lable = TRUE)

#setting opt_lambda to the lambda which produces the lowest cost 
opt_lambda = cv_lasso$lambda.min
cat("Optimal lambda:", opt_lambda, "\n")


#Calculating all how many coefficients that are not zero when using the optimal lambda, minus one due to intercept
num_variables = sum(coef(cv_lasso, s = opt_lambda) != 0) - 1

cat("Number of Variables Chosen:", num_variables, "\n")

#Splitting the test data in x (predictors) and y (target)
x_test = as.matrix(testData %>% select(1:100))
y_test = as.matrix(testData$Fat)

# using the optimal lambda to make predictions on the test data
lasso_test_pred = predict(cv_lasso, newx = x_test, s = opt_lambda)

#plotting the predicted values vs the true values
plot(y_test, lasso_test_pred, xlab = "Original Test Values", ylab = "Predicted Test Values", main = "Predicted test values VS Original test values", pch = 16)

#adding a line y = x line which is when the predicted value = the true value
abline(a = 0, b = 1, col = "red")

#Calculating MSE for the Lasso model
MSE_lasso_test = mean((y_test - lasso_test_pred)^2)
cat("MSE fo the lasso model on the test data:", MSE_lasso_test, "\n")

