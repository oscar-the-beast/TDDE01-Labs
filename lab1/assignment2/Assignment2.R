

##### Task 1 #####
#################

#Read data from file
dataframe = read.csv("parkinsons.csv")

#Create test set of 60% of the data and a test set with the remaining 40%
n <- dim(dataframe)[1]
set.seed(12345)
id <- sample(1:n, floor(n * 0.6))
trainingData <- dataframe[id, ]
testData <- dataframe[-id, ]

#extract the target variable column and the 16 column containing the prediction parameters
library(dplyr) #library to use select function
trainingData = trainingData  %>% select(5:5, 7:22)
testData = testData %>% select(5:5, 7:22)

#Scale the data
library(caret) #library for scaling
scaler = preProcess(trainingData) #preProcess() to learn transformation
scaledTrainingData = predict(scaler, trainingData) #predict() to apply transformation
scaledTestData = predict(scaler, testData)

##### Task 2 #####
#################

#Create a linear model with motor_UPDRS as target variable and the other 16 parameters for predictions
trainingModel = lm(motor_UPDRS~., data=scaledTrainingData)
summary(trainingModel) #displays the model coefficients


#Predict based on the scaled test and training data using the trainingModel
predict_on_scaled_training_data = predict(trainingModel, scaledTrainingData)
predict_on_scaled_test_data = predict(trainingModel, scaledTestData)

#Calculate mean square error of actual motor_UPDRS vs predicted by model
MSE_on_predict_scaled_training_data = mean((scaledTrainingData$motor_UPDRS - predict_on_scaled_training_data)^2)
MSE_on_predict_scaled_test_data = mean((scaledTestData$motor_UPDRS - predict_on_scaled_test_data)^2)

##### Task 3 #####
#################

X = as.matrix(scaledTrainingData %>% select(Jitter...:PPE)) #all parameters except target parameter
Y = as.matrix(scaledTrainingData$motor_UPDRS) #just the target parameter
n = nrow(scaledTrainingData) #number of rows


#-n/2*log(2*pi) is a constant
#-n/2*log(sigma^2) # penalizes models with larger variance
#- (1 / (2 * sigma^2)) * sum((y - x %*% theta)^2) sum of squared residuals, penalizing models that do not fit the data
# sigma^2 is the variance of errors
Loglikelihood = function(theta, sigma) {
  return(-n / 2 * log(2 * pi*sigma^2) - (1 / (2 * sigma^2)) * sum((Y - X %*% theta)^2))
}

#Add a ridge penalty to the negative loglikelihood function
#Ridge penalty penalizes the model by adding the sum of squared values of the coefficients multiplied by lambda
#This helps to shrink the parameter estimates towards zero, providing L2 regularization
Ridge = function(theta, lambda) {
  sigma <- tail(theta, n = 1)
  theta <- theta[-17]
  return(lambda * sum(theta^2) - Loglikelihood(theta, sigma))
}

#computes optimal theta and sigma for the given lambda
RidgeOpt = function(lambda) {
  return(optim(rep(1, 17), fn = Ridge, lambda = lambda, method = "BFGS"))
}


I = as.matrix(diag(16))
#Given the ridge penalty (based on lambda) how many parameters affect the model
DF = function(lambda) {
  return(sum(diag((X %*% solve((t(X) %*% X + lambda * I))) %*% t(X))))
}

##### Task 4 #####
#################

#perform ridgeOpt for different lambdas
opt_theta1 = RidgeOpt(lambda=1)
opt_theta100 = RidgeOpt(lambda=100)
opt_theta1000 = RidgeOpt(lambda=1000)

x_trainingData = X
x_testData = as.matrix(scaledTestData %>% select(Jitter...:PPE))

opt1 = as.matrix(opt_theta1$par[-17])
opt100 = as.matrix(opt_theta100$par[-17])
opt1000 = as.matrix(opt_theta1000$par[-17])

#Perform predictions of motor_UPDRS values
predict_training_opt1 = x_trainingData %*% opt1
predict_training_opt100 = x_trainingData %*% opt100
predict_training_opt1000 = x_trainingData %*% opt1000

predict_test_opt1 = x_testData %*% opt1
predict_test_opt100 = x_testData %*% opt100
predict_test_opt1000 = x_testData %*% opt1000

#Calculate mean square error, actual parkinsons score - predicted
mse_training_opt1 = mean((scaledTrainingData$motor_UPDRS - predict_training_opt1)^2)
mse_training_opt100 = mean((scaledTrainingData$motor_UPDRS - predict_training_opt100)^2)
mse_training_opt1000 = mean((scaledTrainingData$motor_UPDRS - predict_training_opt1000)^2)

mse_test_opt1 = mean((scaledTestData$motor_UPDRS - predict_test_opt1)^2)
mse_test_opt100 = mean((scaledTestData$motor_UPDRS - predict_test_opt100)^2)
mse_test_opt1000 = mean((scaledTestData$motor_UPDRS - predict_test_opt1000)^2)

#Calculate degrees of freedom
df1 = DF(lambda=1) #13.85375
df2 = DF(lambda = 100) #9.72796
df3 = DF(lambda=1000) #5.437577

#Displaying results
resultTable = data.frame(
  Data = c("Training", "Training", "Training", "Test", "Test", "Test"),
  Lambda = c("lambda=1", "lambda=100", "lambda=1000"),
  MSE = c(mse_training_opt1, mse_training_opt100, mse_training_opt1000, mse_test_opt1, mse_test_opt100, mse_test_opt1000),
  Degrees_of_freedom = c(df1, df2, df3, df1, df2, df3)
)
resultTable
