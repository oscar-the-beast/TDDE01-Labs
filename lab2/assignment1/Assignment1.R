
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





