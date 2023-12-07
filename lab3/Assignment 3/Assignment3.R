library(neuralnet)


##### Task 1 #####
#################

#generating 500 random numbers between 0 and 10
set.seed(1234567890)
Var <- runif(n = 500, min = 0, max = 10)

#creating a datafram with all random numbers and the sinus function of these numbers
myData <- data.frame(Var, Sin=sin(Var))

#spliting data into training and test data
trainingData <- myData[1:25,] # Training
testData <- myData[26:500,] # Test



# Random initialization of the weights in the interval [-1, 1]
winit <- runif(n = 22,  min = -1, max = 1 )
nn <- neuralnet(Sin ~ Var, data = trainingData, hidden = c(10), startweights = winit)


# Plot of the training data (black), test data (blue), and predictions (red)
plot(trainingData, cex=2, pch = 16, xlab = "Number", ylab = "Sin(x)")
points(testData, col = "blue", cex=1, pch = 16)
points(testData[,1],predict(nn,testData), col="red", cex=1, pch = 16)
legend("bottomleft", legend = c("Training", "True values", "Predicted values"), col = c("black", "blue", "red"), pch = 19, bty = "n")


##### Task 2 #####
#################

linear <- function(x) x

ReLu <- function(x) ifelse(x > 0, x, 0)

softplus <- function(x) log(1 + exp(x))
 

nn_linear<- neuralnet(Sin ~ Var, data = trainingData, hidden = c(10), startweights = winit, act.fct = linear, linear.output = TRUE)

nn_ReLu<- neuralnet(Sin ~ Var, data = trainingData, hidden = c(10), startweights = winit, act.fct = ReLu, linear.output = TRUE)

nn_softplus<- neuralnet(Sin ~ Var, data = trainingData, hidden = c(10), startweights = winit, act.fct = softplus, linear.output = FALSE)


plot(trainingData, cex=2, pch = 16, xlab = "number", ylab = "Sin",ylim = c(-1,1.5),)
points(testData, col = "blue", cex=1, pch = 16)
points(testData[,1],predict(nn_linear,testData), col="green", cex=1, pch = 16)
points(testData[,1],predict(nn_ReLu,testData), col="purple", cex=1, pch = 16)
points(testData[,1],predict(nn_softplus,testData), col="orange", cex=1, pch = 16)
legend("bottomleft", legend = c("Training","True values", "Linear", "ReLu", "softplus"), col = c("black","blue", "green", "purple", "orange"), pch = 19, bty = "n")


##### Task 3 #####
#################

bigVar <- runif(500, 0, 50)

#creating a datafram with all random numbers and the sinus function of these numbers
bigData <- data.frame(Var = bigVar, Sin=sin(bigVar))

plot(bigData[,1], predict(nn, bigData), col = "red", cex=1, pch = 16, xlab = "number", ylab = "Sin")
points(bigData, col = "blue", cex = 1, pch = 16)
legend("bottomleft", legend = c("True values", "Predicted values"), col = c("blue", "red"), pch = 19, bty = "n")

##### Task 4 #####
#################

nn$weights

##### Task 5 #####
#################

smallVar <- runif(500, 0, 10)
smallData <- data.frame(Var = smallVar, Sin = sin(smallVar))

nn_inverse <- neuralnet(Var ~ Sin, data = smallData, hidden = c(10), startweights = winit, threshold = 0.1)

plot(smallData[,2], smallData[,1], col = "black", cex = 1, pch = 16, ylim = c(0,10), xlab = "Sin", ylab = "number")
points(trainingData[,2],trainingData[,1], col = "blue", cex = 1, pch = 16)
points(trainingData[,2], predict(nn_inverse, trainingData), col = "red", cex = 1, pch = 16)
legend("bottomleft", legend = c("Training values", "True values", "Predicted values"), col = c("black", "blue", "red"), pch = 19)








