

#Read data from csv file
tecator = read.csv("tecator.csv", header = TRUE)


#Spliting data 50%/50% between training and testing
n <- dim(tecator)[1]
set.seed(12345)
id <- sample(1:n, floor(n * 0.5))
trainingData <- tecator[id, ]
testData <- tecator[-id, ]