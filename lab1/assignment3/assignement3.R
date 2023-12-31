library(dplyr)

diabetes = read.csv("pima-indians-diabetes.csv", header = FALSE)
colnames(diabetes) <- c("Number of times pregnant", "Plasma glucose concentration", "Diastolic blood pressure","Triceps skinfold thickness", "2-Hour serum insulin", "Body mass index", "Diabetes pedigree function", "Age", "Diabetes")

#3.1 make a scatterplott

plot(diabetes$`Age`, diabetes$`Plasma glucose concentration`, main = "Plasma glucose concentration vs asge (3.1)",
     xlab = "age", ylab = "Plasma glucose concentration",
     pch = 16, col = ifelse(diabetes$`Diabetes`== 1, 'red', 'blue'))

# Ans: looking at the scatterplot for diabetes depending on plasma glucose, it seems like it will be hard to create a logic regression model for these two parameters


#3.2 train logistic regression model

#creating training data 
train = diabetes%>%select(`Plasma glucose concentration`, `Age`,`Diabetes`)


m1 = glm(Diabetes ~., train, family = 'binomial')

#creating summary of m1
summary(m1)

#calculating prnbability of diabetes for each case with the probalistic model m1
Prob = predict(m1, type="response")

#Making predicitions from by using r=0.5 
Pred_r50=ifelse(Prob>0.5, "1", "0")


#calculating the missclassification error for m1
misclassification_error <- mean(train$Diabetes != Pred_r50)

#printing the missclassification error in the console
print(paste("Misclassification Error (r=0.5):", misclassification_error))


#adding a column named predictions with all the predicitons made by model m1
#train$Prediction_r50 <- Pred_r50



#Making a plot with all the predicted values
plot(train$`Age`, train$`Plasma glucose concentration`, main = "Predicted values for Diabetes (r = 0.5)",
     xlab = "age", ylab = "Plasma glucose concentration",
     pch = 16, col = ifelse(Pred_r50== 1, 'red', 'blue'))


#ans: the logistic regression model m1 uses two parameters to estimate if a person have diabetes or not, it has a 26,3% missclassification error which means that roughly 1 out of 4 persons will be missclassified
# It could therefore be seen as a bad model which is probably underfitted to the training data

#3.3

#extracting coefficients from m1
coefficients <- coef(m1)

#coefficients are

# coefficients[1] = -5.9134 (intercepts)
# coefficients[2] = 0.0356 (Plasma glucose concentration)
# coefficients[3] = 0.0248 (age)
# decission boundry equation: 0 = -5.9124 + 0.0356 * x1 + 0.0248 * x2 //x1 = plasmaglucose, x2 = age// -->
# x1 = -5.9124/(-0.0356) + (0.0248/(-0.0356)) * x2 

# calculate the slope and intercept of the decision boundary line
slope = coefficients[3]/(-coefficients[2])
intercept = (coefficients[1])/(-coefficients[2])

abline(intercept, slope,
       lwd = 2, # line thickness
       lty = 1) # type dashed



#3.4

#Making predicitions from by using r=0.2
Pred_r20=ifelse(Prob>0.2, "1", "0")


#calculating the missclassification error for m1
misclassification_error <- mean(train$Diabetes != Pred_r20)

#printing the missclassification error in the console
print(paste("Misclassification Error (r=0.2):", misclassification_error))


#adding a column named predictions with all the predicitons made by model m1
#train$Prediction_r20 <- Pred_r20



#Making a plot with all trhe predicted values
plot(train$`Age`, train$`Plasma glucose concentration`, main = "Predicted values for Diabetes (r= 0.2)",
     xlab = "age", ylab = "Plasma glucose concentration",
     pch = 16, col = ifelse(Pred_r20 == 1, 'red', 'blue'))



#Making predicitions from by using r=0.8
Pred_r80=ifelse(Prob>0.8, "1", "0")


#calculating the missclassification error for m1
misclassification_error <- mean(train$Diabetes != Pred_r80)

#printing the missclassification error in the console
print(paste("Misclassification Error (r = 0.8):", misclassification_error))


#adding a column named predictions with all the predicitons made by model m1
#train$Prediction_r80 <- Pred_r80



#Making a plot with all trhe predicted values
plot(train$`Age`, train$`Plasma glucose concentration`, main = "Predicted values for Diabetes (r = 0.8)",
     xlab = "age", ylab = "Plasma glucose concentration",
     pch = 16, col = ifelse(Pred_r80 == 1, 'red', 'blue'))



#3.5

#Y = w0 + w1*x1+ w2*x2 + w3* z1 

#z1 = x1^4
train$z1 = train$`Plasma glucose concentration`^4

#z2 = x1^3*x2
train$z2 = train$`Plasma glucose concentration`^3 * train$Age

#Z3 = x1^2*x2^2
train$z3 = train$`Plasma glucose concentration`^2 * train$Age^2

#Z4 = x1^*x2^3
train$z4 = train$`Plasma glucose concentration` * train$Age^3

#z5 = x2^4
train$z5 = train$Age^4


m2 = glm(Diabetes ~., train, family = 'binomial')

#creating summary of m1
summary(m2)

Prob_m2 = predict(m2, type="response")

Pred_m2_r50=ifelse(Prob_m2>0.5, "1", "0")


#calculating the missclassification error for m1
misclassification_error <- mean(train$Diabetes != Pred_m2_r50)

#printing the missclassification error in the console
print(paste("Misclassification Error (M2, r=0.5):", misclassification_error))

plot(train$`Age`, train$`Plasma glucose concentration`, main = "Predicted values for Diabetes m2 model (r = 0.5)",
     xlab = "age", ylab = "Plasma glucose concentration",
     pch = 16, col = ifelse(Pred_m2_r50== 1, 'red', 'blue'))

