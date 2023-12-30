# """""""""""""""""""""""""""""" Task 1 """""""""""""""""""""""""""""" #
library(dplyr)
library(caret)
#Read data from file
data<-read.csv("bank-full.csv",sep= ";",stringsAsFactors=TRUE, header=TRUE)
data = data %>% select(-duration)
#Create training set (40% of data)
n <- dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n * 0.4))
train <- data[id,]
#Create validation set (30% of data)
id1 <- setdiff(1:n, id)
set.seed(12345)
id2 <- sample(id1, floor(n * 0.3))
valid <- data[id2,]
#Create test set (30 of data)
id3 <- setdiff(id1, id2)
test <- data[id3,]
# """""""""""""""""""""""""""""" Task 2 """""""""""""""""""""""""""""" #
library(tree)
d_tree = tree(y~.,data=train)
plot(d_tree)
text(d_tree)
#summary(fit)
d_tree_ms = tree(y~.,data=train, control = tree.control(nrow(train),minsize = 7000))
plot(d_tree_ms)

text(d_tree_ms)
#summary(fit_min_size)
d_tree_md = tree(y~.,data=train, control = tree.control(nrow(train),mindev = 0.0005))
plot(d_tree_md)
text(d_tree_md)
#summary(fit_min_dev)
#Missclassification errors:
MCE_dataset <- function(data_set, tree_type) {
  pred = predict(tree_type, newdata=data_set, type="class")
  MCE_pred = mean(data_set$y != pred)
  cat("Missclassification", deparse(substitute(data_set)), ",", deparse(substitute(tree_type)),
      "=", MCE_pred,"\n")
}
MCE_dataset(train, d_tree)
MCE_dataset(train, d_tree_ms)
MCE_dataset(train, d_tree_md)
MCE_dataset(valid, d_tree)
MCE_dataset(valid, d_tree_ms)
MCE_dataset(valid, d_tree_md)
# """""""""""""""""""""""""""""" Task 3 """""""""""""""""""""""""""""" #
# Initialize vectors to store results
max_leaves <- 50
train_score <- c(0,max_leaves)
valid_score <- c(0,max_leaves)
# Grow trees with different depths and record misclassification rates
for (leaves in 2:max_leaves) {
  tree_pruning <- prune.tree(d_tree_md, best = leaves)
  # Predictions on training and validation sets
  #train_pred <- predict(tree_pruning, newdata = train, type="tree")
  valid_pred <- predict(tree_pruning, newdata = valid, type="tree")
  # Calculate misclassification rates
  train_score[leaves] <- deviance(tree_pruning)
  valid_score[leaves] <- deviance(valid_pred)
  cat(leaves, deviance(valid_pred),"\n")
}
# Plot the misclassification rates against the number of leaves

plot(2:max_leaves, train_score[2:max_leaves], type = "b", col = "blue", xlab = "Number of
Leaves", ylab = "Score", main = "Tree Depth Selection", ylim=c(8000,12000))
points(2:max_leaves, valid_score[2:max_leaves], type = "b", col = "red")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "red"), lty = 1)
# Identify the optimal number of leaves
optimal_train <- which.min(train_score[-1]) + 1
optimal_valid <- which.min(valid_score[-1]) + 1
print(optimal_train)
print(optimal_valid) #Optimal is 22
opt_tree = prune.tree(d_tree_md, best=optimal_valid)
plot(opt_tree)
text(opt_tree, pretty=0)
summary(opt_tree)
# """""""""""""""""""""""""""""" Task 4 """""""""""""""""""""""""""""" #
pred_opt_tree = predict(opt_tree,newdata=test,type="class")
print(pred_opt_tree)
opt_tree_confusion_matrix=table(pred_opt_tree,test$y)
print(opt_tree_confusion_matrix)
acc=sum(diag(opt_tree_confusion_matrix))/sum(opt_tree_confusion_matrix) # 0.8910351
prec=opt_tree_confusion_matrix[4]/sum(opt_tree_confusion_matrix[4],opt_tree_confusion_matrix[2])
sen=opt_tree_confusion_matrix[4]/sum(opt_tree_confusion_matrix[4],opt_tree_confusion_matrix[3])
f1_score = 2*sen*prec/(sen+prec) #0.224554
print(acc)
print(f1_score)
# """""""""""""""""""""""""""""" Task 5 """""""""""""""""""""""""""""" #
predictions_prob <- predict(opt_tree, newdata = test, type = "vector")
predictions = c(nrow(predictions_prob))
for (row in 1:nrow(predictions_prob))
  predictions[row] = ifelse(predictions_prob[row,1]/predictions_prob[row,2] < 5, "yes", "no")
opt_tree_confusion_matrix2 <- table(test$y,predictions)
print("Confusion Matrix with Given Loss Matrix:")

print(opt_tree_confusion_matrix2)
acc2=sum(diag(opt_tree_confusion_matrix2))/sum(opt_tree_confusion_matrix2)
prec2=opt_tree_confusion_matrix2[4]/sum(opt_tree_confusion_matrix2[4],opt_tree_confusion_matrix2[2])
sen2=opt_tree_confusion_matrix2[4]/sum(opt_tree_confusion_matrix2[4],opt_tree_confusion_matrix2[3])
f1_score = 2*sen2*prec2/(sen2+prec2)
print(acc2)
print(f1_score)
# """""""""""""""""""""""""""""" Task 6 """""""""""""""""""""""""""""" #
predictions_prob <- predict(opt_tree, newdata = test, type = "vector")
pi_sequence = seq(0.05, 0.95, 0.05)
TPR_tree = numeric()
FPR_tree = numeric()
TPR_reg = numeric()
FPR_reg = numeric()
logreg_model = glm(y~., data=train,family="binomial")
probability_reg = predict(logreg_model, test, type="response")
for (pi in pi_sequence) {
  prediction_tree = ifelse(predictions_prob[,2] > pi, "yes", "no")
  prediction_reg = ifelse(probability_reg > pi, "yes", "no")
  CM_tree = table(prediction_tree,test$y)
  CM_reg = table(prediction_reg,test$y)
  TP = CM_tree[4]
  FP = CM_tree[2]
  P = CM_tree[3] + CM_tree[4]
  N = CM_tree[1] + CM_tree[2]
  TPR_tree = c(TPR_tree,TP/P)
  FPR_tree = c(FPR_tree,FP/N)
  TP = CM_reg[4]
  FP = CM_reg[2]
  P = CM_reg[3] + CM_reg[4]
  N = CM_reg[1] + CM_reg[2]
  TPR_reg = c(TPR_reg,TP/P)
  FPR_reg = c(FPR_reg,FP/N)
}
print(TPR_tree)
print(FPR_tree)

plot(FPR_tree,TPR_tree,type="b", col="red", ylim=c(0,1), xlim=c(0,1), ylab="TPR",
     xlab="FPR")
points(FPR_reg,TPR_reg,pch=5,type="b", col = "blue")
legend("bottomright", c("Tree", "Logistic regression model"), fill=c("red","blue"))
