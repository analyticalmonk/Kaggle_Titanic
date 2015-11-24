setwd("C:/Users/Akash/Desktop/Advanced Analytics/Kaggle")

X_train <- read.csv("C:/Users/Akash/Desktop/Advanced Analytics/Kaggle/train.csv")
X_test <- read.csv("C:/Users/Akash/Desktop/Advanced Analytics/Kaggle/test.csv")

X_test$survived <- rep(0, 418)

X_train$Age[is.na(X_train$Age)] <- mean(X_train$Age[!is.na(X_train$Age)])
X_test$Fare[is.na(X_test$Fare)] <- mean(X_test$Fare[!is.na(X_test$Fare)])

set.seed(21)
library(caTools)
split <- sample.split(X_train$Survived, SplitRatio = 0.7)
train_training <- X_train[split,]
train_cv <- X_train[!split,]
train_training$Name <- NULL
train_cv$Name <- NULL

library(caret)
library(gbm)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 8)
gbmFit <- train(Survived ~ Pclass + Sex + Fare, data = train_training, method = "gbm", trControl = fitControl, verbose = T)
cv_pred <- predict(gbmFit, train_cv)
table(train_cv$Survived, cv_pred > 0.5)

test_pred <- predict(gbmFit, X_test)
submission <- data.frame(PassengerId = X_test$PassengerId, Survived = as.numeric(test_pred > 0.5))
write.csv(submission, "gbm_model_ClassSexFare2.csv", row.names = F)

# train_model = train[c(1,2,3,5,6,7,8,10)]
# 
# train_model$cabinStat <- as.numeric(train$Cabin != "")
# train_model$Sex <- as.numeric(train$Sex)

##Steps for applying logistic regression#############
#1.) Choose the variable to consider.
# We will be implementing a gender(Sex) and class(Pclass) based model.
#2.) initialize theta.
#
#3.) Implement the cost function,
# Here, cost(theta) = -sigma(y*log(1-y') + (1-y)*log(y'))
#
#4.) Implement gradient descent to obtain the required theta.
#
#5.) Predict y', and obtain a numerical value signifying the accuracy.
#####################################################

# var1 <- train_model$Sex
# var2 <- as.numeric(train_model$Pclass)
# var3 <- train_model$Fare
# # var3 <- as.numeric(train_model$Age)
# var <- cbind(var1, var2, var3)
# y <- train_model$Survived
# 
# n = length(var1)
# 
# theta = matrix(c(0, 0, 0), nrow = 3, ncol = 1)
# 
# cost <- function(theta){
#     y_dash = 1 / (1 + exp(-(t(theta) %*% t(var))))
#     cost <- 1/n * (-sum(y * log(1 - y_dash) + (1 - y) * log(y_dash)))
#     return(cost)
# }
# init_cost = cost(theta)
# 
# l <- optim(par = theta, fn = cost)
# 
# theta <- l$par
# fin_cost <- cost(theta)
# y_dash = 1 / (1 + exp(-(t(theta) %*% t(var))))
# survived_dash <- y_dash > 0.7

