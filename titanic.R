setwd("C:/Users/Akash/Desktop/Advanced Analytics/Kaggle")

train <- read.csv("C:/Users/Akash/Desktop/Advanced Analytics/Kaggle/train.csv")
test <- read.csv("C:/Users/Akash/Desktop/Advanced Analytics/Kaggle/test.csv")

test$survived <- rep(0, 418)

train_model = train[c(1,2,3,5,6,7,8,10)]

train_model$cabinStat <- as.numeric(train$Cabin != "")
train_model$Sex <- as.numeric(train$Sex)

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

var1 <- train_model$Sex
var2 <- as.numeric(train_model$Pclass)
var <- cbind(var1, var2)
y <- train_model$Survived

n = length(var1)

theta = matrix(0, nrow = 2, ncol = 1)

cost <- function(theta){
    y_dash = 1 / (1 + exp(-(t(theta) %*% t(var))))
    cost <- 1/n * (-sum(y * log(1 - y_dash) + (1 - y) * log(y_dash)))
    return(cost)
}

optim(par = theta, fn = cost)