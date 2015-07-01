train <- read.csv("./train.csv", stringsAsFactors = F)
table(train$Survived)
548/nrow(train)
summary(train)

library("mice")
inc_train <- train[c("Age", "Pclass", "SibSp", "Parch", "Fare")]
imputed <- complete(mice(inc_train))
train$Age <- imputed$Age
train$Pclass <- as.factor(train$Pclass)

test <- read.csv("test.csv", stringsAsFactors = F)

model1 <- glm(data = train, family = "binomial", formula = Survived ~ Age + SibSp + Fare)

pred_test1 <- predict(model1, newdata = test, type = "response")
test$Survived <- pred_test1 >= 0.5
test$Survived <- as.numeric(test$Survived)

submission <- test[c("PassengerId", "Survived")]
# Score -- 0.64115
write.csv(submission, file = "1_logReg_r_submission.csv", row.names = F)

model2 <- glm(data = train, family = "binomial", formula = Survived ~ Pclass + Sex + Fare)

pred_test2 <- predict(model2, newdata = test, type = "response")
test$Survived2 <- pred_test2 >= 0.55
test$Survived2 <- as.numeric(test$Survived2)

submission2 <- test[c("PassengerId", "Survived2")]
names(submission2) <- c("PassengerId", "Survived")
## Score -- 0.75120
write.csv(submission2, file = "2_logReg_r_submission.csv", row.names = F)

model3 <- glm(data = train, family = "binomial", formula = Survived ~ Pclass + Sex + Age)

pred_test3 <- predict(model3, newdata = test, type = "response")
test$Survived3 <- pred_test3 >= 0.5
test$Survived3 <- as.numeric(test$Survived3)

submission3 <- test[c("PassengerId", "Survived3")]
names(submission3) <- c("PassengerId", "Survived")
## Score -- 0.76555
write.csv(submission3, file = "3_logReg_r_submission.csv", row.names = F)
