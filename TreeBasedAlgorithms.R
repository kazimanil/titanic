# Decision Tree ----
library("rpart"); library("randomForest"); library("partykit")

# Plain Decision Trees are not used for prediction rather for exploration.
dt <- rpart(data = train, formula = Survived ~ Title + Pclass + Sex + Embarked + glmAge + Cl_Relative + log_Fare + nTicket + CabinDpt)
printcp(dt)
plotcp(dt)
plot(dt)
text(dt, use.n = TRUE, all = TRUE, cex = 0.8)

# Random Forest is an exploration technique.
# rf_train <- train[, .(Title, Pclass, Sex, Embarked, glmAge, Cl_Relative, log_Fare, nTicket)]
# rf_test  <- test[, .(Title, Pclass, Sex, Embarked, glmAge, Cl_Relative, log_Fare, nTicket)]
# rf_survived <- as.factor(train$Survived)
# rf <- randomForest(rf_train, rf_survived)
# 
# plot(rf)
# predict(object = rf, newdata = rf_test, type = "response", norm.votes = TRUE, predict.all = FALSE, proximity = FALSE, nodes = FALSE)

model_ct <- ctree(formula = Survived ~ Title + Pclass + Sex + Embarked + glmAge + Cl_Relative + log_Fare + nTicket, data = train)
test[, prob := predict(object = model_ct, newdata = test)]
test[, Survived := ifelse(prob > 0.5, 1, 0)]
fwrite(test[, .(PassengerId, Survived)], "submission_Ctree_1.csv")

model_ct2 <- ctree(formula = Survived ~ Title + Pclass + Sex + Embarked + glmAge + Cl_Relative + log_Fare + nTicket, data = train, control = ctree_control(minsplit = 31))
test[, prob2 := predict(object = model_ct2, newdata = test)]
test[, Survived2 := ifelse(prob2 > 0.5, 1, 0)]
fwrite(test[, .(PassengerId, Survived = Survived2)], "submission_Ctree_2.csv")

model_ct3 <- ctree(formula = Survived ~ Title + Pclass + Embarked + glmAge + Cl_Relative + log_Fare + nTicket, data = train)
test[, prob3 := predict(object = model_ct2, newdata = test)]
test[, Survived3 := ifelse(prob2 > 0.5, 1, 0)]
fwrite(test[, .(PassengerId, Survived = Survived3)], "submission_Ctree_3.csv")
