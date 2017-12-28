#Randomly shuffle the data
train <- train[sample(nrow(train)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(train)), breaks = 10, labels = FALSE)

acc <- data.table(Model = character(),
                  Accuracy = integer())

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds == i, arr.ind=TRUE)
  testData <- train[testIndexes, ]
  trainData <- train[-testIndexes, ]
  assign(x = paste0("model_", i),
         step(direction = "backward",
              object = glm(formula = Survived ~ glmAge + log_Fare + SibSp + Parch + Embarked + Pclass + Title + Cl_Relative + nTicket,
                           data = trainData, 
                           family = binomial(link = "logit"))))
  testData[, phat := predict.glm(get(paste0("model_", i)), newdata = testData, type = "response")]
  testData[, pred := ifelse(phat > 0.5, 1, 0)]
  pasted <- cbind(Model = paste0("model_", i),
                  Accuracy = testData[pred == Survived, .N / nrow(testData)])
  acc <- rbind(acc, pasted, deparse.level = 0)
  #Use the test and train data partitions however you desire...
}
acc[, Accuracy := as.numeric(Accuracy)]
selectedcols <- colnames(as.data.table(get(acc[Accuracy == max(Accuracy)]$Model)$model))[2:ncol(get(acc[Accuracy == max(Accuracy)]$Model)$model)]
model <- glm(formula = Survived ~ glmAge + log_Fare + Pclass + Title + Cl_Relative,
             data = train, family = binomial(link = "logit"))
test[, phat := predict.glm(model, newdata = test, type = "response")]
test[, Survived := ifelse(phat > 0.5, 1, 0)]
gender_submission <- merge(gender_submission[, 1],
                           test[, c("PassengerId", "Survived")],
                           by = "PassengerId")
fwrite(gender_submission, "submission_20171228_4.csv")
