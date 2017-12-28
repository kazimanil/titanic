# K-NN K Selection ----
acc   <- data.table(set = numeric(),
                    k_nn = numeric(),
                    Accuracy = integer())
s <- 1
while(s < 101){
  knn_train <- train[, .(Survived,
                         Title = as.numeric(Title), 
                         Pclass = as.numeric(Pclass), 
                         Sex = as.numeric(Sex), 
                         Embarked = as.numeric(Embarked), 
                         glmAge, 
                         Relative = as.numeric(Cl_Relative), 
                         log_Fare, 
                         nTicket)]
   
  #Randomly shuffle the data
  knn_train <- knn_train[sample(nrow(train)),]
  knn_r <- knn_train[1:700]   # R: tRain set for K selection
  knn_e <- knn_train[701:891] # E: tEst set for K selection
  knn_s <- knn_r$Survived     # S: Survived column of tRain set
  knn_r[, Survived := NULL]   # 
  knn_a <- knn_e$Survived     # A: Actual outcome of tEst set
  knn_e[, Survived := NULL]   # 
  
  for(k in 1:100){
    knn_o <- cbind(knn_e, 
                   pred = knn(knn_r, knn_e, knn_s, k),
                   res  = knn_a) # O :kNN Outcomes
    acc   <- rbind(acc,
                   as.data.table(t(c(set = s,
                                     k_nn = k, 
                                     Accuracy = knn_o[pred == res, .N / nrow(knn_o)]))))
  }
print(paste0(s, " numaralı rastgele deneme kümesi için işlem tamamlandı."))
s <- s + 1
}

acc[, .(Ort = mean(Accuracy),
        SD = sd(Accuracy))
    , .(k_nn)][order(-Ort)]

# Output ----
knn_test  <- test[, .(PassengerId,
                      Title = as.numeric(Title), 
                      Pclass = as.numeric(Pclass), 
                      Sex = as.numeric(Sex), 
                      Embarked = as.numeric(Embarked), 
                      glmAge, 
                      Relative = as.numeric(Cl_Relative), 
                      log_Fare, 
                      nTicket)]

knn_test <- cbind(knn_test,
                  Survived = knn(knn_train[, 2:9], knn_test[, 2:9], cl = train$Survived, k = 7))

fwrite(knn_test[, c(1,10)], "submission_knn_2.csv")
