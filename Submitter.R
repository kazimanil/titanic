submission <- merge(test, 
                    det[, c(1,2,3,4,5,6,14)][, .(NaiveBayesProb = mean(NaiveBayesProb)), .(Cl_Relative, Cl_Age, Cl_Fare, Embarked, Sex, Pclass)], 
                    by = colnames(det[, c(1,2,3,4,5,6)]))[, c(7,17)]
submission[, Survived := ifelse(NaiveBayesProb > 0.5, 1, 0)]
submission[, NaiveBayesProb := NULL]
fwrite(submission, "submission_20171227.csv")
