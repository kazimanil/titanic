# Data Input -----
rm(list = ls()); gc()
train <- fread("train.csv")
gender_submission <- fread("gender_submission.csv")
test <- fread("test.csv")
source("NaiveBayes_Calculator.R")

# (Train) Data Manipulation ----
# Title Creation
train[, Title := trimws(str_split_fixed(gsub(",", ".", Name), "[.]", n = 3)[, 2])]
train[Title == "Master", Title := "Mr"]
train[Title == "Mlle", Title := "Miss"]
train[Title %in% c("Ms", "Mme"), Title := "Mrs"]
train[Title %in% c("the Countess", "Sir", "Jonkheer", "Lady", "Don", "Dona"), Title := "Noble"]
train[Title %in% c("Capt", "Col", "Major"), Title := "Military"]
train[, Title := as.factor(Title)]

# Na Reduction & Factor Generation
train <- merge(train, 
               train[, mean(Age, na.rm = T), .(Title)],
               by = "Title")
train[, Age := ceiling(Age)]
train[, glmAge := ifelse(is.na(Age), V1, Age)] # 29 is the median and the mean.
train[, V1 := NULL]
train[, Cl_Age := as.factor(pmin(ceiling(glmAge / 10), 8))]
train[, Cl_Relative := as.factor(pmin(SibSp + Parch, 5))]
train[, Pclass := as.factor(Pclass)]
train[, Embarked := as.factor(Embarked)]
train[, Sex := as.factor(Sex)]
train[, log_Fare := log10(Fare + 1)]
train[log_Fare < 1, Cl_Fare := "Cheapest"]
train[log_Fare >= 1 & log_Fare < 1.5, Cl_Fare := "Cheap"]
train[log_Fare >= 1.5 & log_Fare < 2, Cl_Fare := "Modest"]
train[log_Fare > 2, Cl_Fare := "Expensive"]

train[, CabinDpt := substr(Cabin, 1,1)]
train[CabinDpt == " ", CabinDpt := as.character(NA)]

train <- merge(train,
               train[, .(nTicket = .N), Ticket],
               all.x = T, by = "Ticket")
train[, fac_nTicket := as.factor(nTicket)]

# (Test) Data Manipulation ----
# Title Creation
test[, Title := trimws(str_split_fixed(gsub(",", ".", Name), "[.]", n = 3)[, 2])]
test[Title == "Master", Title := "Mr"]
test[Title == "Mlle", Title := "Miss"]
test[Title %in% c("Ms", "Mme"), Title := "Mrs"]
test[Title %in% c("the Countess", "Sir", "Jonkheer", "Lady", "Don", "Dona"), Title := "Noble"]
test[Title %in% c("Capt", "Col", "Major"), Title := "Military"]
test[, Title := as.factor(Title)]

# Na Reduction & Factor Generation
test[is.na(Fare), Fare := 0]

test <- merge(test, 
               test[, mean(Age, na.rm = T), .(Title)],
               by = "Title")
test[, Age := ceiling(Age)]
test[, glmAge := ifelse(is.na(Age), V1, Age)] # 29 is the median and the mean.
test[, V1 := NULL]
test[, Cl_Age := as.factor(pmin(ceiling(glmAge / 10), 8))]
test[, Cl_Relative := as.factor(pmin(SibSp + Parch, 5))]
test[, Pclass := as.factor(Pclass)]
test[, Embarked := as.factor(Embarked)]
test[, Sex := as.factor(Sex)]

test[, log_Fare := log10(Fare + 1)]
test[log_Fare < 1, Cl_Fare := "Cheapest"]
test[log_Fare >= 1 & log_Fare < 1.5, Cl_Fare := "Cheap"]
test[log_Fare >= 1.5 & log_Fare < 2, Cl_Fare := "Modest"]
test[log_Fare > 2, Cl_Fare := "Expensive"]

test[, CabinDpt := substr(Cabin, 1,1)]
test[CabinDpt == " ", CabinDpt := as.character(NA)]

test <- merge(test,
              test[, .(nTicket = .N), Ticket],
              all.x = T, by = "Ticket")
test[, fac_nTicket := as.factor(nTicket)]
