train[, .(mean(Survived), .N), .(Sex)]
summary(aov(formula = Survived ~ Sex, data = train))
train[, .(mean(Survived), .N), .(Embarked)]
summary(aov(formula = Survived ~ Embarked, data = train))
train[, .(mean(Survived), .N), .(Pclass)]
summary(aov(formula = Survived ~ Pclass, data = train))
train[, .(mean(Survived), .N), .(Cl_Fare)] # Fare Classes
summary(aov(formula = Survived ~ Cl_Fare, data = train))
train[, .(mean(Survived), .N), .(Cl_Age)] # Age Classes (some doubts but i guess it will be fine)
summary(aov(formula = Survived ~ Cl_Age, data = train))
train[, .(mean(Survived), .N), .(Cl_Relative)] # Amount of Relatives Classes
summary(aov(formula = Survived ~ Cl_Relative, data = train))

rows <- c("Pclass", "Sex", "Embarked", "Cl_Fare", "Cl_Age", "Cl_Relative")
objective <- "Survived"
