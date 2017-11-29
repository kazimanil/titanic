# Data Input -----
train <- fread("train.csv")
gender_submission <- fread("gender_submission.csv")
test <- fread("test.csv")

# Data Manipulation ----
train[Sex == "female", Num_Sex := 1]
train[Sex != "female", Num_Sex := 0]
# Checking if there is a correlation between 'having shared tickets' and survival ----
mtckt <- train[,
							 .(nTicket = .N, minS = min(Survived), maxS = max(Survived), avgS = mean(Survived)),
							 Ticket] # detecting if there were multiple passengers with a ticket
train <- merge(train,
							 mtckt,
							 all.x = T, by = "Ticket")
ggplot(data = train[nTicket > 1], aes(x = nTicket, y = maxS)) + geom_jitter() + facet_wrap(facets = "minS")
train[nTicket > 1, 
			.N,
			.(minS, minS = maxS)] 
train[nTicket > 1, 
			.N,
			.(nTicket, minS, minS = maxS)][order(nTicket, minS)] 
# it seems there is. Lets check the correlation.
corr_S_nT <- cor.test(train[nTicket > 1]$nTicket, train[nTicket > 1]$avgS) 
# Very significant(p~0), negative and somehow strong (%35,2) relationship.
# Lets build a correlation matrix with available data ----
corr_train <- train[, c(3, 4, 7, 8, 9, 10, 17)]
corr_matrix <- cor(corr_train, use = "complete.obs")
round(corr_matrix, 2)
corrplot(corr_matrix, type = "upper", tl.col = "black", tl.srt = 45, order = "hclust")
