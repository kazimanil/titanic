NBCalc <- function(train, test, rows, objective){
  
  stopifnot((is.character(rows) & is.character(objective)) | 
              (is.numeric(rows) & is.numeric(objective))) # stops if rows and objective is not in the same format
  
  rts <- if(is.numeric(rows)){
    as.numeric(cbind(as.data.table(t(rows)), as.data.table(objective)))
  } else {
    as.character(cbind(as.data.table(t(rows)), as.data.table(objective)))
  }                                                        # rows to select
                       
  dat <- train[, rts, with = FALSE]                        # train data inside the function
  det <- test[, rows, with = FALSE]                        # test data inside the function
  det$p1 <- train[, mean(get(objective))]                  # overall probability of success / desired output. i.e churners for churn analysis
  p1 <- train[, mean(get(objective))]                      # overall probability of success / desired output. i.e churners for churn analysis
  nrowdet <- nrow(det)                                     # for data check
  rff <- ncol(dat) - 1                                     # amount of rows to be used in forloop
  for(i in 1:rff){
    name <- colnames(dat)[i]                               # name of the i-th column in forloop
    res  <- dat[, .(value = unique(get(name)),             # factors of the i-th column in forloop
                    lift  = mean(get(objective)) / p1),    # lift of the i-th column in forloop
                  .(get(name))][,2:3]
    res  <- `colnames<-`(res, c("value", paste0("lift_", name)))
    det  <- merge(det,
                  res,
                  by.x = paste0(name), by.y = "value")
    stopifnot(nrowdet == nrow(det))
  }
  scol <- rff + 2
  ecol <- (2 * rff) + 1
  det[, NaiveBayesProb := apply(det[, scol:ecol], 1, prod) * p1]
  return(det)
}
