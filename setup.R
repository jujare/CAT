n_tube_end_form <- extract_numbers(tube_end_form, c(1))
n_tube_end_a_form <- n_tube_end_form
n_tube_end_x_form <- n_tube_end_form
colnames(n_tube_end_a_form) <- c("end_a", "end_a_forming")
colnames(n_tube_end_x_form) <- c("end_x", "end_x_forming")
rm(n_tube_end_form)

# Normalize cost of training set
maxCost = max(n_train_set$cost)
minCost = min(n_train_set$cost)

# merge tables
mergetrain <- merge(n_train_set, n_bill_of_materials)
mergetrain <- merge(mergetrain, n_tube)

mergetest <- merge(n_test_set, n_bill_of_materials)
mergetest <- merge(mergetest, n_tube)

# remove any columns with less than 70 % non-NA data
for (i in rev(seq(dim(mergetrain)[2])))
{
  if ((1-sum(is.na(mergetrain[, i]))/dim(mergetrain)[1]) < 1)
  {
    columnname <- colnames(mergetrain)[i]
    mergetrain <- mergetrain[, -i]
    mergetest <- mergetest[, -grep(paste0("^", columnname, "$"), colnames(mergetest))]
  }
}

for (i in seq(dim(mergetrain)[2]))
{
  if (any(is.na(mergetrain[, i])))
  {
    printf("did not expect this")
    if (class(mergetrain[[1, i]]) == "numeric")
    {
      mergetrain[is.na(mergetrain[,i]), i] <- as.numeric(0)
    }
    else
    {
      mergetrain[, i] <- as.character(mergetrain[, i])
      mergetrain[is.na(mergetrain[,i]), i] <- as.character("NotAvail")
      mergetrain[, i] <- as.factor(mergetrain[, i])
    }
  }
}

for (i in seq(dim(mergetest)[2]))
{
  if (any(is.na(mergetest[, i])))
  {
    printf("did not expect this")
    if (class(mergetest[[1, i]]) == "numeric")
    {
      mergetest[is.na(mergetest[,i]), i] <- as.numeric(0)
    }
    else if (class(mergetest[[1, i]]) == "factor")
    {
      mergetest[, i] <- as.character(mergetest[, i])
      mergetest[is.na(mergetest[,i]), i] <- as.character("NotAvail")
      mergetest[, i] <- as.factor(mergetest[, i])
    }
  }
}

# create cv set
library(caret)
#library(doParallel)
#cl <- makeCluster(3)
#registerDoParallel(cl)

set.seed(23)
inTrain <- createDataPartition(y = mergetrain$cost, p = 0.9, list = FALSE)
training <- mergetrain[inTrain,]
testing <- mergetrain[-inTrain,]

ctrl <- trainControl(method="cv", number=3)
#fit <- train(cost ~ ., data = mergetrain, method = "rlm", trControl = ctrl)
#print("training rf")
set.seed(23)
fit1 <- train(cost ~ ., data = mergetrain, method = "rf", trControl = ctrl)
#set.seed(23)
#print("training extraTrees")
#fit2 <- train(cost ~ ., data = mergetrain, method = "extraTrees", trControl = ctrl)
#set.seed(23)
#print("training Boruta")
#fit3 <- train(cost ~ ., data = mergetrain, method = "Boruta", trControl = ctrl)

#stopCluster(cl)

predictions <- predict(fit1, testing[, -grep("^cost$", colnames(testing))])

testpredictions <- predict(fit1, mergetest[, -grep("^id$", colnames(mergetest))])

print("cv original cost")
print(summary(testing[, grep("^cost$", colnames(testing))]))
print("cv predictions")
print(summary(predictions))
library(Metrics)
print("rmsle for CV")
print(rmsle(testing[, grep("cost", colnames(testing))], predictions)) # best: 0.138052458