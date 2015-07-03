extract_numbers <- function(df, colSeq)
{
  for (i in colSeq)
  {
    df[, i] <- abs(as.numeric(extract_numeric(df[, i])))
  }  
  
  for (i in seq(dim(df)[2]))
  {
    if (class(df[, i]) == "character")
      df[, i] <- as.factor(df[, i])
  }
  df
}

setwd("~/Learning/MachineLearning/Kaggle/CAT/data/competition_data")

library(stringr)
library(tidyr)

filenames <- list.files(pattern="*.csv", full.names=TRUE)

for (i in seq_along(filenames))
{
  ncols <- dim(read.csv(filenames[i]))[2]
  assign(str_sub(filenames[i], start = 3, end = -5),  read.csv(filenames[i], colClasses = rep("character", ncols)))
}

# removing quote_date
train_set <- train_set[, -3]
test_set <- test_set[, -4]

# extract numbers from the data frames
n_bill_of_materials <- extract_numbers(bill_of_materials, seq(dim(bill_of_materials)[2]))
n_comp_adaptor <- extract_numbers(comp_adaptor, c(seq(1, dim(comp_adaptor)[2]-3, 1), dim(comp_adaptor)[2]))
n_comp_boss <- extract_numbers(comp_boss, c(1, 2, 4, 7, 8, 9, 11, 12, 15))
n_comp_elbow <- extract_numbers(comp_elbow, c(1:9,  12, 16))
n_train_set <- extract_numbers(train_set, c(1:4, 6, 7))
n_tube <- extract_numbers(tube, c(1:7, 12:16))
n_specs <- extract_numbers(specs, c(1:11))
n_test_set <- extract_numbers(test_set, c(1:5, 7))
n_tube_end_form <- extract_numbers(tube_set_form, c(1))

# Normalize cost of training set
maxCost = max(n_train_set$cost)
minCost = min(n_train_set$cost)

# merge tables
merget <- merge(n_train_set, n_bill_of_materials)
merget <- merge(merget, n_tube)
merget <- merge(merget, n_specs)

mergetest <- merge(n_test_set, n_bill_of_materials)
mergetest <- merge(mergetest, n_tube)
mergetest <- merge(mergetest, n_specs)

# remove any columns with less than 70 % non-NA data
for (i in rev(seq(dim(merget)[2])))
{
  if ((1-sum(is.na(merget[, i]))/dim(merget)[1]) < 1)
    merget <- merget[, -i]
}

for (i in rev(seq(dim(mergetest)[2])))
{
  if ((1-sum(is.na(mergetest[, i]))/dim(mergetest)[1]) < 1)
    mergetest <- mergetest[, -i]
}


# create cv set
library(caret)
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

set.seed(23)
inTrain <- createDataPartition(y = merget$cost, p = 0.9, list = FALSE)
training <- merget[inTrain,]
testing <- merget[-inTrain,]