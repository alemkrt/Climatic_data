#These functions calculate the relative significance of explanatory variables to predict treeline location

# treel_data data frame should be imported before running these functions.
# This should have the treeline response variable and a set of potential explanatory variables 

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#Testing variable importance in Random forest model
library(caret)
library(tidyverse)
varImpiter <- function (dtable, times_1, times_2){
  tc <- trainControl(method = "cv", number = 5)  
  list1 <- list()
  for (i in 1:times_1) {
    samp <- dtable |>
      group_by(treeline) |>
      slice_sample(n = 1000) |>
      ungroup()
    column_list <- vector("list", times_2)
    for (j in 1:times_2) {
      column_list[[j]] <- varImp(train (treeline~., data=samp, method="rf", ntree=1000, trControl = tc))$importance
      print(paste("iteration", j, "of", i, "is done"))
    }
    result_df <- data.frame(do.call(cbind, column_list))
    result <- apply(result_df, 1, mean)
    var_name <- paste0("iter", i)
    assign(var_name, result)
    list1 <- append(list1, list(get(var_name)))
    print(paste("outer iteration", i, "is done"))
  }
  # a <- rbind(iter1, iter2, iter3, iter4, iter5, iter6, iter7, iter8, iter9, iter10)
  return <- do.call(rbind, list1)
  # var_list <- list()
  # for(i in 1:times_1) {
  #   var_list[[i]] <- get(paste0("iter", i))
  # return <- as.data.frame(do.call(cbind, var_list))}
}

set.seed(100200)
# 20 is the number of independent samples and 20 runs of random forest model 
# Other values can be inserted
rf_varimp <- varImpiter(treel_data, 200, 20)
results <- data.frame(
  Mean = apply(rf_varimp, 2, mean),
  SD = apply(rf_varimp, 2, sd)
)
print(results)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Testing accuracies of different models with bootstrap

bootstrap_rf <- function (data,formula,m){
  # Create a bootstrap sample
  samp <- data |>
    group_by(treeline) |>
    slice_sample(n = 1000) |>
    ungroup()
  # Train gbm model
  #Insert the particular explanatory variables(s) after ~ 
  rf <- train (treeline~Tquart+GP5sum+TJul+GP0.9mean+Tmax+GP5mean, data=samp, method="rf", trControl = tc)
  # Extract accuracy from cross-validation
  return <- max(rf$results$Accuracy)
  }

bootstrap_gbm <- function (data,formula,m){
  # Create a bootstrap sample
  samp <- data |>
    group_by(treeline) |>
    slice_sample(n = 1000) |>
    ungroup()
  # Train gbm model
  #Insert the particular explanatory variables(s) after ~ 
  gbm <- train (treeline~GP0.9mean, data=samp, method="gbm", trControl = tc)
  # Extract accuracy from cross-validation
  return <- max(gbm$results$Accuracy)
}

# Calculate mean and standard deviation of accuracies
set.seed(42)  # For reproducibility
#insert the needed formula as an argument to bootstrap_rf
val <- replicate(50, bootstrap_gbm(treel_data,treeline~., 500))
print (c(mean=mean(val), sd=sd(val)))

