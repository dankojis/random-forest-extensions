# Helper functions for reem_forest.R


# function takes a random sample (of size mtry) of features from X matrix. Returns variable names
subset_features = function(X_matrix= X, mtry){
  features <- colnames(X)
  subset <- sample(features, mtry, replace = FALSE)
  return(subset)
}


# returns bootstrap dataset of size n
bootstrap = function(data, n){
  indices <- sample.int(nrow(data),n, replace = TRUE)
  bootstrap_data <- data[indices,]
  return(bootstrap_data)
}


# returns boostrap dataset of size n and out-of-bag dataset for samples not chosen in original bootstrap.
bootstrap_oob = function(data, n){
  indices <- sample.int(nrow(data),n, replace = TRUE)
  bootstrap_data <- data[indices,]
  oob_data <- data[-indices,]
  
  return(list(bootstrap_data, oob_data))
}



