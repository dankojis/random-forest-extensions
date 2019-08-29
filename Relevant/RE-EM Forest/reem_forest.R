source('simData_methods.R')
source('./RE-EM Forest/functions.R')
library(REEMtree)
library(gtools)
library(rlist)
library(caret)



#GENERATE DATA       # Date Structured as follows: X variables, y variable, patient/cluster variable
#######################################################################################################
# n <- 100
# T <- 5
# data <- as.data.frame(sim_2(n,T)) # genearate CS  data set (no random effect)
# colnames(data)[colnames(data) == 'V401'] <- 'y'
# 
# X = as.data.frame(  data[,-ncol(data)]  ) # remove y value from for X matrix
# y = data$y # assign target variable
# # add patient/cluster information
# for (i in 1:n){
#   data$patient[(1+(i-1)*T):(i*T)] = rep(i,T)
# }
# 
# 
# # Testing data
# data_test <- as.data.frame(sim_2(100,5)) # generate AR data set (no random effect--> use sim_3_RE() for RE)
# X_test = as.data.frame(  data_test[,-ncol(data_test)]  ) # remove y value from for X matrix
# y_test = data_test[,ncol(data_test)] # assign target variable
# # add patient/cluster information
# for (i in 1:n){
#   data_test$patient[(1+(i-1)*T):(i*T)] = rep(i,T)
# }
#######################################################################################################


# RE-EM Forest Functions

# ***Curently data must have precise format
#################   # parameter to include : dt_max_depth, mtry
# ** algorithm subsets features once for each tree. Does NOT subset at each step of each decision tree. This is issue**
random_forest_algorithm = function(train_data, ntree=500, mtry=max(floor((ncol(train_data)-2)/3), 1) ){
  forest <- list()
  
  for(i in 1:ntree){
    boot <- bootstrap(data, nrow(data))
    X_boot <- boot[,1:(ncol(boot)-2)]   # FIX THIS   # this code depends on X matrix being all of orig. data except last two colums (y and patient)
    
    sub_feat <- subset_features(X_boot, mtry) # choose a subset of size mtry of original features
    formula = as.formula(paste("y~",paste(sub_feat,collapse="+"))) 
    
    
    ##### Decision Tree Implemenation
    tree <- REEMtree(formula = formula, data=boot,random=~1|patient)
    # tree <- rpart(formula,data=boot)
    #####
    
    
    forest <- list.append(forest, tree)
    
  }
  return(forest)
}



# ***Curently data must have precise format
#################   # parameter to include : dt_max_depth, mtry
bagging_algorithm = function(train_data, ntree=500, mtry=max(floor((ncol(train_data)-2)/3), 1) ){
  forest <- list()
  
  for(i in 1:ntree){
    boot <- bootstrap(data, nrow(data))
    X_boot <- boot[,1:(ncol(boot)-2)]   # FIX THIS   # this code depends on X matrix being all of orig. data except last two colums (y and patient)
    
    features <- colnames(X_boot)
    formula = as.formula(paste("y~",paste(features,collapse="+"))) 
    
    
    ##### Decision Tree Implemenation
    tree <- REEMtree(formula = formula, data=boot,random=~1|patient)
    # tree <- rpart(formula,data=boot)
    #####
    
    
    forest <- list.append(forest, tree)
    
  }
  return(forest)
}



# predict function: input (1) a forest_model returned from the random_forest_algorithm() function 
# and (2) test data. Returns predictions for the test data. 
forest_predict =  function(forest_model, test_data){
  all_predictions <- as.data.frame(matrix( ,nrow(test_data),ncol=0)) #create empty dataframe
  
  for(i in 1:length(forest_model)){
    tree_predictions <- unlist(predict(forest_model[i], test_data, EstimateRandomEffects=FALSE)) # predictions from ith tree
    all_predictions <- as.data.frame(cbind(all_predictions,tree_predictions)) # concatenate predictions from each tree
  }
  
  predictions <- rowMeans(all_predictions) # calculate average of predictions from trees
  return(predictions)
  
}


#reem.forest <- random_forest_algorithm(data, ntree = 500) #  ~ 5 minutes to run


# *** Computationally slow ~20 min for example data
###### Permutation Test for Variable Importance
# returns list of [1]names of variables in order of importance [2]dataframe of varaibles and their error difference
forest_importance = function(forest_model, test_data){
  
  X_test = as.data.frame(  test_data[,-ncol(test_data)]  ) # remove y value from for X matrix
  y_test = test_data[,ncol(test_data)] # assign target variable
  
  pred <- forest_predict(forest_model, test_data)
  base_error <- mean((pred-y_test)^2) # baseline error from non-permutated data
  error_difference <- as.data.frame(matrix( ,1,ncol=0)) #create empty df to store
  
  for(var in colnames(X_test)){
    data_perm <- test_data
    data_perm[var] <- permute(test_data[[var]]) # permute column of interest
    
    perm_predictions <- forest_predict(forest_model = forest_model, test_data = data_perm)
    perm_error <- mean((perm_predictions-y_test)^2)
    
    error_difference[var] <- abs(base_error - perm_error)  #### **computationally inefficient (dataframe=bad)
    
  }
  var_importance <- colnames(sort(error_difference, decreasing = TRUE))
  return(list(var_importance, error_difference))
  
}








