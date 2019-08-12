source('simData_methods.R')
source('./RE-EM Forest/functions.R')
library(REEMtree)
library(gtools)
library(rlist)
library(caret)


# Generate data
n <- 100
T <- 5
data <- as.data.frame(sim_2(n,T)) # genearate CS  data set (no random effect)
colnames(data)[colnames(data) == 'V401'] <- 'y'

X = as.data.frame(  data[,-ncol(data)]  ) # remove y value from for X matrix
y = data$y # assign target variable 

# add patient/cluster information
for (i in 1:n){
  data$patient[(1+(i-1)*T):(i*T)] = rep(i,T)
}

# 
data_test <- as.data.frame(sim_2(100,5)) # generate AR data set (no random effect--> use sim_3_RE() for RE)
X_test = as.data.frame(  data_test[,-ncol(data_test)]  ) # remove y value from for X matrix
y_test = data_test[,ncol(data_test)] # assign target variable

#  X_var <- paste("V",1:400,sep="")
#  Formula = as.formula(paste("y~",paste(X_var,collapse="+")))
# 
# 
# reem <- REEMtree(formula = Formula, data=data,random=~1|patient)
# plot(reem)



p <- ncol(X)
mtry = if (!is.null(y) && !is.factor(y)) max(floor(p/3), 1) else floor(sqrt(p))



#################   # parameter to include : dt_max_depth, mtry
random_forest_algorithm = function(train_data, ntree = 500){
  forest <- list()
  
  for(i in 1:ntree){
    boot <- bootstrap(data, nrow(data))
    X_boot <- boot[,1:(ncol(boot)-2)]   # FIX THIS   # this code depends on X matrix being all of orig. data except last two colums (y and patient)
    
    sub_feat <- subset_features(X_boot, mtry) # choose a subset of size mtry of original features
    formula = as.formula(paste("y~",paste(sub_feat,collapse="+"))) 
    
    #tree <- REEMtree(formula = formula, data=boot,random=~1|patient)
    tree <- rpart(formula,data=boot)
    
    forest <- list.append(forest, tree)
    
  }
  return(forest)
}


random_forest_predict =  function(forest_model, test_data){
  all_predictions <- as.data.frame(matrix( ,nrow(test_data),ncol=0)) #create empty dataframe
  
  for(i in 1:length(forest_model)){
    tree_predictions <- unlist(predict(forest_model[i], test_data)) # predictions from ith tree
    all_predictions <- as.data.frame(cbind(all_predictions,tree_predictions)) # concatenate predictions from each tree
  }
  
  predictions <- rowMeans(all_predictions) # calculate average of predictions from trees
  return(predictions)
  
}

rf <- random_forest_algorithm(data,ntree = 480)
pred <- random_forest_predict(rf, data_test)

baseline <- mean((pred-y_test)^2)


def feature_importance_dropcolumn(estimator, X, y, cv=5):
  
  base_accuracy = np.mean(cross_val_score(estimator, X, y, cv=cv))
column_indices = np.arange(X.shape[1]).astype(int)
drop_accuracies = np.zeros(column_indices.shape[0])

for idx in column_indices:
  mask = np.ones(column_indices.shape[0]).astype(bool)
mask[idx] = False
drop_accuracy = np.absolute(base_accuracy - np.mean(cross_val_score(estimator, X[:,mask], y, cv=cv)) )
drop_accuracies[idx] = drop_accuracy

return drop_accuracies




pred <- random_forest_predict(rf, data_test)
base_accuracy <- mean((pred-y_test)**2)

accuracy_differnce <- as.data.frame(matrix( ,1,ncol=0))

for(var in colnames(X_test[1:10])){
  data_perm <- data_test
  data_perm[var] <- permute(data_test[[var]])

  perm_predictions <- random_forest_predict(forest_model = forest_model, test_data = data_perm)
  perm_accuracy <- mean((perm_predictions-y_test)^2)
  
  accuracy_differnce[var] <- abs(base_accuracy - perm_accuracy)  #### This should be improved for compuatational efficiency (dataframe=bad)
  
}

var_importance <- colnames(sort(accuracy_differnce, decreasing = TRUE))































ntree= 4
MSE_all_trees <- c()
forest <- NA

for(tree in 1:ntree){
  boot_oob_list <- bootstrap_oob(data,nrow(data))  
  
  boot <- as.data.frame(boot_oob_list[1])
  X_boot <- boot[,1:(ncol(boot)-2)]   # FIX THIS   # this code depends on X matrix being all of orig. data except last two colums (y and patient)
  
  oob <- as.data.frame(boot_oob_list[2])
  
  
  sub_feat <- subset_features(X_boot, mtry) # choose a subset of size mtry of original features
  formula = as.formula(paste("y~",paste(sub_feat,collapse="+"))) 
  
  
  # reem <- REEMtree(formula = formula, data=boot,random=~1|patient)
  # plot(reem)
  # print(reem$Tree$frame$var)
  
  
  
  tree <- rpart(formula,data=boot)
  plot(tree)
  text(tree,use.n = TRUE)
  
  forest <- list.append(forest, tree)
  
  
  predictions <- predict(tree, oob)
  
  MSE_tree <- mean((predictions-oob$y)^2) # calculate MSE using Out-of-Bag data as test set for tree
  MSE_all_trees <- c(MSE_all_trees, MSE_tree)
  
}

MSE <- mean(MSE_all)







