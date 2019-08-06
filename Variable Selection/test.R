# 1). Generate or load one data set. 

# 2). Random Forest: fit n_run (e.g. 100) number random forest models to data. For each model, record which variables
# are selected. Store MSE, too. Store  results in 'result_rf' where last
# row is the column averages

# 3). Fuzzy Forest: fit n_run fuzzy forest models to data. For each model, record which variables are selected and MSE. 
# Store results in 'results_ff' where last row is column averages. 

# 4). Random Forest Different Data:  Generate one test data set to be used for all error calculations.
# Repeat following n_run times: Generate a data set. Fit a random forest model. Record which of the 
# important variables are selected and record MSE (i.e. unlike previously, each 
# random forest model is fit on a newly generated dataset). 

# 5). Fuzzy Forest Different Data. Generate one test data set to be used for all error calculations. Repeat
# following 100 times: Generate a data set. Fit a fuzzy forest model. Record which of the 
# important variables are selected and store MSE. Store results in 'ff_dif_data'


source('simData_methods.R')
library(WGCNA)
library(randomForest)
library(fuzzyforest)
library(dplyr)


# 1).  load or generate data set
# data <- read.csv("    ")
n= 100   #number of patients
T = 5  # number of observations per patient
p = 400 # number of features

set.seed(131)
data <- sim_3(n,T,cor_feature = 0.8) # generate AR data set with random effect (use sim_3() for no RE)
#data <- sim_2(n,T,cor_feature = 0.8) # genearate CS  data set (no random effect)


X = as.data.frame(  data[,-ncol(data)]  ) # remove y value from for X matrix
y = data[,ncol(data)] # assign target variable


n_test= 30   #number of patients
T = 5  # number of observations per patient
p = 400 # number of features

set.seed(241)
data_test <- sim_3(n_test,T,cor_feature = 0.8) # generate AR data set (no random effect--> use sim_3_RE() for RE)
#data <- sim_2(n,T,cor_feature = 0.8) # genearate CS  data set (no random effect)


X_test = as.data.frame(  data_test[,-ncol(data_test)]  ) # remove y value from for X matrix
y_test = data_test[,ncol(data_test)] # assign target variable


# 2). 
#######################################################################3 
# Random Forests

n_run <-  100 # the number of times RF will run on the data set
n_top <-  10 # the top n_top variables will be selected

# create empty data frame to save simulation results in
result_rf <-  matrix(0,n_run+1,400+1) # the last row is for average
result_rf <-  data.frame(result_rf)
names(result_rf)[401] <-  "error"
names(result_rf)[1:400] <-  paste("V",1:400,sep="")

system.time({
  for(Repeat in 1:n_run){
    set.seed(Repeat+64) # change seed each loop
    
    rf <- randomForest(X,y) # fit random forest
    
    # error on the test set
    preds <- predict(rf, newdata=X_test)
    error = sum((y_test-preds)^2)/(n_test*T)
    
    
    # this is a quicker way to get the ranking (not juct choosing) of varibales 
    importance_order <- sort(rf$importance, decreasing = TRUE,index.return=TRUE) # sorts features by importance
    top_variables = importance_order$ix[1:n_top] # the ranking
    
    # If variable was selected as important, indicate with 1 (otherwise 0)
    for (i in 1:400){
      result_rf[Repeat,i] <- as.numeric(i %in% top_variables)
    }
    result_rf[Repeat,401] <- error
    print(Repeat)
  }
})
result_rf[n_run+1,] <-  colMeans(result_rf[1:n_run,])
 write.csv(result_rf,file = 'results_rf.csv')

# 3). 
####################################################################################
# Fuzzy Forests

n_run = 100 # the number of times RF will run on the data set
n_top = 10 # the top n_top variables will be selected

select_params <- select_control(number_selected = n_top) # select number of important features to keep

# create empty data frame to save simulation results in
result_ff = matrix(0,n_run+1,400+1) # the last row is for average
result_ff = data.frame(result_ff)
names(result_ff)[401] = "error"
names(result_ff)[1:400] = paste("V",1:400,sep="")

system.time({
  for(Repeat in 1:n_run){
    set.seed(Repeat+99) # change seed each loop
    
    ff <- wff(X,y, select_params = select_params)
    
    top_variables = ff$feature_list[,1]
    
    # error on the test set
    preds <- predict(ff, new_data=X_test)
    error = sum((y_test-preds)^2)/(n_test*T)
    
    # If variable was selected as important, indicate with 1 (otherwise 0)
    for (i in 1:400){
      result_ff[Repeat,i] <- as.numeric(paste("V",i,sep="") %in% top_variables)
    }
    result_ff[Repeat,401] <- error
    print(Repeat)
  }
})
result_ff[n_run+1,] = colMeans(result_ff[1:n_run,])

write.csv(result_ff,file = 'results_ff.csv')



# 4). 
################## ####################################################333
# Random Forests Different Data

n_run <-  100 # the number of times RF will run on the data set
n_top <-  20 # the top n_top variables will be selected

# create empty data frame to save simulation results in
rf_dif_data <-  matrix(0,n_run+1,400+1) # the last row is for average
rf_dif_data <-  data.frame(rf_dif_data)
names(rf_dif_data)[401] <-  "error"
names(rf_dif_data)[1:400] <-  paste("V",1:400,sep="")

system.time({
  for(Repeat in 1:n_run){
    set.seed(Repeat+102) # change seed each loop
    
    data <- sim_3(n,T,cor_feature = 0.8) # generate AR data set with random effect (use sim_3() for no RE)
    #data <- sim_2(n,T,cor_feature = 0.8) # genearate CS  data set (no random effect)
    
    X = as.data.frame(  data[,-ncol(data)]  ) # remove y value from for X matrix
    y = data[,ncol(data)] # assign target variable
    
    rf <- randomForest(X,y) # fit random forest
    
    # error on the test set
    preds <- predict(rf, newdata=X_test)
    error = sum((y_test-preds)^2)/(n_test*T)
    
    
    # this is a quicker way to get the ranking (not juct choosing) of varibales 
    importance_order <- sort(rf$importance, decreasing = TRUE,index.return=TRUE) # sorts features by importance
    top_variables = importance_order$ix[1:n_top] # the ranking
    
    # If variable was selected as important, indicate with 1 (otherwise 0)
    for (i in 1:400){
      rf_dif_data[Repeat,i] <- as.numeric(i %in% top_variables)
    }
    rf_dif_data[Repeat,401] <- error
    print(Repeat)
  }
})
rf_dif_data[n_run+1,] <-  colMeans(rf_dif_data[1:n_run,])

write.csv(rf_dif_data,file = 'rf_dif_data.csv')


# 5).
#######################################################################################3 
# Fuzzy Forests Different Data


n_run = 100 # the number of times RF will run on the data set
n_top = 20 # the top n_top variables will be selected

select_params <- select_control(number_selected = n_top) # select number of important features to keep

# create empty data frame to save simulation results in
ff_dif_data = matrix(0,n_run+1,400+1) # the last row is for average
ff_dif_data = data.frame(ff_dif_data)
names(ff_dif_data)[401] = "error"
names(ff_dif_data)[1:400] = paste("V",1:400,sep="")

system.time({
  for(Repeat in 1:n_run){
    set.seed(Repeat+443) # change seed each loop
    
    ff <- wff(X,y, select_params = select_params)
    
    top_variables = ff$feature_list[,1]
    
    # error on the test set
    preds <- predict(ff, new_data=X_test)
    error = sum((y_test-preds)^2)/(n_test*T)
    
    # If variable was selected as important, indicate with 1 (otherwise 0)
    for (i in 1:400){
      ff_dif_data[Repeat,i] <- as.numeric(paste("V",i,sep="") %in% top_variables)
    }
    ff_dif_data[Repeat,401] <- error
    print(Repeat)
  }
})
ff_dif_data[n_run+1,] = colMeans(ff_dif_data[1:n_run,])

write.csv(ff_dif_data,file = 'ff_dif_data.csv')



# save data set (for steps 2 and 3) and test data set
write.csv(data, file = 'data.csv')
write.csv(data_test, file = 'data_test.csv')

