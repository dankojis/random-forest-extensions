# 1). Generate or load one data set. 

# 2). Random Forest: fit 100 random forest models to data. For each model, record which of the important 
# variables (v1,v2,v3,v301,v302,v303) are selected. Store oob error, too. Store  results in 'result_rf' where last
# row is the column averages

# 3). Fuzzy Forest: fit 100 fuzzy forest models to data. For each model, record which of the important
# variables(v1,v2,v3,v301,v302,v303) are selected. Store results in 'results_ff' where last row 
# is column averages

# 4). Random Forest Different Data: Generate a data set. Fit a random forest model. Record which of the 
# important variables are selected and store oob error. Store results in 'rf_dif_data'.
# Repeat 100 times (i.e. unlike previously, each random forest model is fit on a newly generated dataset). 

# 5). Fuzzy Forest Different Data. Generate one test data set to be used for all error calculations. Repeat
# following 100 times: Generate a data set. Fit a fuzzy forest model. Record which of the 
# important variables are selected and store prediction error. Store results in 'ff_dif_data'


source('simData_methods.R')
library(WGCNA)
library(randomForest)
library(fuzzyforest)
library(dplyr)
#library(magrittr)


# 1).  load or generate data set
# data <- read.csv("    ")

  n= 100   #number of patients
  T = 5  # number of observations per patient
  p = 400 # number of features
  
  data <- sim_3_RE(n,T,cor_feature = 0.8)


X = data[,-ncol(data)] # remove y value from for X matrix
y = data[,ncol(data)] # assign target variable
y <- as.factor(ifelse(y < 0, 0,1)) # change to categorical y



################ 2). 
# Random Forests

result_rf <- data.frame(V1 = c(NA), V2 = c(NA),V3 = c(NA), V301 = c(NA),   # create empty data frame to
                     V302 = c(NA), V303 = c(NA), Error = c(NA))                        # save simulation results in

for(Repeat in 1:100){
  set.seed(Repeat+34) # change seed each loop

  rf <- randomForest(X,y) # fit random forest
  
  oob_error <- rf$err.rate[500] # OOB estimate of error rate
  
  
  importance_order <- sort(rf$importance, decreasing = TRUE) # sorts features by importance
  feature_threshold <- importance_order[10] # obtains IncNodePurity for 10th most important feature
  
  top_variables <- which(rf$importance >= feature_threshold) # stores index values for top 10 important features
  
  
  # If variable was selected as important, indicate with 1 (otherwise 0)
  result_rf[Repeat,1] <- as.numeric('1'%in% top_variables)
  result_rf[Repeat,2] <- as.numeric('2'%in% top_variables)
  result_rf[Repeat,3] <- as.numeric('3'%in% top_variables)
  result_rf[Repeat,4] <- as.numeric('301'%in% top_variables)
  result_rf[Repeat,5] <- as.numeric('302'%in% top_variables)
  result_rf[Repeat,6] <- as.numeric('303'%in% top_variables)
  result_rf[Repeat,7] <- oob_error
  print(result_rf)
  
}

# Create final row of column averages
result_rf[Repeat + 1, 1] <- sum(result_rf[1:Repeat,1])/Repeat
result_rf[Repeat + 1, 2] <- sum(result_rf[1:Repeat,2])/Repeat
result_rf[Repeat + 1, 3] <- sum(result_rf[1:Repeat,3])/Repeat
result_rf[Repeat + 1, 4] <- sum(result_rf[1:Repeat,4])/Repeat
result_rf[Repeat + 1, 5] <- sum(result_rf[1:Repeat,5])/Repeat
result_rf[Repeat + 1, 6] <- sum(result_rf[1:Repeat,6])/Repeat
result_rf[Repeat + 1, 7] <- sum(result_rf[1:Repeat,7])/Repeat


# write.csv(result_rf,file = 'results_rf.csv')


############### 3). 
# Fuzzy Forests 

select_params <- select_control(number_selected = 10) # select number of important features to keep

result_ff <- data.frame(V1 = c(NA), V2 = c(NA),V3 = c(NA), V301 = c(NA),   # create empty data frame to
                        V302 = c(NA), V303 = c(NA), Error = c(NA))                        # save simulation results in


test_data <- sim_3_RE(n,T,cor_feature = 0.8) # generate AR test data set
# test_data <- sim_2(n,T,cor_feature = 0.8) # genearate CS test data set

X_test = test_data[,-ncol(test_data)] # remove y value from for X matrix
y_test = test_data[,ncol(test_data)] # assign target variable
y_test <- as.factor(ifelse(y_test < 0, 0,1)) # change to categorical y

for(Repeat in 1:100){
  set.seed(Repeat+75)
  
  ff <- wff(X,y, select_params = select_params) # fit fuzzy forest
  
  
  preds <- predict(ff, new_data=X_test)
  accuracy <- sum(ifelse(preds == y_test, 1, 0))/dim(X_test)[1]
  error_rate <- 1 - accuracy
  
 
  
  # If variable was selected as important, indicate with 1 (otherwise 0)
  result_ff[Repeat,1] <- as.numeric('V1'%in% ff$feature_list[,1])
  result_ff[Repeat,2] <- as.numeric('V2'%in% ff$feature_list[,1])
  result_ff[Repeat,3] <- as.numeric('V3'%in% ff$feature_list[,1])
  result_ff[Repeat,4] <- as.numeric('V301'%in% ff$feature_list[,1])
  result_ff[Repeat,5] <- as.numeric('V302'%in% ff$feature_list[,1])
  result_ff[Repeat,6] <- as.numeric('V303'%in% ff$feature_list[,1])
  result_ff[Repeat,7] <- error_rate
  print(result_ff)
  
}

# Create final row of column averages
result_ff[Repeat + 1, 1] <- sum(result_ff[1:Repeat,1])/Repeat
result_ff[Repeat + 1, 2] <- sum(result_ff[1:Repeat,2])/Repeat
result_ff[Repeat + 1, 3] <- sum(result_ff[1:Repeat,3])/Repeat
result_ff[Repeat + 1, 4] <- sum(result_ff[1:Repeat,4])/Repeat
result_ff[Repeat + 1, 5] <- sum(result_ff[1:Repeat,5])/Repeat
result_ff[Repeat + 1, 6] <- sum(result_ff[1:Repeat,6])/Repeat
result_ff[Repeat + 1, 7] <- sum(result_ff[1:Repeat,7])/Repeat

# write.csv(result_ff,file = 'result_ff')


################## 4). 
# Random Forests Different Data

rf_dif_data <- data.frame(V1 = c(NA), V2 = c(NA),V3 = c(NA), V301 = c(NA),   # create empty data frame to
                        V302 = c(NA), V303 = c(NA), Error = c(NA))             # save simulation results in

n= 100   #number of patients
T = 5  # number of observations per patient
p = 400 # number of features

for(Repeat in 1:4){
  set.seed(Repeat+34) # change seed each loop
  
  
  data <- sim_3_RE(n,T,cor_feature = 0.8)
  
  X = data[,-ncol(data)] # remove y value from for X matrix
  y = data[,ncol(data)] # assign target variable
  y <- as.factor(ifelse(y < 0, 0,1)) # change to categorical y
  
  rf <- randomForest(X,y) # fit random forest
  
  oob_error <- rf$err.rate[500] # OOB estimate of error rate
  
  importance_order <- sort(rf$importance, decreasing = TRUE) # sorts features by importance
  feature_threshold <- importance_order[10] # obtains IncNodePurity for 10th most important feature
  
  top_variables <- which(rf$importance >= feature_threshold) # stores index values for top 10 important features
  
  
  # If variable was selected as important, indicate with 1 (otherwise 0)
  rf_dif_data[Repeat,1] <- as.numeric('1'%in% top_variables)
  rf_dif_data[Repeat,2] <- as.numeric('2'%in% top_variables)
  rf_dif_data[Repeat,3] <- as.numeric('3'%in% top_variables)
  rf_dif_data[Repeat,4] <- as.numeric('301'%in% top_variables)
  rf_dif_data[Repeat,5] <- as.numeric('302'%in% top_variables)
  rf_dif_data[Repeat,6] <- as.numeric('303'%in% top_variables)
  rf_dif_data[Repeat,7] <- oob_error
  print(rf_dif_data)
  
}

# Create final row of column averages
rf_dif_data[Repeat + 1, 1] <- sum(rf_dif_data[1:Repeat,1])/Repeat
rf_dif_data[Repeat + 1, 2] <- sum(rf_dif_data[1:Repeat,2])/Repeat
rf_dif_data[Repeat + 1, 3] <- sum(rf_dif_data[1:Repeat,3])/Repeat
rf_dif_data[Repeat + 1, 4] <- sum(rf_dif_data[1:Repeat,4])/Repeat
rf_dif_data[Repeat + 1, 5] <- sum(rf_dif_data[1:Repeat,5])/Repeat
rf_dif_data[Repeat + 1, 6] <- sum(rf_dif_data[1:Repeat,6])/Repeat
rf_dif_data[Repeat + 1, 7] <- sum(rf_dif_data[1:Repeat,7])/Repeat


# write.csv(rf_dif_data,file = 'rf_dif_data.csv')


################## 5). 
# Fuzzy Forests Different Data

ff_dif_data <- data.frame(V1 = c(NA), V2 = c(NA),V3 = c(NA), V301 = c(NA),   # create empty data frame to
                          V302 = c(NA), V303 = c(NA), Error = c(NA))             # save simulation results in

n= 100   #number of patients
T = 5  # number of observations per patient
p = 400 # number of features


select_params <- select_control(number_selected = 20) # select number of important features to keep

ff_dif_data <- data.frame(V1 = c(NA), V2 = c(NA),V3 = c(NA), V301 = c(NA),   # create empty data frame to
                        V302 = c(NA), V303 = c(NA), Error = c(NA))                        # save simulation results in

test_data <- sim_3_RE(n,T,cor_feature = 0.8) # generate AR test data set
# test_data <- sim_2(n,T,cor_feature = 0.8) # genearate CS test data set

X_test = test_data[,-ncol(test_data)] # remove y value from for X matrix
y_test = test_data[,ncol(test_data)] # assign target variable
y_test <- as.factor(ifelse(y_test < 0, 0,1)) # change to categorical y

for(Repeat in 1:100){
  set.seed(Repeat+94) # change seed each loop
  
  
  data <- sim_3_RE(n,T,cor_feature = 0.8)
  
  X = data[,-ncol(data)] # remove y value from for X matrix
  y = data[,ncol(data)] # assign target variable
  y <- as.factor(ifelse(y < 0, 0,1)) # change to categorical y
  
  
  ff <- wff(X,y, select_params = select_params) # fit fuzzy forest
  
  preds <- predict(ff, new_data=X_test)
  accuracy <- sum(ifelse(preds == y_test, 1, 0))/dim(X_test)[1]
  error_rate <- 1 - accuracy
  

  # If variable was selected as important, indicate with 1 (otherwise 0)
  ff_dif_data[Repeat,1] <- as.numeric('V1'%in% ff$feature_list[,1])
  ff_dif_data[Repeat,2] <- as.numeric('V2'%in% ff$feature_list[,1])
  ff_dif_data[Repeat,3] <- as.numeric('V3'%in% ff$feature_list[,1])
  ff_dif_data[Repeat,4] <- as.numeric('V301'%in% ff$feature_list[,1])
  ff_dif_data[Repeat,5] <- as.numeric('V302'%in% ff$feature_list[,1])
  ff_dif_data[Repeat,6] <- as.numeric('V303'%in% ff$feature_list[,1])
  ff_dif_data[Repeat,7] <- error_rate
  print(ff_dif_data)
  
}

# Create final row of column averages
ff_dif_data[Repeat + 1, 1] <- sum(ff_dif_data[1:Repeat,1])/Repeat
ff_dif_data[Repeat + 1, 2] <- sum(ff_dif_data[1:Repeat,2])/Repeat
ff_dif_data[Repeat + 1, 3] <- sum(ff_dif_data[1:Repeat,3])/Repeat
ff_dif_data[Repeat + 1, 4] <- sum(ff_dif_data[1:Repeat,4])/Repeat
ff_dif_data[Repeat + 1, 5] <- sum(ff_dif_data[1:Repeat,5])/Repeat
ff_dif_data[Repeat + 1, 6] <- sum(ff_dif_data[1:Repeat,6])/Repeat
ff_dif_data[Repeat + 1, 7] <- sum(ff_dif_data[1:Repeat,7])/Repeat


#write.csv(ff_dif_data, file = 'ff_dif_data.csv')
