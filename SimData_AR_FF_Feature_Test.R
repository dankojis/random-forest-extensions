# Purpose: Generate a dataset with autoregresive covariance structure within individuals (n). 
#          Create target variable, y, as a funtion of specified variables. Fit fuzzy forest
#          to dataset and examine the feature importance and selection. Store the results of 
#          which features were correctly chosen and which wer not. Repeat this process 50 times. 
#          


source("Data_AR.R")

# Create n number of individuals, T observations per individual, p features
n= 100 
T = 10
p = 400
varnoise <- 0  ######### currently no noise - should change

select_params = select_control(number_selected = 6) # the number of features the fuzzy forrest will select

result <- data.frame(V1 = c(NA), V2 = c(NA),V3 = c(NA), V301 = c(NA),   # create empty data frame to
                     V302 = c(NA), V303 = c(NA))                        # save simulation results in


# run 50 different simulated datasets, fit fuzzy forest to each, and display the selected feature list
for(Repeat in 1:50){
  set.seed(Repeat+32)
  
  X <- as.matrix(Data_AR(T, varnoise))
  
  
  # generate AR dataset
  for (i in 2:n){
    new <- Data_AR(T,varnoise)
    X <- rbind(X, new)
  }
  
  y <- X[,p+1] # assign the last column as the target variable
  X <- as.data.frame(X[,1:p]) # select only the feature columns 
  
  ff <- wff(X, y, select_params = select_params )    # currently uses default parameters
  
  # If variable was selected as important, indicate with 1 (otherwise 0)
  result[Repeat,1] <- as.numeric('V1'%in% ff$feature_list[,1])
  result[Repeat,2] <- as.numeric('V2'%in% ff$feature_list[,1])
  result[Repeat,3] <- as.numeric('V3'%in% ff$feature_list[,1])
  result[Repeat,4] <- as.numeric('V301'%in% ff$feature_list[,1])
  result[Repeat,5] <- as.numeric('V302'%in% ff$feature_list[,1])
  result[Repeat,6] <- as.numeric('V303'%in% ff$feature_list[,1])
  print(result)
  
}


result[,7] <- rowSums(result)
colnames(result)[7]<-"Number Correct Variables"
# save(result, file = "wff_n100_T10_select6_.Rda")


