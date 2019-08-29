source("./RE-EM Forest/reem_forest.R")
source("./RE-EM Forest/SimData.R")
library(tidyverse)
library(magrittr)
library(randomForest)
library("glmertree")
library(rlist)





n_values <- c(50,100,200)
T <-  5 # number of observations per patients

datasets <- list()
for(n in n_values){
  
  data <- sim_quad(n*2,T) # generate dataset twice as big -> half used for training, half for testing
  # add time_squared
  data$time2 = (data$time)^2
  
  datasets <- list.append(datasets, data)
  
}



###################### REEM Tree
df2 = data.frame(matrix(0,length(n_values),3))
colnames(df2) <- c("model", "n", "MSE")
df2$model <- "Longtree"

fixed_regress = c("time","time2")
fixed_split = c("treatment")
cluster = "patient"
var_select = paste("V",1:400,sep="")

i <- 1
for(set in datasets){
  ####
  set <- as.data.frame(set)

  n <- nrow(set)/(T*2) # size of train and test set
  data <- set[1:(T*n),] # assign first half of data to training set
  data_test <- set[(T*n+1):nrow(set),] # assign second half of data to test set
  ####
  
  
  mytree = Longtree(data,fixed_regress=fixed_regress,fixed_split=fixed_split,
                      var_select=var_select,cluster=cluster,Fuzzy=TRUE)
    
  error <- mean((predict(mytree,newdata=data_test)-data_test$y)**2)
  
  
  df2[i,"n"] <- n
  df2[i, "MSE"] <- error
  i <- i+1

}








for(n in n_values){
  set.seed(99) # ***seed should be same for each model! Test on same data
  T <-  5 # number of observations per patients
  data <- sim_quad(n,T)
  data <- data[, -401] # remove rand_int
  data$time2 = (data$time)^2   # add time_squared
  data %<>% select(-c("patient", "y"), c("patient", "y")) # **move patient and y to last 2 columns**
  X = data[,-c(404:405)]# remove y value from for X matrix
  y = data$y # assign target variable
  
  
  ######## MODEL
  features <- colnames(X)
  formula <- as.formula(paste("y~",paste(features,collapse="+"))) 
  
  reem.tree <- REEMtree(formula = formula, data=data,random=~1|patient)
  reem.pred <- predict(reem.tree, data_test, EstimateRandomEffects=FALSE)
  reem.error <-  mean((reem.pred-y_test)^2)
  
  df2[i,"n"] <- n
  df2[i, "MSE"] <- reem.error
  i <- i+1
}



