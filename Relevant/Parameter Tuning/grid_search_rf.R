source('Data Generator.R')
library(randomForest)




# set.seed(100)
# n = 100
# imp_mod = c(1,4)
# var_noise = 1
# train = sim_time(n,imp_mod=imp_mod, var_noise=var_noise)
# train$time2 <- (train$time)^2
# 
# 

# validation set data
set.seed(102) # seed for validation set
test <- sim_time(n,imp_mod=imp_mod, var_noise=var_noise)
test$time2 = (test$time)^2


# size of training dataset to best tested
n_values <- c(100, 200, 300, 400, 500)

# Paramters to test
mtry <- c(100,150,200,250,300,350,400)
nodesize <- c(5)


df <- data.frame(n = c(NA),mtry = c(NA), nodesize= c(NA), MSE = c(NA))   # create empty data frame 



i <- 1
for(n in n_values){
  set.seed(100)
  train = sim_time(n,imp_mod=imp_mod, var_noise=var_noise)
  train$time2 <- (train$time)^2
  
  X = train[,-c(401,404,405)]# remove y value, patient, and rand_int from for X matrix
  y = train$y # assign target variable
  
  
  for(m in mtry){
    for(ns in nodesize){
      
          print(m)
          print(ns)
          print(n)
          
          rf = randomForest(X,y, mtry = m, nodesize = ns)
          
          mse <- mean((predict(rf,newdata=test)-test$y)^2)
          
          
          df[i,"n"] <- n
          df[i, "MSE"] <- mse
          df[i, "nodesize"] <- ns
          df[i, "mtry"] <- m
          
          
          i <- i+1
        }
      }
    }
  
  
# save dataframe as csv
write.csv(df, file = "rf_tuning_mtry_n.csv")
  








