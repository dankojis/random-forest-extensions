source("./RE-EM Forest/reem_forest.R")
source("./RE-EM Forest/SimData.R")
library(tidyverse)
library(magrittr)
library(randomForest)
library("glmertree")


######################### **** Genrate one test set upfront and use for all tests. Don't keep Generating ########


# training data
n <- 100 # number of patients
T <-  5 # number of observations per patients

set.seed(110)

data <- sim_quad(n,T)

data <- data[, -401] # remove rand_int

# add time_squared
data$time2 = (data$time)^2

data %<>% select(-c("patient", "y"), c("patient", "y"))

X = data[,-c(404:405)]# remove y value from for X matrix
y = data$y # assign target variable


# testing data
n_test <- 100 # number of patients
T <-  5 # number of observations per patients

set.seed(111)

data_test <- sim_quad(n_test, T)
data_test <- data_test[, -401] # remove rand_int


# add time_squared
data_test$time2 = (data_test$time)^2

data_test %<>% select(-c("patient", "y"), c("patient", "y"))
X_test = data_test[,-c(404:405)]# remove y value from for X matrix
y_test = data_test$y # assign target variable



reem.forest <- random_forest_algorithm(data, ntree = 500)
reem.forest.pred <- forest_predit(reem.forest, data_test)
reem.forest.error <-  mean((reem.forest.pred-y_test)^2)

rf <- randomForest(X,y)
rf.pred <- predict(rf, X_test)
rf.error <- mean((rf.pred-y_test)^2)



####################### Random Forest

n_values <- c(10,20,40,60,80,100,200,300,500)

df = data.frame(matrix(0,length(n_values),3))
colnames(df) <- c("model", "n", "MSE")
df$model <- "Random Forest"

i <- 1
for(n in n_values){
  
  T <-  5 # number of observations per patients
  data <- sim_quad(n,T)
  data <- data[, -401] # remove rand_int
  data$time2 = (data$time)^2   # add time_squared
  data %<>% select(-c("patient", "y"), c("patient", "y")) # **move patient and y to last 2 columns**
  X = data[,-c(404:405)]# remove y value from for X matrix
  y = data$y # assign target variable
  
  # testing data
  n_test <- 100 # number of patients
  T <-  5 # number of observations per patients
  data_test <- sim_quad(n_test, T)
  data_test <- data_test[, -401] # remove rand_int
  data_test$time2 = (data_test$time)^2 # add time_squared
  data_test %<>% select(-c("patient", "y"), c("patient", "y"))
  X_test = data_test[,-c(404:405)]# remove y value from for X matrix
  y_test = data_test$y # assign target variable
  ###
  
  ######## MODELS
  # reem.forest <- random_forest_algorithm(data, ntree = 500)
  # reem.forest.pred <- forest_predit(reem.forest, data_test)
  # reem.forest.error <-  mean((reem.forest.pred-data$y)^2)
  
  rf <- randomForest(X,y)
  pred <- predict(rf, X_test)
  mse <- mean((pred-y_test)^2)
  
  df[i,"n"] <- n
  df[i, "MSE"] <- mse
 i <- i+1
}



##### REEM Forest

n_values <- c(10,20,40,60,80,100,200,300,500)

df2 = data.frame(matrix(0,length(n_values),3))
colnames(df2) <- c("model", "n", "MSE")
df2$model <- "REEM Forest"

i <- 1
for(n in n_values){
  
  T <-  5 # number of observations per patients
  data <- sim_quad(n,T)
  data <- data[, -401] # remove rand_int
  data$time2 = (data$time)^2   # add time_squared
  data %<>% select(-c("patient", "y"), c("patient", "y")) # **move patient and y to last 2 columns**
  X = data[,-c(404:405)]# remove y value from for X matrix
  y = data$y # assign target variable
  
  # testing data
  n_test <- 100 # number of patients
  T <-  5 # number of observations per patients
  data_test <- sim_quad(n_test, T)
  data_test <- data_test[, -401] # remove rand_int
  data_test$time2 = (data_test$time)^2 # add time_squared
  data_test %<>% select(-c("patient", "y"), c("patient", "y"))
  X_test = data_test[,-c(404:405)]# remove y value from for X matrix
  y_test = data_test$y # assign target variable
  ###
  
  ######## MODELS
  reem.forest <- random_forest_algorithm(data, ntree = 500)
  reem.forest.pred <- forest_predit(reem.forest, data_test)
  reem.forest.error <-  mean((reem.forest.pred-y_test)^2)

  df2[i,"n"] <- n
  df2[i, "MSE"] <- reem.forest.error
  i <- i+1
}




df_total <- rbind(df, df2)




dfg <- rbind(df1,df2,df3,df4)

# XY Scatterplot
ggplot(df_total, aes(x=n, y=MSE)) +
  geom_point(col='black', size=1) + 
  aes(col=model)+ #can add 3rd dimension, can add dimension with size too. 
  geom_smooth(method="loess", se=FALSE) + # se=FALSE to turnoff confidence bands 
  #xlim(c(10, 20)) + ylim(c(3, 5)) +  # set X and Y boundaries
  #scale_x_continuous(breaks=seq(0, 40, 5), labels = sprintf("%1.2f%%", seq(0, 40, 5)))+  # set ticks/text on axis
  labs(title="Model Comparison: Mean Squared Error vs Sample Size", subtitle="subtitle", y="Mean Squared Error (MSE)", x="Sample Size (n)", caption="caption")



