source("./RE-EM Forest/reem_forest.R")
source("./RE-EM Forest/SimData.R")
library(tidyverse)
library(magrittr)
library(randomForest)
library("glmertree")




# TESTING DATA
######################################################################
set.seed(6)
n_test <- 100 # number of patients
T <-  5 # number of observations per patients

set.seed(115)

data_test <- sim_quad(n_test, T)
data_test <- data_test[, -401] # remove rand_int

data_test$time2 = (data_test$time)^2 # add time_squared

data_test %<>% select(-c("patient", "y"), c("patient", "y"))
X_test = data_test[,-c(404:405)]# remove y value from for X matrix
y_test = data_test$y # assign target variable
#######################################################################


####################### Random Forest

n_values <- c(10,20,30,40,50,60,70,80,100,150,200,300,400,500,1000)

df = data.frame(matrix(0,length(n_values),3))
colnames(df) <- c("model", "n", "MSE")
df$model <- "Random Forest"

i <- 1
for(n in n_values){
  set.seed(99) # ***seed should be same for each model! Test on same data
  T <-  5 # number of observations per patients
  data <- sim_quad(n,T)
  data <- data[, -401] # remove rand_int
  data$time2 = (data$time)^2   # add time_squared
  data %<>% select(-c("patient", "y"), c("patient", "y")) # **move patient and y to last 2 columns**
  X = data[,-c(404:405)]# remove y value from for X matrix
  y = data$y # assign target variable
  
  
  rf <- randomForest(X,y)
  pred <- predict(rf, X_test)
  mse <- mean((pred-y_test)^2)
  
  df[i,"n"] <- n
  df[i, "MSE"] <- mse
  i <- i+1
}


###################### REEM Tree
n_values <- c(10,20,30,40,50,60,70,80,100,150,200,300,400,500,1000)

df2 = data.frame(matrix(0,length(n_values),3))
colnames(df2) <- c("model", "n", "MSE")
df2$model <- "REEM Tree"

i <- 1
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


###################### Bagging REEM Trees
n_values <- c(10,20,30,40,50,60,70,80,100,150,200,300,400,500,1000)

df3 = data.frame(matrix(0,length(n_values),3))
colnames(df3) <- c("model", "n", "MSE")
df3$model <- "Bagging REEM Trees"

i <- 1
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
  
  reem.bag <- bagging_algorithm(data, ntree = 100) ### CHANGE ntree!!!
  reem.bag.pred <- forest_predict(reem.bag, data_test)
  reem.bag.error <- mean((reem.bag.pred-y_test)^2)

  df3[i,"n"] <- n
  df3[i, "MSE"] <- reem.bag.error
  i <- i+1
}


###################### Longtrees
# testing data
n_test <- 100 # number of patients
T <-  5 # number of observations per patients
set.seed(101)
data_test <- sim_quad(n_test,T)
# data_test <- sim_quad(n_test, T)
data_test$time2 = (data_test$time)^2

n_values <- c(10,20,30,40,50,60,70,80,100,150,200,300,400,500,1000)

df4 = data.frame(matrix(0,length(n_values),3))
colnames(df4) <- c("model", "n", "MSE")
df4$model <- "Longtree"

fixed_regress = c("time","time2")
fixed_split = c("treatment")
cluster = "patient"
var_select = paste("V",1:400,sep="")

i <- 1
for(n in n_values){
  set.seed(99) # ***seed should be same for each model! Test on same data
  
  # training data
  T <-  5 # number of observations per patients

  data <- sim_quad(n,T)
  # add time_squared
  data$time2 = (data$time)^2
  
  

  
  
  
  
  
  
  ######## MODEL
  mytree = Longtree(data,fixed_regress=fixed_regress,fixed_split=fixed_split,
                    var_select=var_select,cluster=cluster,Fuzzy=TRUE)
  mytree.error <- mean((predict(mytree,newdata=data_test)-data_test$y)**2)
  
  
  df4[i,"n"] <- n
  df4[i, "MSE"] <- mytree.error
  i <- i+1
}












df_total <- rbind(df, df2, df3)


write.csv(df_total,file = 'model_comparison4.csv')


# XY Scatterplot
ggplot(df_total, aes(x=n, y=MSE)) +
  geom_point(aes( color=model))+
  #geom_point(aes(shape=model),col='black', size=1, shape=model) + 
  aes(col=model)+ #can add 3rd dimension, can add dimension with size too. 
  geom_smooth(method="loess", se=FALSE) + # se=FALSE to turnoff confidence bands 
  #xlim(c(10, 20)) + ylim(c(3, 5)) +  # set X and Y boundaries
  #scale_x_continuous(breaks=seq(0, 40, 5), labels = sprintf("%1.2f%%", seq(0, 40, 5)))+  # set ticks/text on axis
  labs(title="Model Comparison: Mean Squared Error vs Sample Size", subtitle="subtitle", y="Mean Squared Error (MSE)", x="Sample Size (n)", caption="caption")
