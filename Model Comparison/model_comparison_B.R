source("./RE-EM Forest/reem_forest.R")
source("./RE-EM Forest/SimData.R")
library(tidyverse)
library(magrittr)
library(randomForest)
library("glmertree")
library(rlist)





set.seed(100)
n_values <- c(10,20,30,40,50,60,70,80,100,150,200,300,400,500,1000)
T <-  5 # number of observations per patients

datasets <- list()
for(n in n_values){
  
  data <- sim_quad(n,T) # generate training data set
  data$time2 = (data$time)^2   # add time_squared

  data_test <- sim_quad(n,T) # generate test data set
  data_test$time2 = (data_test$time)^2   # add time_squared

  datasets <- list.append(datasets, list(data,data_test))
}






######## Random Forest
df1 = data.frame(matrix(0,length(n_values),3))
colnames(df1) <- c("model", "n", "MSE")
df1$model <- "Random Forest"


i <- 1
for(set in datasets){
  
  data <- set[[1]]
  data_test <- set[[2]]
  n <- nrow(data)/T # size of train and test set
  
  data %<>% select(-c("patient", "y"), c("patient", "y")) # **move patient and y to last 2 columns**
  X = data[,-c(401,405,406)]# remove y value from for X matrix
  y = data$y # assign target variable
  
  data_test %<>% select(-c("patient", "y"), c("patient", "y")) # **move patient and y to last 2 columns**
  X_test = data_test[,-c(401,405,406)]# remove y value from for X matrix
  y_test = data_test$y # assign target variable 
  
  print(nrow(data))
  print(nrow(data_test))
  
  rf <- randomForest(X,y)
  pred <- predict(rf, X_test)
  mse <- mean((pred-y_test)^2)
  
  
  df1[i,"n"] <- n
  df1[i, "MSE"] <- mse
  i <- i+1
  
}


######## REEM TREE
df2 = data.frame(matrix(0,length(n_values),3))
colnames(df2) <- c("model", "n", "MSE")
df2$model <- "RE-EM Tree"


i <- 1
for(set in datasets){
  
  data <- set[[1]]
  data_test <- set[[2]]
  n <- nrow(data)/T # size of train and test set
  
  data %<>% select(-c("patient", "y"), c("patient", "y")) # **move patient and y to last 2 columns**
  X = data[,-c(401,405,406)]# remove y value from for X matrix
  y = data$y # assign target variable
  
  data_test %<>% select(-c("patient", "y"), c("patient", "y")) # **move patient and y to last 2 columns**
  X_test = data_test[,-c(401,405,406)]# remove y value from for X matrix
  y_test = data_test$y # assign target variable 
  
  print(nrow(data))
  print(nrow(data_test))
  
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



######## Generalized Lineear Mixed Model Trees
df3 = data.frame(matrix(0,length(n_values),3))
colnames(df3) <- c("model", "n", "MSE")
df3$model <- "GLMM Tree"


cluster = "patient"
var_select = c(paste("V",1:400,sep=""),"time","time2")

i <- 1
for(set in datasets[9:15]){
  
  data <- set[[1]]
  data_test <- set[[2]]
  n <- nrow(data)/T # size of train and test set
  
  print(nrow(data))
  print(nrow(data_test))
  
  formula = as.formula(paste("y~",paste(var_select,collapse = "+"),
                             "|",cluster,"|",
                             paste(var_select,collapse = "+")))
  
  
  tree = lmertree(formula, data = data)
  
  error <- mean((predict(tree,newdata=data_test)-data_test$y)**2)
  
  
  df3[i,"n"] <- n
  df3[i, "MSE"] <- error
  i <- i+1
  
}






















######## Longtree
df4 = data.frame(matrix(0,length(n_values),3))
colnames(df4) <- c("model", "n", "MSE")
df4$model <- "Longtree"

fixed_regress = c("time","time2")
fixed_split = c("treatment")
cluster = "patient"
var_select = paste("V",1:400,sep="")

i <- 1
for(set in datasets){
  
  data <- set[[1]]
  data_test <- set[[2]]
  n <- nrow(data)/T # size of train and test set
  
  print(nrow(data))
  print(nrow(data_test))
  
  mytree = Longtree(data,fixed_regress=fixed_regress,fixed_split=fixed_split,
                    var_select=var_select,cluster=cluster,Fuzzy=TRUE)
  
  error <- mean((predict(mytree,newdata=data_test)-data_test$y)**2)
  
  
  df4[i,"n"] <- n
  df4[i, "MSE"] <- error
  i <- i+1
  
}








prev <- model_comparison2[,-1]

df_total <- rbind(df4, prev)


df3 <- df3[1:7,]
df3.1 <- df3[2:7,]
dfq <- rbind(df4,df1,df2,df3.1)

df_total <- rbind(df4,df1,df2,df3.1)

write.csv(df_total, file = "results.csv")


# XY Scatterplot
ggplot(df_total, aes(x=n, y=MSE)) +
  geom_point(aes(color=model))+
  #geom_point(aes(shape=model),col='black', size=1, shape=model) + 
  aes(col=model)+ #can add 3rd dimension, can add dimension with size too. 
  geom_smooth(method="loess", se=FALSE) + # se=FALSE to turnoff confidence bands 
  #xlim(c(10, 20)) + ylim(c(3, 5)) +  # set X and Y boundaries
  #scale_x_continuous(breaks=seq(0, 40, 5), labels = sprintf("%1.2f%%", seq(0, 40, 5)))+  # set ticks/text on axis
  labs(title="Model Comparison: Mean Squared Error vs Sample Size", subtitle="", y="Mean Squared Error (MSE)", x="Sample Size (n)", caption="")




