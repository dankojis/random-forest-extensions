library("party")
library(MASS)
library("glmertree")
library("caret")
source("simData_methods.r")
library("randomForest")
library("rpart")
library("fuzzyforest")
library("WGCNA")


#Data
# training data
n <- 100 # number of patients
T <-  5 # number of observations per patients

set.seed(100)

data <- as.data.frame(sim_3(n, T)) 
colnames(data)[401] <- "y"


# add patient/cluster information
for (i in 1:n){
  data$patient[(1+(i-1)*T):(i*T)] = rep(i,T)
}

# add first PC for each module
data_mat = data[,1:400]
data$PC1 = svd(data_mat[,1:100])$u[,1] 
data$PC2 = svd(data_mat[,101:200])$u[,1] 
data$PC3 = svd(data_mat[,201:300])$u[,1]  
data$PC4 = svd(data_mat[,301:400])$u[,1] 

# test data
n <- 30 # number of patients; OVERWRITE! n=30 now! 
T <-  5 # number of observations per patients
set.seed(101)
data_test <- as.data.frame(sim_3(n, T)) 
colnames(data_test)[401] <- "y"


# add patient/cluster information
for (i in 1:n){
  data_test$patient[(1+(i-1)*T):(i*T)] = rep(i,T)
}
# add PC
data_mat = data_test[,1:400]
data_test$PC1 = svd(data_mat[,1:100])$u[,1]  
data_test$PC2 = svd(data_mat[,101:200])$u[,1] 
data_test$PC3 = svd(data_mat[,201:300])$u[,1] 
data_test$PC4 = svd(data_mat[,301:400])$u[,1] 




# Methods for extracting names of splitting features used in a tree
# tree: a tree object; data: the train or test set
get_split_names = function(tree,data){
  # path: the string that contains all the node information
  paths <- pre:::list.rules(mytree, removecomplements = FALSE)
  vnames = names(data)
  # the regex for a variable
  # tomatch = paste(paste(var,"<="),"|",paste(var,">"),sep="")
  # match to tomatch in path
  tmp = vnames[sapply(sapply(vnames, FUN = function(var) grep(paste(paste(var,"<="),"|",paste(var,">"),sep=""), paths)), length) > 0]
  return (tmp)
}



##### Gtree
# regress_var = paste("PC",c(3),sep="")
# regress_var = paste("PC",c(1,2,3),sep="")
# regress_var = paste("V",c(1,2,3,301,302,303),sep="") # abs correct regressor
# regress_var = paste("V",c(1,301,65,305,9,100,101),sep="") # disturbation
regress_var = paste("V",c(1,2,3,301,302,303,5,185),sep="") # abs correct regressor
# regress_var = paste("V",301:400,sep="") # abs correct regressor


# split_var = paste("V",201:300,sep="") # always use all the features to split
split_var = paste("V",c(1,2,3,301,302,303,5,185),sep="") # abs correct regressor
# split_var = paste("V",c(3,2),sep="")

cluster_var = "patient"
Formula = as.formula(paste("y~",paste(regress_var,collapse = "+"),
                           "|",cluster_var,"|",
                           paste(split_var,collapse = "+")))
# mob_control_ = mob_control(maxdepth = 5)

system.time({
  lmm_tree <- lmertree(Formula, data = data,alpha=0.2) # you can change alpha = 0.2
})
# mean square error on test data
sum((predict(lmm_tree,newdata=data_test,re.form=NA)-data_test$y)**2)/(n*T)

# the selected features
get_split_names(lmm_tree$tree,data)

coef(lmm_tree)
plot(lmm_tree,which="tree",main = "glmertree")
