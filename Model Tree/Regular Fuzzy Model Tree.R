library("partykit")
source("simData_methods.r")
library("party")
library(MASS)
library("glmertree")
library("caret")
library("randomForest")
library("rpart")
library("fuzzyforest")
library("WGCNA")
library("pre")



set.seed(100)

# No time structure
n = 500 # num of observations
p = 400
p0 = 100

cor_feature = 0.8

#### covariance matrix between features: it is either 0 (independent) or cor_feature ####
cov_feature = matrix(0,nrow = p, ncol = p)
# cov within the first three modules
cov_star = matrix(cor_feature,nrow = p0,ncol = p0)
diag(cov_star)=1
# put cov_star into cov_feature
cov_feature[1:p0,1:p0] = cov_star
cov_feature[(p0+1):(2*p0),(p0+1):(2*p0)] = cov_star
cov_feature[(2*p0+1):(3*p0),(2*p0+1):(3*p0)] = cov_star
cov_feature[(3*p0+1):(4*p0),(3*p0+1):(4*p0)] = diag(p0)
####

X = mvrnorm(n = n, rep(0, p), cov_feature)
y = f_sim(X)
data = cbind(X,y)
data = data.frame(data)

data$PC1 = svd(X[,1:100])$u[,1] 
data$PC2 = svd(X[,101:200])$u[,1] 
data$PC3 = svd(X[,201:300])$u[,1]  
data$PC4 = svd(X[,301:400])$u[,1] 



# test data set
set.seed(101)

# No time structure
n_test = 100 # num of observations

X_test = mvrnorm(n = n_test, rep(0, p), cov_feature)
y_test = f_sim(X_test)
data_test = cbind(X_test,y_test)
data_test = data.frame(data_test)

# add PC
data_test$PC1 = svd(X_test[,1:100])$u[,1]  
data_test$PC2 = svd(X_test[,101:200])$u[,1] 
data_test$PC3 = svd(X_test[,201:300])$u[,1] 
data_test$PC4 = svd(X_test[,301:400])$u[,1] 


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




## Fuzzy Model Tree

 regress_var = paste("PC",c(1),sep="")
#regress_var = paste("V",1:100,sep="")
# regress_var = paste("V",c(1,2,3,301,302,303),sep="")
# regress_var = paste("V",c(1,2,3,70,52,185,285,266,301,302,303),sep="")

 split_var = paste("V",101:200,sep="") # For demo: CHANGE 101 TO 100 and alpha=0.5
split_var = paste("V",1:100,sep="")
# split_var = paste("V",c(1,2,3,70,52,185,285,266,301,302,303),sep="")

 Formula = as.formula(paste("y~",paste(regress_var,collapse = "+"), "|",
                         paste(split_var,collapse = "+")))

# If just use regular tree (not model tree,no regress_var): miss 302 and 303!
split_var = paste("V",1:400,sep="")
Formula = as.formula(paste("y~",paste(split_var,collapse="+")))

# Formula

# If specify regress_var, the following is the coeffiently of them
# If not (regular tree, no model), it returns the average of its leaves
# coef(mytree) 

# test
# tmp = get_split_names(mytree,data) # can be split_var
# paste(tmp,collapse = "+")


# Note: the parameters of lmtree in "..." will 
# directly go into mob_control like alpha and maxdepth, minsize
system.time({
  # the smaller the alpha, the deeper the tree. alpha =0.05 by default
  # mytree = lmtree(Formula,data = data,minsize = 10,alpha = 0.2,maxdepth=5) # demo
  mytree = lmtree(Formula,data = data,minsize = 10)
})

# mse on test set
mean((predict(mytree,newdata=data_test)-data_test$y_test)**2)

# the selected features
get_split_names(mytree,data)

plot(mytree)





#### Other Models for Comparison
# the following tree method use one of formulae below

# use all covariates 
var = paste("V",1:400,sep="")

# only use important ones, which means cheating
# var = paste("V",c(1,2,3,301,302,303),sep="")

# use part of important ones
# var = paste("V",c(2,301,303),sep="") # part of important regress

# perturbation: mixture of correct and incorrect
# var = paste("V",c(1,301,99,100,307,299),sep="")

Formula = as.formula(paste("y~",paste(var,collapse = "+")))


# ctree:  conditional inference framework.
system.time({c_tree <- ctree(
  Formula, 
  data = data)
})


# To see the importance of features in c_tree, run this instead
# system.time({c_tree <- train(
#   Formula, 
#   data = data,method="ctree")
# })
# varImp(c_tree)

plot(c_tree,main = "conditional inference")

# mean square error
mean((predict(c_tree,newdata=data_test)-data_test$y_test)**2)



# To see the importance of features in c_tree, run this instead
# system.time({c_tree <- train(
#   Formula, 
#   data = data,method="ctree")
# })
# varImp(c_tree)






# For Fuzzy Forest
# 
# The features I feed it cannot be too small (cannot be 6), or FF will raise an error
# When I use all the correct features and some other features (but not 1:400), sometimes it is worse than 1:400 case, which is strang

# Fuzzy Forest
# fuzzy forest
# Fuzzy Forest
# params is stored in xxx_control object
# power: belta (I choose it without much care)
WGCNA_params = WGCNA_control(power=3,TOMType="unsigned",minModuleSize=30,
                             numericLabels=TRUE,pamRespectsDendro=FALSE)

mtry_factor     = 1 # mtry = sqrt(p)*mtry_factor; mtry is the num in subspace method
drop_fraction   = 0.25 # drop xxx in each iteration of RFE-RF
number_selected = 8 # we want 10 out of all features
keep_fraction   = 0.05 # keep xxx for each module
min_ntree        = 500 # used for calculating ntree
ntree_factor    = 5 # used for calculating ntree
final_ntree     = 500 # RF in selecting step

screen_params = screen_control(drop_fraction = drop_fraction,
                               keep_fraction  = keep_fraction,
                               min_ntree      = min_ntree,
                               mtry_factor    = mtry_factor,
                               ntree_factor   = ntree_factor)

select_params = select_control(drop_fraction  = drop_fraction,
                               number_selected = number_selected,
                               min_ntree       = min_ntree,
                               mtry_factor     = mtry_factor,
                               ntree_factor    = ntree_factor)    

# var = paste("V",c(1:50,301,302,303),sep="")
# var = paste("V",1:400,sep="")
Formula = as.formula(paste("y~",paste(var,collapse = "+")))

# a "fuzzy_forest" object
system.time({
  set.seed(20)
  wff_fit = wff(Formula,data = data,WGCNA_params = WGCNA_params,
                screen_params = screen_params,
                select_params = select_params,
                final_ntree = final_ntree)
})

mean((predict(wff_fit,new_data=data_test[,1:400])-data_test$y_test)**2)
varImpPlot(wff_fit$final_rf,type=2,main="FF Variable Importance")




# Random Forest
system.time({
  set.seed(20)
  rf <- randomForest(data[,1:400],data$y)
})
mean((predict(rf,newdata=data_test[,1:400])-data_test$y_test)**2)
varImpPlot(rf,type=2,main="RF Variable Importance")
