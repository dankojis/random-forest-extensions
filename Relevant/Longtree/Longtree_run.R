source("Longtree.R")

fixed_regress = c("time","time2")
fixed_split = c("treatment")
cluster = "patient"
var_select = paste("V",1:400,sep="")



# training data
n <- 300 # number of patients
T <-  5 # number of observations per patients

set.seed(100)

data <- sim_quad(n,T)
# add time_squared
data$time2 = (data$time)^2

# testing data
n_test <- 100 # number of patients
T <-  5 # number of observations per patients
set.seed(101)
data_test <- sim_quad(n_test,T)
# data_test <- sim_quad(n_test, T)
data_test$time2 = (data_test$time)^2




# Fuzzy=TRUE 
# alpha = 0.2, maxdepth_factor_select = 0.5 (all default)
system.time({
  mytree = Longtree(data,fixed_regress=fixed_regress,fixed_split=fixed_split,
                    var_select=var_select,cluster=cluster,Fuzzy=TRUE)
})
mean((predict(mytree,newdata=data_test)-data_test$y)**2)




# Fuzzy=False 
# alpha = 0.1, maxdepth_factor_select = 0.5
system.time({
  mytree = Longtree(data,fixed_regress=fixed_regress,fixed_split=fixed_split,
                    var_select=var_select,alpha=0.1,cluster=cluster,Fuzzy=FALSE)
})
mean((predict(mytree,newdata=data_test)-data_test$y)**2)
