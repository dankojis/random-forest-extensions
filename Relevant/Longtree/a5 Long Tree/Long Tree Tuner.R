# code from https://github.com/Yuancheng-Xu/UCLA-CSST/blob/master/Longtree/a1Longtree/Longtree%20Tuner.ipynb
source("Data Generator.r")
source("Longtree.r")

### training, validation and test set ###
set.seed(100)
n = 500
p = 400
imp_mod = c(1,4)
var_noise = 1
data = sim_time(n=n,p=p,imp_mod=imp_mod, var_noise=var_noise,a1=5,a2=-5)
data$time2 = (data$time)^2

# test set (used for testing performance using optimal parameters)
set.seed(101)
n_test = 100
data_test = sim_time(n=n_test,p=p,imp_mod=imp_mod, var_noise=var_noise,a1=5,a2=-5)
data_test$time2 = (data_test$time)^2

# validation set (used for tuning parameters)
set.seed(102)
n_valid = 100
data_valid = sim_time(n=n_valid,p=p,imp_mod=imp_mod, var_noise=var_noise,a1=5,a2=-5)
data_valid$time2 = (data_valid$time)^2
###



fixed_regress = c("time","time2")
fixed_split = c("treatment")
cluster = "patient"
var_select = paste("V",1:p,sep="")


# This is default parameters
# alpha_screen = 0.2; alpha_select = 0.2; alpha_predict = 0.05
# maxdepth_factor_select = 0.5; maxdepth_factor_screen = 0.04
# minsize_multiplier = 5
# Fuzzy=TRUE


# Tuning parameters on the validation set: alpha,maxdepth,Fuzzy
alpha_screen = 0.05; alpha_select = 0.2; alpha_predict = 0.2
maxdepth_factor_select = 1; maxdepth_factor_screen = 0.2
minsize_multiplier = 5
Fuzzy=TRUE

time = system.time({
  mytree = Longtree(data,fixed_regress=fixed_regress,fixed_split=fixed_split,
                    var_select=var_select,cluster=cluster,Fuzzy=Fuzzy,
                    maxdepth_factor_select =  maxdepth_factor_select,
                    maxdepth_factor_screen = maxdepth_factor_screen,
                    minsize_multiplier = minsize_multiplier, 
                    alpha_screen = alpha_screen,
                    alpha_select=alpha_select,alpha_predict=alpha_predict)
})
time # running time
mean((predict(mytree,newdata=data_valid,re.form=NA)-data_valid$y)**2)
# coef(mytree)
# 
# # performance of test set
# mse = mean((predict(mytree,newdata=data_test,re.form=NA)-data_test$y)**2)
# mse
# plot(mytree)



save_item = list(tree=mytree,mse=mse,alpha_screen=alpha_screen,
                 alpha_select = alpha_select, alpha_predict=alpha_predict,
                 maxdepth_factor_select=maxdepth_factor_select,
                 maxdepth_factor_screen=maxdepth_factor_screen,
                 minsize_multiplier = minsize_multiplier, 
                 final_selection = mytree$final_selection, Fuzzy=Fuzzy,time=time)
name = paste("Longtree_n",n,".rds",sep="")
saveRDS(save_item, name)




# PREDICTION STEP TUNING
# the final selection among all the chosen features 
var = paste("V",c(1,2,3,301,302,303),sep="")
regress_var = c("time","time2",var)
split_var = c("treatment",var)

Formula = as.formula(paste("y~",paste(regress_var,collapse = "+"),
                           "|",cluster,"|",
                           paste(split_var,collapse = "+")))
system.time({mytree = lmertree(Formula, data = data,alpha = 0.05,maxdepth=7,
                               minsize=20)}) 
mean((predict(mytree,newdata=data_valid,re.form=NA)-data_valid$y)**2)
mean((predict(mytree,newdata=data_test,re.form=NA)-data_test$y)**2)
# coef(mytree)