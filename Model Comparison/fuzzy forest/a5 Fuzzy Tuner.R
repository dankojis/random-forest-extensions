source("./Data Generator.r")
library(fuzzyforest)
library(WGCNA)


### training and test set ###
set.seed(100)
n = 500 # change n here each time
p = 400
imp_mod = c(1,4)
var_noise = 1
data = sim_time(n=n,p=p,imp_mod=imp_mod, var_noise=var_noise)
data$time2 = (data$time)^2

# test set
set.seed(101)
n_test = 100
data_test = sim_time(n=n_test,p=p,imp_mod=imp_mod, var_noise=var_noise)
data_test$time2 = (data_test$time)^2




n_run = 50 # the number of times RF will run on the data set
n_top = 12 # the top n_top variables will be selected
# create empty data frame to save simulation results in
result_ff = matrix(0,n_run+1,p+4) # the last row is for average
result_ff = data.frame(result_ff)
names(result_ff)[p+1] = "time"
names(result_ff)[p+2] = "time2"
names(result_ff)[p+3] = "treatment"
names(result_ff)[p+4] = "error"
names(result_ff)[1:p] = paste("V",1:p,sep="")




system.time({
  for(Repeat in 1:n_run){
    set.seed(Repeat+32) # change seed each loop
    
    # since treatment is categorical, we cannot include it in WGCNA
    data_WGCNA = data[,1:p] # only the covariates
    
    net = blockwiseModules(data_WGCNA, power = 6,TOMType = "unsigned", 
                           minModuleSize = 30,reassignThreshold = 0, 
                           mergeCutHeight = 0.25,numericLabels = FALSE, 
                           pamRespectsDendro = FALSE,verbose = 0)
    
    var = c(paste("V",1:p,sep=""),"time","time2","treatment")
    Formula = as.formula(paste("y~",paste(var,collapse = "+")))
    
    net$colors[["time"]] = "grey"
    net$colors[["time2"]] = "grey"
    net$colors[["treatment"]] = "grey"
    
    ff_fit = ff(Formula,data = data,module_membership=net$colors,
                screen_params = screen_control(min_ntree = 500,keep_fraction = 0.06),
                select_params = select_control(min_ntree = 500,number_selected = n_top), 
                final_ntree = 1000, num_processors = 1)        
    
    top_variables = ff_fit$feature_list[,1]
    error = mean((predict(ff_fit,new_data=data_test)-data_test$y)**2)
    
    # If variable was selected as important, indicate with 1 (otherwise 0)
    for (i in 1:p){
      result_ff[Repeat,i] <- as.numeric(paste("V",i,sep="") %in% top_variables)
    }
    result_ff[Repeat,p+1] <- as.numeric("time" %in% top_variables)
    result_ff[Repeat,p+2] <- as.numeric("time2" %in% top_variables)
    result_ff[Repeat,p+3] <- as.numeric("treatment" %in% top_variables)
    result_ff[Repeat,p+4] <- error
    
    # show the progress
    flush.console()
    cat(Repeat,"\n")
  }
})
result_ff[n_run+1,] = colMeans(result_ff[1:n_run,])
name = paste("ff_n",n,".csv",sep="")
write.csv(result_ff,file = name)



