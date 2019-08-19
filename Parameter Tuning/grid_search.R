source("Longtree.R")
library("glmertree")


n <- 100
T <- 5
set.seed(100)
train <- sim_quad(n,T)
train$time2 = (train$time)^2

set.seed(102) # seed for validation set
test <- sim_quad(n,T)
test$time2 = (test$time)^2


# size of training dataset
n_values <- c(100,200,300,400,500,600,700,800,900,1000)
# Paramters to test
alpha <- c(0.01,0.05,0.10,0.2,0.3)
maxdepth_factor_screen <- c(0.02,0.04,0.08,0.2)
maxdepth_factor_select <- c(0.3,0.5,0.7)
fuzzy <- c(TRUE,FALSE)


df <- data.frame(n = c(NA),alpha = c(NA), maxdepth_factor_screen= c(NA),
                       maxdepth_factor_select=c(NA),
                       MSE = c(NA))   # create empty data frame 


 
 
# Formula = as.formula(paste("y~",paste(regress_var,collapse = "+"),
#                            "|",cluster,"|",
#                            paste(split_var,collapse = "+")))


fixed_regress = c("time","time2")
fixed_split = c("treatment")
cluster = "patient"
var_select = paste("V",1:400,sep="")


i <- 1
for(n in n_values){
  set.seed(100)
  train <- sim_quad(n,T)
  train$time2 = (train$time)^2

  for(a in alpha){
    for(screen in maxdepth_factor_screen){
       for(select in maxdepth_factor_select){
              for(f in fuzzy){
      
      print(screen)
      print(select)
      print(a)
      print(n)
      
      mytree = Longtree(train,fixed_regress=fixed_regress,fixed_split=fixed_split,
                        var_select=var_select,cluster=cluster,Fuzzy=f, alpha = a,
                        maxdepth_factor_screen = screen, maxdepth_factor_select = select)
  
      mse <- mean((predict(mytree,newdata=test)-test$y)**2)
      
      
      df[i,"n"] <- n
      df[i, "MSE"] <- mse
      df[i, "alpha"] <- a
      df[i, "maxdepth_factor_screen"] <- screen
      df[i, "maxdepth_factor_select"] <- select
      df[i, "Fuzzy"] <- f
      
      i <- i+1
        }
        }
      }
    }
    
    
    
    
  }


df$model <- "Longtree"


#write.csv(df, "Longtree1200.csv")




