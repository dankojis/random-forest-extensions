# source("TimeSeriesSimilarity.r") # used for similarity matrix 
# must run above code manually
source("WGCNA_TS.R")
source('simData_methods.R')


# generate data 
#data30 <-  simAR_dif_cor(n=100,T=5, group_cor = c(0.3,0.3,0.3,0.3,0.0), var_noise = 1,alpha = 0.8)
# data20 <-  simAR_dif_cor(n=100,T=5, group_cor = c(0.2,0.2,0.2,0.2,0.0), var_noise = 1,alpha = 0.8)
# data60 <-  simAR_dif_cor(n=100,T=5, group_cor = c(0.6,0.6,0.6,0.6,0.0), var_noise = 1,alpha = 0.8)
# data80 <-  simAR_dif_cor(n=100,T=5, group_cor = c(0.8,0.8,0.8,0.8,0.0), var_noise = 1,alpha = 0.8)
 dataMix1 <- simAR_dif_cor(n=100,T=5, group_cor = c(0.2,0.4,0.6,0.8,0.0), var_noise = 1,alpha = 0.8)
# dataMix2 <-  simAR_dif_cor(n=100,T=5, group_cor = c(0.3,0.3,0.6,0.9,0.0), var_noise = 1,alpha = 0.8)
# dataMix3 <-  simAR_dif_cor(n=100,T=5, group_cor = c(0.2,0.4,0.8,0.8,0.0), var_noise = 1,alpha = 0.8)


X = data[,-ncol(data)]
y = data[,ncol(data)]


# parameters of WGCNA_TS
# the time length for each units
len_time = rep(5,100)
# We like large modules, so we set the minimum module size relatively high
minModuleSize = 30



# set of paramter values to test for beta (power) and MEDissThres.
# *NOTE* : This is would be a biased way to select the best parameters (we are cherry picking
#          the best performers, no validation set). However, not a major concern for the preliminary application. 
beta <- c(2,4,6,8,10,12,14,16)
thresh <- c(0.10,0.15,0.20,0.25,0.30,0.35)

# create empty dataset to fill with results
results <- data.frame(Type = c(NA), Beta = c(NA), Threshold = c(NA), Zero = c(NA), One = c(NA),   # create empty data frame to
                      Two = c(NA), Three = c(NA), Four = c(NA))                        # save simulation results in



### type = "cor"

# iterate over different softPower and MEDissThres values
for(i in beta){
  for(j in thresh){
    my_color = WGCNA_TS(X,len_time,softPower=i,minModuleSize,MEDissThres = j,type = "cor")
    # print(i)
    # print(j)
    # print(table(my_color))
    results <- rbind(results, c("cor",i,j, sum(my_color==0),sum(my_color==1),sum(my_color==2),  # store group assignments
                                sum(my_color==3),sum(my_color==4)))
    
  }
}

# Beta value of 6 peforms the best. A reasonable choice for MEDissThres could be anything from 0.2-0.3
# This combination sorts them perfectly. 




### type = "L2" , Euclidean

for(i in beta){
  for(j in thresh){
    my_color = WGCNA_TS(X,len_time,softPower=i,minModuleSize,MEDissThres = j,type = "L2")
    results <- rbind(results, c("L2",i,j, sum(my_color==0),sum(my_color==1),sum(my_color==2),  # store group assignments
                                sum(my_color==3),sum(my_color==4)))
    
  }
}


### Regular WGCNA, should be same as blockwiseMudules
for(i in beta){
  for(j in thresh){
    my_color = WGCNA_regular(X,len_time,softPower=i,minModuleSize,MEDissThres=j)
    results <- rbind(results, c("Regular",i,j, sum(my_color==0),sum(my_color==1),sum(my_color==2),  # store group assignments
                                sum(my_color==3),sum(my_color==4)))
    
  }
}


# regular their function in WGCNA
for(i in beta){
  for(j in thresh){
    net = blockwiseModules(X, power = i,TOMType = "unsigned", 
                                minModuleSize = 30,reassignThreshold = 0, 
                                mergeCutHeight = j,numericLabels = TRUE, 
                                pamRespectsDendro = FALSE,saveTOMs = TRUE,verbose = 3)
    results <- rbind(results, c("blockwiseModules",i,j, sum(net$colors==0),sum(net$colors==1),sum(net$colors==2),  # store group assignments
                                sum(net$colors==3),sum(net$colors==4)))
    
  }
}



### fastDTW

beta <- c(4,6,8)
thresh <- c(0.20,0.25,0.30)

for(i in beta){
  for(j in thresh){
    my_color =WGCNA_TS(X,len_time,softPower=i,minModuleSize,MEDissThres = j,
                       type = "fastDTW")
    print(i)
    print(j)
    print(table(my_color))
    results <- rbind(results, c("fastDTW",i,j, sum(my_color==0),sum(my_color==1),sum(my_color==2),  # store group assignments
                                sum(my_color==3),sum(my_color==4)))
    
  }
}




save(results, file='data60.Rda')






