source("TimeSeriesSimilarity.r")
source("WGCNA_TS.R")
source('simData_methods.R')
library("WGCNA")


# parameters of WGCNA_TS

# type used for computing time series distance
type = "fastDTW" # computed within each time series and then average
# the time length for each units
len_time = rep(5,100)
# beta for WGCNA 
softPower = 4
# We like large modules, so we set the minimum module size relatively high
minModuleSize = 30
# When merge similar modules using eigengenes (After we get modules for dissTom)
# We choose a height cut of MEDissThres (= 0.25 for example) corresponding to 
# correlation of 1-MEDissThres (0.75) to merge
MEDissThres = 0.25



# generate data
# n = 100
# T = 5
# 
# # CS structure on X: data5
# # data = sim_4(n,T,cor_feature=0.8,var_noise=1,beta=0.8)
# 
# # AR struture on X: data4
# data = sim_3(n,T,cor_feature=0.3,var_noise=1,alpha=0.8)
# 
# 
# # if the data is in a csv, do the following instead
# # data <- read.csv(file="data4.csv", header=TRUE, sep=",")
# # data = data[,-1] # delete the X column (name of rows)
# 
data <-  simAR_dif_cor(n=100,T=5, group_cor = c(0.8,0.8,0.8,0.8,0.0), var_noise = 1,alpha = 0.8)
 X = data[,-ncol(data)]
 y = data[,ncol(data)]




# My function: fastDTW
system.time({
  my_color = WGCNA_TS(X,len_time,softPower=8,minModuleSize,MEDissThres = MEDissThres,
                      type = "fastDTW")
})
table(my_color)
#my_color



# My function: cor
my_color = WGCNA_TS(X,len_time,softPower=6,minModuleSize,MEDissThres,type = "cor")
table(my_color)
my_color



# My function: euclidean
my_color = WGCNA_TS(X,len_time,softPower=6,minModuleSize,MEDissThres,type = "L2")
table(my_color)
my_color

# regular WGCNA, should be same as blockwiseMudules
color_reg = WGCNA_regular(X,len_time,softPower=4,minModuleSize,MEDissThres)
table(color_reg)
color_reg




# regular their function in WGCNA
net = blockwiseModules(X, power = 4,TOMType = "unsigned", 
                       minModuleSize = 30,reassignThreshold = 0, 
                       mergeCutHeight = 0.25,numericLabels = TRUE, 
                       pamRespectsDendro = FALSE,saveTOMs = TRUE,verbose = 3)
colors = net$colors
table(net$colors)
colors
