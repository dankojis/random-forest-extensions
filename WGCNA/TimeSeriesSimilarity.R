library("fuzzyforest")
source("WGCNA_TS.r")
library("WGCNA")
library("randomForest")


#data
data <- read.csv(file="data4.csv", header=TRUE, sep=",")

data <- data_x

data = data[,-1] # delete the X column (name of rows)
X = data[,-ncol(data)]
y = data[,ncol(data)]

################## parameters for FF (ff) ################
# params is stored in xxx_control object
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

############# parameters for WGCNA_TS ########
# type used for computing time series distance
type = "cor"
# the time length for each units
len_time = rep(5,100)
# beta
softPower = 6
# We like large modules, so we set the minimum module size relatively high
minModuleSize = 30
# When merge similar modules using eigengenes (After we get modules for dissTom)
# We choose a height cut of MEDissThres (= 0.25 for example) corresponding to 
# correlation of 1-MEDissThres (0.75) to merge
MEDissThres = 0.25
#################################################

# compute module membership by WGCNA_TS
# X = X[,1:32] ###### test!!!
module_membership = WGCNA_TS(X,len_time,softPower,minModuleSize,MEDissThres,type)
# module_membership #####test!!!



table(module_membership)




module_membership




ff_fit = ff(X, y,module_membership=module_membership,
            screen_params = screen_control(min_ntree = 500),
            select_params = select_control(min_ntree = 500), final_ntree = 5000,
            num_processors = 1)


print(ff_fit)



modplot(ff_fit)
varImpPlot(ff_fit$final_rf,type=2,main="Variable Importance Plot")
