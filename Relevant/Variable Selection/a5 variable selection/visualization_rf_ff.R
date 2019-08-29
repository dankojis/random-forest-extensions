# Feature selection visualization of fuzzy forest and random forest
# data was simulated from sim_time() function with a1=5, a2=-5. 50 simulations were run for each sample size
#
# 1). Code to create one bar plot. Csv file can/should be changed for desired data. Graph will plot the true
# response variables (i.e. V1,V2,V3,V301,V302,V303) and any other variables selected at frequency > 20%
#
# 2). Code for a 4-panel figures. First figure is for Fuzzy forest at sizes n=100,200,300,400. Second figure is
# for random forest at sizes n=100,200,300,400


# 1). 
# Code to create one plot 
###############################################################################
data <- read.csv("ff_n100.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet

imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

freq <- as.numeric(data[51,var_both])


names <- c(var_both[1:(length(var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis

barplot(freq~var_both,
        main="Fuzzy Forest: 50 simulations (n=100)", ylab = 'Frequency of Selection', 
        xlab = 'Variable', las=2, names.arg = names)

###############################################################################


# 2). 
### Code for multiple plots
#  fuzzy forest n = 100
data <- read.csv("ff_n100.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet

imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
ff100_var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

ff100_freq <- as.numeric(data[51,ff100_var_both])
ff100_names <- c(var_both[1:(length(ff100_var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis


# fuzzy forest n = 200
data <- read.csv("ff_n200.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet


imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
ff200_var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

ff200_freq <- as.numeric(data[51,ff200_var_both])
ff200_names <- c(var_both[1:(length(ff200_var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis


# fuzzy forest n = 300
data <- read.csv("ff_n300.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet


imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
ff300_var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

ff300_freq <- as.numeric(data[51,ff300_var_both])
ff300_names <- c(var_both[1:(length(ff300_var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis




# fuzzy forest n = 400
data <- read.csv("ff_n400.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet


imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
ff400_var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

ff400_freq <- as.numeric(data[51,ff400_var_both])
ff400_names <- c(var_both[1:(length(ff400_var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis

#########################

#  Random forest n = 100
data <- read.csv("rf_n100.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet


imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
rf100_var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

rf100_freq <- as.numeric(data[51,rf100_var_both])
rf100_names <- c(var_both[1:(length(rf100_var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis



# Random forest n = 200
data <- read.csv("rf_n200.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet


imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
rf200_var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

rf200_freq <- as.numeric(data[51,rf200_var_both])
rf200_names <- c(var_both[1:(length(rf200_var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis


# fuzzy forest n = 300
data <- read.csv("rf_n300.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet


imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
rf300_var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

rf300_freq <- as.numeric(data[51,rf300_var_both])
rf300_names <- c(var_both[1:(length(rf300_var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis


# fuzzy forest n = 400
data <- read.csv("rf_n400.csv") # read in desired data
data <- data[,-1] # remove 1st id row
X <- data[,1:403] # select variables V1:V400, time, time2, and treatmet

imp_var = c(1,2,3,301,302,303,401,402,403) # variables of interest
selected_var <- c(which(X[51,]>0.2)) #  variables selected at frequency above 20%
rf400_var_both <- sort(unique(c(imp_var,selected_var))) # variables selcted at above 20% and original variables of interest

rf400_freq <- as.numeric(data[51,rf400_var_both])
rf400_names <- c(var_both[1:(length(rf400_var_both)-3)], "time", "time2", "treatment") # names for x each bar on x axis







# Fuzzy Forest Panel Plot 
par(mfrow = c(2,2))
barplot(ff100_freq~ff100_var_both,
        main="Fuzzy Forest (n=100)", ylab = 'Frequency of Selection', xlab = 'Variable',
        las=2, names.arg = ff100_names)
barplot(ff200_freq~ff200_var_both,
        main="Fuzzy Forest (n=200)", ylab = 'Frequency of Selection', xlab = 'Variable',
        las=2, names.arg = ff200_names)
barplot(ff300_freq~ff300_var_both,
        main="Fuzzy Forest (n=300)", ylab = 'Frequency of Selection', xlab = 'Variable', 
        las=2, names.arg = ff300_names)
barplot(ff400_freq~ff400_var_both,
        main="Fuzzy Forest (n=400)", ylab = 'Frequency of Selection', xlab = 'Variable',
        las=2, names.arg = ff400_names)

dev.copy(jpeg,filename="Fuzzy Forest.jpg");
dev.off ();


# Random Forest Panel Plot
par(mfrow = c(2,2))
barplot(rf100_freq~rf100_var_both,
        main="Random Forest (n=100)", ylab = 'Frequency of Selection', xlab = 'Variable', 
        las=2, names.arg = rf100_names)
barplot(rf200_freq~rf200_var_both,
        main="Random Forest (n=200)", ylab = 'Frequency of Selection', xlab = 'Variable', 
        las=2, names.arg = rf200_names)
barplot(rf300_freq~rf300_var_both,
        main="Random Forest (n=300)", ylab = 'Frequency of Selection', xlab = 'Variable',
        las=2, names.arg = rf300_names)
barplot(rf400_freq~rf400_var_both,
        main="Random Forest (n=400)", ylab = 'Frequency of Selection', xlab = 'Variable', 
        las=2, names.arg = rf400_names)

dev.copy(jpeg,filename="Random Forest.jpg");
dev.off ()
