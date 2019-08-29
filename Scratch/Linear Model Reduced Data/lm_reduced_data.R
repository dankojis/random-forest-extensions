source('simData_methods.R')
library(tidyverse)
library(dplyr)
library(magrittr)



n <- 100 # number of patients
T <-  5 # number of observations per patients

data <- as.data.frame(sim_3_RE(n, T, .8)) # generate data with desired structure

# X = data[,-ncol(data)] # remove y value from for X matrix
# y = data[,ncol(data)] # assign target variable


# create time variable, rename target variable
data$time <- rep(1:T,n )
colnames(data)[401] <- "y"
# data %<>% dplyr::select(y, time, everything()) # this line will affect indexing


## Create vectors of variable names for future data frame
#orig_var <- colnames(data)[1:(ncol(data)-2)]
slope_var <- paste0("Slope",seq(from=1,to=400))
se_var <- paste0("SE", seq(1,400))
residual_var <- paste0("Residual",seq(1,400))


# build regression model predicting response, y, from time for each patient. Replace y values with the slope values
# of these regression models. Each patient should have all same y values; we will delete all but one 
# observation/patient later
a <- 1; b <- T
for(i in 1:(nrow(data)/T)){
  lm <- lm(y[a:b]~time[a:b], data)
  
  data$y[a:b] <- lm$coefficients[2]

  a <- a + T; b <- b +T
}
# create categorical represenation of y. If slope is positive, assign 1. If negative, assign 0. 
data$y_cat <- ifelse(data$y < 0, 0, 1)



# For each patient, fit a separate linear regression model predicting each variable from time. That is, there will
# be patients*variables number of regression models. Then, store the slope, std error, and residual values from each
# model
z <- 1
for(var in colnames(data)[1:(ncol(data)-2)]){

      a <- 1; b <- T
      
      for(i in 1:(nrow(data)/T)){
        lm <- lm(data[[var]][a:b]~time[a:b], data)
        
        
        data[[ slope_var[z] ]][a:b] <- lm$coefficients[2]
        data[[ se_var[z] ]][a:b] <- coef(summary(lm))[2, 2]
        data[[ residual_var[z] ]][a:b] <- lm$residuals
        
        a <- a + T; b <- b +T
        
      }
      z <- z+1
}

data <- data[,-c(1:400)] # remove the original X variables, only keeping the regression model information


# inside loop: finds the squared residuals of each patient and sums them up. Assigns this value to first time observation
# of patient. The other time ovbservations (after 1) will then be deleted, so we are left with just the 
# sum of squares for each patient
# outer loop: repeat this process for every residual variabe (that was obtain from regressions on different X variables)
for(var in residual_var){

  data[[var]] <- data[[var]]^2 # square the residuals
  
  a <- 1; b <- T
 
  for(i in 1:n){
    
    data[[var]][a] <- sum(data[[var]][a:b]) 
    
    a <- a+T; b <- b+T
    
  }
  
}

a <- seq(from=1,to=n*T, by=T) # index for first time observation of each patient
data <- data[a,] # keep only one observation (where the sum was stored) per patient
 






# write.csv(data, file="lm_reduced_data.csv")     

# # Useful for checking dataset
# lm <- lm(V1[221:225]~time[221:225], data)
# summary(lm)
# plot(data$time[221:225], data$V1[221:225])
# abline(lm)






