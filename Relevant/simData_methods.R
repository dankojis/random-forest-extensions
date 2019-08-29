library(MASS)


f_sim = function (X_data){
  y = (5*X_data[,1]+2*X_data[,2]+2*X_data[,3]+5*X_data[,2]*X_data[,3]
       +5*X_data[,301]+2*X_data[,302]+2*X_data[,303]+5*X_data[,302]*X_data[,303])
  return (y)
}




# f_sim_cat = function (X_data){
#   eqn <- (5*X_data[,1]+2*X_data[,2]+2*X_data[,3]+5*X_data[,2]*X_data[,3]
#           +5*X_data[,301]+2*X_data[,302]+2*X_data[,303]+5*X_data[,302]*X_data[,303])
#   
#   p <- exp(eqn)/(1+exp(eqn))
#   
#   y <- ifelse(p < .5, 0, 1)
#   
#   return (y)
# }



# CS time structured on y and grouped features
# method found: https://github.com/Yuancheng-Xu/UCLA-CSST/blob/master/FF_timeSeries/SimData.r
sim_2 = function(n,T,cor_feature=0.8,var_noise=1,cor_noise=0.8){
  p = 400
  p0 = 100
  data = matrix(0,nrow = n*T, ncol = p+1)
  
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
  
  #### covariance matrix of noise #####
  # the covariance matrix of noise within a unit
  cov_noise_star = matrix(cor_noise,nrow = T, ncol = T)
  diag(cov_noise_star) = var_noise
  # the overal cov matrix has diagonal block matrix as cov_noise_star
  cov_noise = matrix(0,nrow = n*T, ncol = n*T)
  for (i in 1:n){
    cov_noise[(1+(i-1)*T):(i*T),(1+(i-1)*T):(i*T)] = cov_noise_star
  }
  ###
  
  # Create X matrix
  data[1:(n*T),1:p] = mvrnorm(n=n*T,rep(0,p),cov_feature)
  
  # create label y
  data[1:(n*T),p+1] = (f_sim(data[1:(n*T),1:p]) 
                       + mvrnorm(n=1,rep(0,n*T),cov_noise))
  
  return(data)
  
}


# Following pdf file: simulation_AR
# x(i+1) = alpha*x(i) + (1-alpha^2)^0.5*std_normal; in the pdf, alpha = 0.8
# method found: https://github.com/Yuancheng-Xu/UCLA-CSST/blob/master/FF_timeSeries/SimData.r
sim_3 = function(n,T,cor_feature=0.8,var_noise=1,alpha=0.8){
  p = 400
  p0 = 100
  data = matrix(0,nrow = n*T, ncol = p+1)
  
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
  
  # create x matrix
  tmp = (1-alpha**2)**0.5
  for (i in 1:n){
    data[1+(i-1)*T,1:p] = mvrnorm(n = 1, rep(0, p), cov_feature)
    for (j in 2:T){
      data[j+(i-1)*T,1:p] = (alpha*data[j-1+(i-1)*T,1:p]+
                               tmp*mvrnorm(n = 1, rep(0, p), cov_feature))
    }
  }
  
  # create y 
  data[1:(n*T),p+1] = ( f_sim(data[1:(n*T),1:p])+ 
                          mvrnorm(n = 1, rep(0,n*T), diag(n*T)) )
  
  return (data)
}





# Following pdf file: simulation_AR
# x(i+1) = alpha*x(i) + (1-alpha^2)^0.5*std_normal; in the pdf, alpha = 0.8
sim_3_RE = function(n,T,cor_feature=0.8,var_noise=1,alpha=0.8){
  p = 400
  p0 = 100
  data = matrix(0,nrow = n*T, ncol = p+1)
  
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
  
  # create x matrix
  tmp = (1-alpha**2)**0.5
  for (i in 1:n){
    data[1+(i-1)*T,1:p] = mvrnorm(n = 1, rep(0, p), cov_feature)
    for (j in 2:T){
      data[j+(i-1)*T,1:p] = (alpha*data[j-1+(i-1)*T,1:p]+
                               tmp*mvrnorm(n = 1, rep(0, p), cov_feature))
    }
  }
  
  # incoroporate random effects for each patient
  randomeffect <- rep(rnorm(n,0,5),each=T)
  
  
  # create y 
  data[1:(n*T),p+1] = ( f_sim(data[1:(n*T),1:p])+ 
                          mvrnorm(n = 1, rep(0,n*T), diag(n*T)) + randomeffect )
  
  return (as.data.frame(data))
}



# Builds off of sim_2 function. Creates a group1, group2, and a time variable. The target variable, y,
# is now also dependent on a quadratic function depenent on time. The equation takes the form:
#    y = f(x) + (a1x^2 +b1x +c1)*group1 + (a2x^2 +b2x +c2)*group2 + error
# where the group variables are either 0 or 1 (1 indicating that the patient is in that group, 0 otherwise).
sim_quad = function(n,T,cor_feature=0.8,var_noise=1,cor_noise=0.8, a1=5,a2=-5,b1=1,b2=1,c1=-10, c2 =10){
  p = 400
  p0 = 100
  data = matrix(0,nrow = n*T, ncol = p)

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

  #### covariance matrix of noise #####
  # the covariance matrix of noise within a unit
  cov_noise_star = matrix(cor_noise,nrow = T, ncol = T)
  diag(cov_noise_star) = var_noise
  # the overal cov matrix has diagonal block matrix as cov_noise_star
  cov_noise = matrix(0,nrow = n*T, ncol = n*T)
  for (i in 1:n){
    cov_noise[(1+(i-1)*T):(i*T),(1+(i-1)*T):(i*T)] = cov_noise_star
  }
  ###

  # Create X matrix
  data[1:(n*T),1:p] = mvrnorm(n=n*T,rep(0,p),cov_feature)


  data <- as.data.frame(data)
  data$time <- rep(1:T, n)
  data$group1[1:(n*T/2)] <- 1; data$group1[((n*T/2)+1):(n*T)] <- 0
  data$group2[1:(n*T/2)] <- 0; data$group2[((n*T/2)+1):(n*T)] <- 1


  # create label y
  data$y = ( f_sim(data[1:(n*T),1:p])+

                          a1*(data$time - median(1:T))^2*data$group1 + a2*(data$time - median(1:T))^2*data$group2 +
                          b1*data$time*data$group1 + b2*data$time*data$group2 + c1*data$group1 + c2*data$group2 +

                          mvrnorm(n = 1, rep(0,n*T), diag(n*T)) )


  return(data)

}

  # plot(data$time[251:500],data$y[251:500])
  # plot(data$time[1:250],data$y[1:250])
  # plot(data$time,data$y)



# group_cor - input a vector of correlation values. The first value will be the correlation between the first
# 100 variables. The 2nd value will be the correlation between variables 101-200. And so on. 

# example call: data <- simAR_dif_cor(n=100,T=5, group_cor = c(0.2,0.4,0.6,0.8,0.0), var_noise = 1,alpha = 0.8)

simAR_dif_cor = function(n,T, group_cor=c(0.0,0.2,0.4,0.6,0.8),var_noise=1,alpha=0.8){
  nGroup = length(group_cor) 
  p = nGroup*100
  p0 = 100
  data = matrix(0,nrow = n*T, ncol = p+1)
  
  #### covariance matrix between features:
  cov_feature = matrix(0,nrow = p, ncol = p)
  
  for(i in 1:nGroup){
    cov_star <- matrix( group_cor[i], nrow=p0, ncol=p0)
    diag(cov_star) <- 1
    # put cov_star into cov_feature
    cov_feature[((i-1)*p0+1):(i*p0) , ((i-1)*p0+1):(i*p0)] = cov_star
    
  }
  
  
  # create x matrix
  tmp = (1-alpha**2)**0.5
  for (i in 1:n){
    data[1+(i-1)*T,1:p] = mvrnorm(n = 1, rep(0, p), cov_feature)
    for (j in 2:T){
      data[j+(i-1)*T,1:p] = (alpha*data[j-1+(i-1)*T,1:p]+
                               tmp*mvrnorm(n = 1, rep(0, p), cov_feature))
    }
  }
  
  # incoroporate random effects for each patient
  randomeffect <- rep(rnorm(n,0,5),each=T)
  
  
  # create y 
  data[1:(n*T),p+1] = ( f_sim(data[1:(n*T),1:p])+ 
                          mvrnorm(n = 1, rep(0,n*T), diag(n*T)) + randomeffect )
  
  return(as.data.frame(data))
}





