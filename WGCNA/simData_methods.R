library(MASS)


f_sim = function (X_data){
  y = (5*X_data[,1]+2*X_data[,2]+2*X_data[,3]+5*X_data[,2]*X_data[,3]
       +5*X_data[,301]+2*X_data[,302]+2*X_data[,303]+5*X_data[,302]*X_data[,303])
  return (y)
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



simAR_dif_cor = function(n,T,var_noise=1,alpha=0.8){
  p = 500
  p0 = 100
  data = matrix(0,nrow = n*T, ncol = p+1)
  
  #### covariance matrix between features:
  cov_feature = matrix(0,nrow = p, ncol = p)

  cov_star1 = matrix(0.2,nrow = p0,ncol = p0) # set correlation for first group of variables
  diag(cov_star1)=1
  # put cov_star into cov_feature
  cov_feature[1:p0,1:p0] = cov_star1
  
  cov_star2 = matrix(0.4,nrow = p0,ncol = p0)    # set correlation for 2nd group of variables
  diag(cov_star2)=1
  cov_feature[(p0+1):(2*p0),(p0+1):(2*p0)] = cov_star2
  
  cov_star3 = matrix(0.6,nrow = p0,ncol = p0) # set correlation for 3rd group of variables
  diag(cov_star3)=1
  cov_feature[(2*p0+1):(3*p0),(2*p0+1):(3*p0)] = cov_star3
  
  cov_star4 = matrix(0.8,nrow = p0,ncol = p0) # set correlation for 4th group of variables
  diag(cov_star4)=1
  cov_feature[(3*p0+1):(4*p0),(3*p0+1):(4*p0)] = cov_star4
    
  cov_feature[(4*p0+1):(5*p0),(4*p0+1):(5*p0)] = diag(p0)  # independent for last group
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
  
  return(data)
}


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





