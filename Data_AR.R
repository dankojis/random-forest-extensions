# Generate X for one sample; X is T*p
# p features and T oberservations
# p= 4*p0 : the first 3 p0 features are 3 modules, independent bewteen modules and
# correlated (cov=0.8) within each. The last module is independent within and with the 
# first three
# var_noise: noise level (used in generating label y)

# return a T*(p+1) matrix, the last row is the label y
Data_AR = function(T,var_noise){
  
  p0 = 100
  p = 4*p0
  
  #### covariance matrix between features: it is either 0 (independent) or 0.8 ####
  cov_feature = matrix(0,nrow = p, ncol = p)
  # cov within the first three modules
  cov_star = matrix(0.8,nrow = p0,ncol = p0)
  diag(cov_star)=1
  # put cov_star into cov_feature
  cov_feature[1:p0,1:p0] = cov_star
  cov_feature[(p0+1):(2*p0),(p0+1):(2*p0)] = cov_star
  cov_feature[(2*p0+1):(3*p0),(2*p0+1):(3*p0)] = cov_star
  cov_feature[(3*p0+1):(4*p0),(3*p0+1):(4*p0)] = diag(p0)
  ####
  
  #### X_data ####
  # T*p Data matrix, pre-allocate memory
  X_data = matrix(0,nrow = T,ncol = p)
  # the first row
  X_data[1,] = mvrnorm(n = 1, rep(0, p), cov_feature)
  # the next row depends on the previous one
  for (i in 2:T){
    X_data[i,] = 0.8*X_data[i-1,]+0.6*mvrnorm(n = 1, rep(0, p), cov_feature)
  }
  ###
  
  ### create labels y ###
  # create a n vector for labels
  y = matrix(data=0,nrow = T)
  # build y according to our model
  y = (5*X_data[,1]+2*X_data[,2]+2*X_data[,3]+5*X_data[,2]*X_data[,3]
       +5*X_data[,301]+2*X_data[,302]+2*X_data[,303]+5*X_data[,302]*X_data[,303]
       +mvrnorm(n = 1, rep(0, T), diag(x=var_noise,T)))    # Adress: incoporoate random effect and individual error
  ###
  
  # return a T*(p+1) matrix
  return (cbind(X_data,y))
}

