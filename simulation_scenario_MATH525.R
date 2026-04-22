simulate_scenario <- function(N,n,p_true,p_noise,num_MC,log_file){
  
  # Memory preallocation 
  results <- data.frame(
    N = integer(num_MC),
    n = integer(num_MC),
    p_true = integer(num_MC),
    p_noise = integer(num_MC),
    mu_true = integer(num_MC),
    iteration = integer(num_MC),
    
    HT_SRSWOR = integer(num_MC),
    # TODO: Preallocate memory for your estimation methods
    ma_SRSWOR_OLS = numeric(num_MC), # OLS = ordinary least squares
    ma_SRSWOR_RF  = numeric(num_MC),
    ma_SRSWOR_GBM = numeric(num_MC),
    ma_SRSWOR_LASSO = numeric(num_MC),
    ma_SRSWOR_NET = numeric(num_MC),
    
    HT_Bern = integer(num_MC),
    # TODO: Preallocate memory for your estimation methods
    ma_Bern_OLS = numeric(num_MC),
    ma_Bern_RF  = numeric(num_MC),
    ma_Bern_GBM = numeric(num_MC),
    ma_Bern_LASSO = numeric(num_MC),
    ma_Bern_NET = numeric(num_MC),
    
    stringsAsFactors = FALSE
  )
  
  pop <- generate_population(N,p_true,p_noise)
  
  Y <- pop$Y
  X <- pop$X # !!! design matrix without intercept !!!
  
  mu_true <- mean(Y)
  
  for(i in 1:num_MC){
    
    # The following is a way to define unique seed values for each simulation of our algorithm
    set.seed(N+1000*n+100*p_true+10*p_noise+i)
    
    # Simle random sampling without replacement
    samp_SRSWOR <- sample(1:N,n,replace=FALSE)
    
    Y_SRSWOR <- Y[samp_SRSWOR]
    X_SRSWOR <- X[samp_SRSWOR,]
    
    # Bernoulli sampling
    indicators_Bern <- rbinom(N,size=1,prob=n/N)
    samp_Bern <- which(indicators_Bern == 1)
    
    Y_Bern <- Y[samp_Bern]
    X_Bern <- X[samp_Bern,]
    
    # Estimation
    est_SRSWOR <- estimate_methods(Y_SRSWOR,X_SRSWOR,Y,X)
    est_Bern <- estimate_methods(Y_Bern,X_Bern,Y,X)
    
    # Write progress to the log file
    log_data <- paste(N, n, p_true, p_noise, mu_true, i,  est_SRSWOR$HT,
                      # TODO:  Add your results in the same order as defined in "column_names" in "main_MATH525.R"
                      est_SRSWOR$ma_OLS, est_SRSWOR$ma_RF, est_SRSWOR$ma_GBM, est_SRSWOR$ma_LASSO,est_SRSWOR$ma_NET,
                      
                      est_Bern$HT,
                      # TODO:  Add your results in the same order as defined in "column_names" in "main_MATH525.R"
                      est_Bern$ma_OLS, est_Bern$ma_RF,est_Bern$ma_GBM,est_Bern$ma_LASSO,est_Bern$ma_NET,
                      sep = ",")
    write(log_data, file = log_file, append = TRUE)
    
    # Adding the results
    results[i, ] <- list(
      N = N,
      n = n,
      p_true = p_true, 
      p_noise = p_noise,
      mu_true = mu_true,
      iteration = i,
      HT_SRSWOR = est_SRSWOR$HT,
      # TODO:  Add your results
      ma_SRSWOR_OLS = est_SRSWOR$ma_OLS,
      ma_SRSWOR_RF = est_SRSWOR$ma_RF,
      ma_SRSWOR_GBM = est_SRSWOR$ma_GBM,
      ma_SRSWOR_LASSO = est_SRSWOR$ma_LASSO,
      ma_SRSWOR_NET = est_SRSWOR$ma_NET,
      
      HT_Bern = est_Bern$HT,
      ma_Bern_OLS = est_Bern$ma_OLS,
      ma_Bern_RF  = est_Bern$ma_RF,
      ma_Bern_GBM = est_Bern$ma_GBM,
      ma_Bern_LASSO = est_Bern$ma_LASSO,
      ma_Bern_NET = est_Bern$ma_NET
    )
  }  
  return(results)
}
