generate_population <- function(N,p_true,p_noise){
  
  # Set seed based on N,p_true,p_noise for reproducibility
  set.seed(N+p_true+p_noise)
  
  X1 <- matrix(rnorm(N*p_true),N,p_true) # design matrix of true predictors
  X2 <- matrix(rt(N * p_noise, df = 3),N,p_noise) # design matrix of noise predictors
  
  beta <- runif(p_true+1,-1,1) # +intercept
  
  eps <- rnorm(N)
  
  Y <- cbind(1, X1) %*% beta + eps # with intercept
  
  list(
    X=cbind(X1,X2),
    Y=as.vector(Y),
    beta=beta
  )
}



# TODO: Please control if the formula below is correct; JC check: Yes, i think this is correct.
model_assisted_estimator <- function(Y_pop_pred, Y_s_pred, HT_estim, incl_prob_vec, N){
  mu_bar <- mean(Y_pop_pred) + HT_estim - sum(Y_s_pred/incl_prob_vec)/N
  return(mu_bar)
}



estimate_methods <- function(Y_s,X_s,Y_pop,X_pop){
  
  N <- length(Y_pop)
  n <- length(Y_s)
  X_s_df   <- as.data.frame(X_s)
  X_pop_df <- as.data.frame(X_pop)
  ### Horvitz-Thompson mean (SRSWOR case)
  
  HT <- mean(Y_s)
  incl_prob_vec <- rep(n/N, n)
  
  ### ---------------------------
  ### 1) OLS
  ### ---------------------------
  
  # Check if OLS can be computed
  if (ncol(X_s) <= n) {
    # Use lm.fit for faster computation (no formula parsing like in lm(y~...))
    # lm.fit does not automatically add an intercept, so we include a column of ones
    fit_OLS <- lm.fit(x = cbind(1,X_s), y = Y_s)
    coef_OLS <- fit_OLS$coefficients
    names(coef_OLS) <- NULL   # remove names of the coefficients
    Y_pop_OLS <- cbind(1,X_pop) %*% coef_OLS
    Y_s_OLS <- cbind(1,X_s) %*% coef_OLS
    
    ma_OLS <- model_assisted_estimator(Y_pop_OLS, Y_s_OLS, HT, incl_prob_vec, N)
  } else{
    ma_OLS <- NA
  }
  
  ### ---------------------------
  ### 2) Random Forest
  ### ---------------------------
  fit_RF <- randomForest::randomForest(
    x = X_s_df,
    y = Y_s
  )
  
  Y_pop_RF <- predict(fit_RF, newdata = X_pop_df)
  Y_s_RF   <- predict(fit_RF, newdata = X_s_df)
  
  ma_RF <- model_assisted_estimator(Y_pop_pred = Y_pop_RF, Y_s_pred = Y_s_RF,
                                    HT_estim = HT, incl_prob_vec = incl_prob_vec, N = N
  )
  
  ### ---------------------------
  ### 3) Boosting (GBM)
  ### ---------------------------
  dat_gbm <- data.frame(Y = Y_s, X_s_df)
  
  fit_GBM <- gbm::gbm(
    formula = Y ~ .,
    data = dat_gbm,
    distribution = "gaussian",
    n.trees = 500,
    interaction.depth = 3,
    shrinkage = 0.05,
    n.minobsinnode = 3,
    bag.fraction = 0.8,
    train.fraction = 1.0,
    verbose = FALSE
  )
  
  best_iter <- fit_GBM$n.trees
  
  Y_pop_GBM <- predict(fit_GBM, newdata = X_pop_df, n.trees = best_iter)
  Y_s_GBM   <- predict(fit_GBM, newdata = X_s_df,   n.trees = best_iter)
  
  ma_GBM <- model_assisted_estimator(Y_pop_pred = Y_pop_GBM, Y_s_pred = Y_s_GBM,
                                     HT_estim = HT,incl_prob_vec = incl_prob_vec, N = N
  )
  
  
  ### ---------------------------
  ### 4) LASSO
  ### ---------------------------
  fit_LASSO <- glmnet::cv.glmnet(
    x = as.matrix(X_s),
    y = Y_s,
    family = "gaussian",
    alpha = 1,
    nfolds = 5,
    standardize = TRUE
  )
  
  Y_pop_LASSO <- as.numeric(predict(fit_LASSO, newx = as.matrix(X_pop), s = "lambda.min"))
  Y_s_LASSO   <- as.numeric(predict(fit_LASSO, newx = as.matrix(X_s),   s = "lambda.min"))
  
  ma_LASSO <- model_assisted_estimator( Y_pop_pred = Y_pop_LASSO, Y_s_pred = Y_s_LASSO, 
                                        HT_estim = HT, incl_prob_vec = incl_prob_vec, N = N)
  
  
  ### ---------------------------
  ### 5) Elastic Net
  ### ---------------------------
  fit_ENET <- glmnet::cv.glmnet(
    x = as.matrix(X_s),
    y = Y_s,
    family = "gaussian",
    alpha = 0.5,   
    nfolds = 5,
    standardize = TRUE
  )
  
  Y_pop_ENET <- as.numeric(predict(fit_ENET, newx = as.matrix(X_pop), s = "lambda.min"))
  Y_s_ENET   <- as.numeric(predict(fit_ENET, newx = as.matrix(X_s),   s = "lambda.min"))
  
  ma_NET <- model_assisted_estimator(
    Y_pop_pred = Y_pop_ENET, Y_s_pred = Y_s_ENET, 
    HT_estim = HT, incl_prob_vec = incl_prob_vec, N = N)
  
  
  return(list(
    HT      = HT,
    ma_OLS  = ma_OLS,
    ma_RF   = ma_RF,
    ma_GBM  = ma_GBM,
    ma_LASSO  = ma_LASSO,
    ma_NET  = ma_NET
  ))
  
}


