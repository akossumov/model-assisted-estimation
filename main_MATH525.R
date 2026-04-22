### This will be needed for cluster usage in case computations become too expensive

#.libPaths(c("~/R/library", .libPaths()))

# TODO: Load packages required for your estimation procedures
#library(doParallel, lib.loc="~/R/library")
#library(MASS, lib.loc="~/R/library")

# !!! IF YOU RUN SIMULATIONS ON YOUR LOCAL PC, DO NOT EXECUTE THE CODE ABOVE, BUT PLEASE STILL LOAD PACKAGES THAT YOU WILL USE (see TODO above) !!!

library(doParallel)
library(MASS)

### SIMULATION PARAMETERS

# !!! Firstly, let us try (computationally) simpler scenarios !!!
N_vec <- c(500) # considered population size
n_proportion <- c(0.3) # proportion of population used as sample size

all_pred_proportion <- seq(0.1, 2, 0.1)
true_pred_proportion <- 0.7*all_pred_proportion # number of true predictors = true_pred_proportion * N, where N is a population size
noise_pred_proportion <- 0.3*all_pred_proportion # number of noise predictors = noise_pred_proportion * n, where n is a sample size

num_MC <- 200 # number of Monte-Carlo simulations at each simulation scenario

### SOURCE ALL REQUIRED FUNCTIONS THAT WILL BE NEEDED
setwd("/home/jiayin99/links/scratch/MATH525/")
source("functions_MATH525.R")
source("simulation_scenario_MATH525.R")

### CREATE GRID OF SIMULATION SCENARIOS
pred_pairs <- data.frame(
  true_pred_proportion = true_pred_proportion,
  noise_pred_proportion = noise_pred_proportion
)

scen_GRID <- expand.grid(
  N = N_vec,
  n_proportion = n_proportion,
  pair_id = seq_len(nrow(pred_pairs))
)

# Attach the matched pairs
scen_GRID$true_pred_proportion <- pred_pairs$true_pred_proportion[scen_GRID$pair_id]
scen_GRID$noise_pred_proportion <- pred_pairs$noise_pred_proportion[scen_GRID$pair_id]

# Optional: remove helper column
scen_GRID$pair_id <- NULL

scen_GRID$n <- round(scen_GRID$N * scen_GRID$n_proportion)
scen_GRID$p_true <- round(scen_GRID$true_pred_proportion * scen_GRID$n) 
scen_GRID$p_noise <- round(scen_GRID$noise_pred_proportion * scen_GRID$n)

scen_LIST <- split(scen_GRID, seq(nrow(scen_GRID))) # List representation of the considered simulation scenarios

### LOG DIRECTORY (RESULTS WILL BE STORED IN THIS DIRECTORY)

log_dir <- "logs"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

### PARALLEL SETUP

# Set up parallel backend
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# export global objects used in foreach
clusterExport(cl, c("num_MC", "scen_LIST", "log_dir"))

# To improve computational performance, the entire simulation study is parallelized across the rows of scen_LIST
clusterEvalQ(cl,{
  source("functions_MATH525.R")
  source("simulation_scenario_MATH525.R")
  library(MASS)
  library(randomForest)
  library(gbm)
  library(glmnet)
  library(Matrix)
})

### PARALLEL SIMULATION

# Parallelization with respect to different rows of scen_LIST
results_list <- foreach(scen_row = scen_LIST,
                        .packages = c("MASS", "randomForest", "gbm", "glmnet", "Matrix")) %dopar% {
                          
                          N <- scen_row$N
                          n <- scen_row$n
                          p_true <- scen_row$p_true
                          p_noise <- scen_row$p_noise
                          # Create a log file for (N,n,p_true,p_noise), which will help us track at each step the simulation is being conducted
                          log_file <- file.path(log_dir,
                                                paste0("N_",N,
                                                       "_n_",n,
                                                       "_ptrue_",p_true,
                                                       "_pnoise_",p_noise,
                                                       ".csv"))
                          
                          # Column names of the log file
                          # TODO: Add name of your method here as ma_SRSWOR_name and ma_Bern_OLS ("ma" means model assisted estimator)
                          column_names <- c(
                            "N", "n", "p_true", "p_noise", "mu_true", "iteration", 
                            "HT_SRSWOR", "ma_SRSWOR_OLS", "ma_SRSWOR_RF", "ma_SRSWOR_GBM", "ma_SRSWOR_LASSO","ma_SRSWOR_NET",  
                            "HT_Bern", "ma_Bern_OLS", "ma_Bern_RF","ma_Bern_GBM", "ma_Bern_LASSO", "ma_Bern_NET"
                          )
                          
                          write(paste(column_names, collapse=","), file=log_file)
                          
                          res <- simulate_scenario(N,n,p_true,p_noise,num_MC,log_file)
                          
                          return(res)
                        }

stopCluster(cl)
