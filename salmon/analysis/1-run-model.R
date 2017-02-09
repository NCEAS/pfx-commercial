library(dplyr)

diffdat = readRDS(file="salmon/data-generated/salmon.rds")

diffdat = filter(diffdat, !is.na(days.change))

# prep data for stan
N = nrow(diffdat) # number of rows of data
X_ij = diffdat[,c("days.change")] #"specDiv.change","specdiv.prev",
#if(class(X_ij)=="numeric") X_ij = matrix(X_ij,ncol=1)
J = ncol(X_ij) # number of covariates on mean
X_sigma_ik = diffdat[,c("days.change")] #"specDiv.change","specdiv.prev",
#if(class(X_sigma_ik)=="numeric") X_sigma_ik = matrix(X_sigma_ik,ncol=1)
K = ncol(X_sigma_ik) # number of covariates on variance

offset = log(diffdat$revenue.prev)
y_i = log(diffdat$revenue)
n_strategy = length(unique(diffdat$permit))
strategy_i = as.numeric(as.factor(diffdat$permit))
n_str_yr = length(unique(diffdat$year_permit))
str_yr_i = as.numeric(as.factor(diffdat$year_permit))
n_yr = length(unique(diffdat$year))
year_i = diffdat$year - min(diffdat$year) + 1

b1_cov_i = diffdat$specDiv.change # log(percent change in diversity)
b2_cov_i = diffdat$specDiv.change * diffdat$specdiv.prev # log(percent change in diversity)
b3_cov_i = diffdat$specDiv.change * (diffdat$specdiv.prev^2)
b4_cov_i = diffdat$days.change

# create list for STAN
stan_data = list("N"=N, "y_i"=y_i, "offset"=offset, "n_strategy"=n_strategy,
  "n_str_yr"=n_str_yr,"str_yr_i"=str_yr_i, "n_yr"=n_yr, "year_i"=year_i,
  "b1_cov_i"=b1_cov_i, "b2_cov_i"=b2_cov_i, "b3_cov_i"=b3_cov_i, "b4_cov_i"=b4_cov_i,
  "g1_cov_i"=b1_cov_i, "g2_cov_i"=b2_cov_i, "g3_cov_i"=b3_cov_i, "g4_cov_i"=b4_cov_i)

if(modelname=="quadratic") {

  stan_pars = c("b0_str_yr", "b0_str_yr_tau", "g0_strategy", "g0_strategy_tau",
    "b_1", "b_2", "b_3", "b_4", "b1_str_yr_tau", "b2_str_yr_tau", "b3_str_yr_tau",
    "b4_str_yr_tau", "b1_str_yr_mu", "b2_str_yr_mu", "b3_str_yr_mu",
    "b4_str_yr_mu", "g0", "g_1", "g_2", "g_3", "g_4", "g1_str_yr_tau", "g2_str_yr_tau",
    "g3_str_yr_tau", "g4_str_yr_tau", "g1_str_yr_mu", "g2_str_yr_mu", "g3_str_yr_mu",
    "g4_str_yr_mu")

  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  # estimate model. This model is modified from the simulation model by (1) including indices to allow NAs in the inputted data, and (2) including estimated year effects (intercepts)
  mod = stan(file = 'salmon/analysis/portfolio-offset-quadratic.stan',data = stan_data,
    verbose = TRUE, chains = 3, thin = 1, warmup = 1000, iter = 2000, pars = stan_pars)

  save.image("salmon/analysis/model-quadratic.Rdata")

}

if(modelname=="linear") {
  stan_pars = c("b0_str_yr", "b0_str_yr_tau", "g0_strategy", "g0_strategy_tau",
    "b_1", "b_2", "b_4", "b1_str_yr_tau", "b2_str_yr_tau",
    "b4_str_yr_tau", "b1_str_yr_mu", "b2_str_yr_mu",
    "b4_str_yr_mu", "g0", "g_1", "g_2", "g_4", "g1_str_yr_tau", "g2_str_yr_tau",
    "g4_str_yr_tau", "g1_str_yr_mu", "g2_str_yr_mu",
    "g4_str_yr_mu")

  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  # estimate model. This model is modified from the simulation model by (1) including indices to allow NAs in the inputted data, and (2) including estimated year effects (intercepts)
  mod = stan(file = 'salmon/analysis/portfolio-offset-linear.stan',data = stan_data,
    verbose = TRUE, chains = 3, thin = 1, warmup = 1000, iter = 2000, pars = stan_pars)

  save.image("salmon/analysis/model-linear.Rdata")
}

if(modelname=="linear_varrw") {
  stan_pars = c("b0_str_yr", "b0_str_yr_tau", "g_0", "g0_strategy_tau",
    "b_1", "b_2", "b_4", "b1_str_yr_tau", "b2_str_yr_tau",
    "b4_str_yr_tau", "b1_str_yr_mu", "b2_str_yr_mu",
    "b4_str_yr_mu", "g0", "g_1", "g_2", "g_4", "g1_str_yr_tau", "g2_str_yr_tau",
    "g4_str_yr_tau", "g1_str_yr_mu", "g2_str_yr_mu",
    "g4_str_yr_mu")

  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  # estimate model. This model is modified from the simulation model by (1) including indices to allow NAs in the inputted data, and (2) including estimated year effects (intercepts)
  mod = stan(file = 'salmon/analysis/portfolio-offset-linear-varrw.stan',data = stan_data,
    verbose = TRUE, chains = 3, thin = 1, warmup = 1000, iter = 2000, pars = stan_pars)

  save.image("salmon/analysis/model-linear-varrw.Rdata")
}

if(modelname=="interceptonly") {
stan_pars = c("b0_str_yr", "b0_str_yr_tau", "g0_strategy", "g0_strategy_tau",
  "b_1", "b_4", "b1_str_yr_tau", "b4_str_yr_tau",
  "b4_str_yr_mu", "g0", "g_1", "g_4", "g1_str_yr_tau",
  "g4_str_yr_tau", "g4_str_yr_mu")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# estimate model. This model is modified from the simulation model by (1) including indices to allow NAs in the inputted data, and (2) including estimated year effects (intercepts)
mod = stan(file = 'salmon/analysis/portfolio-offset-linear-nointeraction.stan',data = stan_data,
  verbose = TRUE, chains = 3, thin = 1, warmup = 1000, iter = 2000, pars = stan_pars)

save.image("salmon/analysis/model-linear-nointeractions.Rdata")

}


#############################################################
# Fit model with interactions
#############################################################
stan_pars = c("b0_str_yr", "b0_str_yr_tau", "g0_strategy", "g0_strategy_tau",
  "b_1", "b_2", "b_4", "b1_str_yr_tau", "b2_str_yr_tau",
  "b4_str_yr_tau", "b1_str_yr_mu", "b2_str_yr_mu",
  "b4_str_yr_mu", "g0", "g_1", "g_2", "g_4", "g1_str_yr_tau", "g2_str_yr_tau",
  "g4_str_yr_tau", "g1_str_yr_mu", "g2_str_yr_mu",
  "g4_str_yr_mu")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# estimate model. This model is modified from the simulation model by (1) including indices to allow NAs in the inputted data, and (2) including estimated year effects (intercepts)
mod = stan(file = 'salmon/analysis/portfolio-offset-linear.stan',data = stan_data,
  verbose = TRUE, chains = 3, thin = 1, warmup = 1000, iter = 2000, pars = stan_pars)

save.image("salmon/analysis/model-linear.Rdata")

