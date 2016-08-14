library(dplyr)
library(rstan)
options(mc.cores = 4L)
rstan_options(auto_write = TRUE)

source("portfolio/analysis/prep-stan-model-matrix.R")

standat <- list(
  N = nrow(dat),
  J = ncol(mm),
  X_ij = as.matrix(mm),

  K = ncol(mm2),
  X_sigma_ik = as.matrix(mm2),

  y_i = log(dat$revenue),
  offset = log(dat$revenue.prev),

  n_strategy = length(unique(dat$strategy_id)),
  strategy_i = dat$strategy_id,
  n_str_yr = length(unique(dat$str_yr_id)),
  str_yr_i = dat$str_yr_id,

  b1_cov_i = b1(dat$spec_change),
  b2_cov_i = b2(dat$spec_change),
  g1_cov_i = b1(dat$spec_change),
  g2_cov_i = b2(dat$spec_change),

  mean_div = dat$strategy_mean_div,
  mean_div_str = md$strategy_mean_div
  )

# custom tighter inits:
beta_init <- function() runif(standat$J, -0.1, 0.1)
sigma_init <- function() runif(standat$K, -0.1, 0.1)
dev_str_init <- function() runif(standat$n_strategy, 0, 0)
dev_yr_init <- function() runif(standat$n_str_yr, 0, 0)
tau_init <- function() runif(1, 0.2, 0.4)
init_fun <- function() {
  list(
    b0 = beta_init()[1],
    b0_strategy = dev_str_init(),
    b0_str_yr = dev_yr_init(),
    b0_strategy_tau = tau_init(),
    b0_str_yr_tau = tau_init(),
    b_j = beta_init(),
    h1 = beta_init()[1],
    h2 = beta_init()[1],
    b1_strategy = dev_str_init(),
    b1_strategy_tau = tau_init(),
    b2_strategy = dev_str_init(),
    b2_strategy_tau = tau_init(),
    g0 = sigma_init()[1],
    g0_strategy = dev_str_init(),
    g0_strategy_tau = tau_init(),
    g_k = sigma_init(),
    g1_strategy = dev_str_init(),
    g1_strategy_tau = tau_init(),
    g2_strategy = dev_str_init(),
    g2_strategy_tau = tau_init())
}

m <- stan("portfolio/analysis/portfolio-offset.stan",
  data = standat, iter = 400, chains = 4,
  pars = c("mu", "sigma", "b0_str_yr"), include = FALSE) #, init = init_fun)
save(m, file = "portfolio/data-generated/m.rda")
b <- broom::tidy(m, conf.int = T, estimate.method = "median", rhat = T, ess = T)
filter(b, rhat > 1.15)
filter(b, ess < 40)
filter(b, grepl("^h1", term))
filter(b, grepl("^b0", term))
filter(b, grepl("^g0", term))
filter(b, grepl("^g_k", term))
filter(b, grepl("^b_j", term))
filter(b, grepl("*_tau$", term))