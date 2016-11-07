library(dplyr)
library(rstan)
options(mc.cores = 4L)
rstan_options(auto_write = TRUE)

source("portfolio/analysis/3.0-prep-model-matrix.R")
load("portfolio/data-generated/diff-dat-stan.rda")

b1 <- function(x, bp = 0) ifelse(x < bp, x, 0)
b2 <- function(x, bp = 0) ifelse(x < bp, 0, x)

standat_ifq <- list(
  N = nrow(dat),
  J = ncol(mm),
  X_ij = as.matrix(mm),

  K = ncol(mm2),
  X_sigma_ik = as.matrix(mm2),

  y_i = log(dat$revenue),
  offset = log(dat$revenue.prev),

  n_strategy = length(unique(dat$strategy_ifq_id)),
  strategy_i = dat$strategy_ifq_id,
  n_str_yr = length(unique(dat$str_ifq_yr_id)),
  str_yr_i = dat$str_ifq_yr_id,

  b1_cov_i = b1(dat$spec_change),
  b2_cov_i = b2(dat$spec_change),
  g1_cov_i = b1(dat$spec_change),
  g2_cov_i = b2(dat$spec_change),

  mean_div_str = md_ifq$scaled_strategy_mean_div,
  mean_day_str = md_ifq$scaled_strategy_mean_days
)

# custom tighter inits:
beta_init <- function() rnorm(standat_ifq$J)
sigma_init <- function() rnorm(standat_ifq$K)
dev_str_init <- function() rnorm(standat_ifq$n_strategy, 0, 0.1)
dev_yr_init <- function() rnorm(standat_ifq$n_str_yr, 0, 0.1)
tau_init <- function() runif(1, 0.05, 0.5)
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

m_ifq <- stan("portfolio/analysis/portfolio-offset.stan",
  data = standat_ifq, iter = 2000, chains = 4,
  pars = c("mu", "sigma"), include = FALSE, init = init_fun)
save(m_ifq, standat_ifq, file = "portfolio/data-generated/m_ifq.rda")
