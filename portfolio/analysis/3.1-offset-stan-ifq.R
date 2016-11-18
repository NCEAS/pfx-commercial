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

m_ifq <- stan("portfolio/analysis/portfolio-offset.stan",
  data = standat_ifq, iter = 2000, chains = 4,
  pars = c("mu", "sigma"), include = FALSE)
save(m_ifq, standat_ifq, file = "portfolio/data-generated/m_ifq.rda")
