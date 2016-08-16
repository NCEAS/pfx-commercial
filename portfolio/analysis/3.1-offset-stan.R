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

m <- stan("portfolio/analysis/portfolio-offset.stan",
  data = standat, iter = 2000, chains = 4,
  pars = c("mu", "sigma", "b0_str_yr"), include = FALSE)
save(m, file = "portfolio/data-generated/m.rda")
b <- broom::tidy(m, conf.int = T, estimate.method = "median", rhat = T, ess = T)
filter(b, rhat > 1.05)
filter(b, ess < 100)
filter(b, grepl("^h1", term))
filter(b, grepl("^b0", term))
filter(b, grepl("^g0", term))
filter(b, grepl("^g_k", term))
filter(b, grepl("^b_j", term))
filter(b, grepl("*_tau$", term))
