library(dplyr)
library(rstan)
options(mc.cores = 4L)
rstan_options(auto_write = TRUE)

source("portfolio/analysis/3.0-prep-model-matrix.R")
load("portfolio/data-generated/diff-dat-stan.rda")

b1 <- function(x, bp = 0) ifelse(x < bp, x, 0)
b2 <- function(x, bp = 0) ifelse(x < bp, 0, x)

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

  mean_div_str = md$scaled_strategy_mean_div,
  mean_day_str = md$scaled_strategy_mean_days
)

m <- stan("portfolio/analysis/portfolio-offset.stan",
  data = standat, iter = 2000, chains = 4,
  pars = c("mu", "sigma"), include = FALSE)
save(m, standat, file = "portfolio/data-generated/m.rda")

b <- broom::tidy(m1, conf.int = TRUE, estimate.method = "median", rhat = TRUE, ess = TRUE)
filter(b, rhat > 1.05)
filter(b, ess < 100)
filter(b, grepl("^h1", term))
filter(b, grepl("^b0", term))
filter(b, grepl("^g0", term))
filter(b, grepl("^g_k", term))
filter(b, grepl("^b_j", term))
filter(b, grepl("*_tau$", term))

# Fit the same model without the effort predictor

standat_noeffort <- standat
effort_columns <- grep("days", names(as.data.frame(standat_noeffort$X_ij)))
standat_noeffort$X_ij <- standat_noeffort$X_ij[, -effort_columns]
standat_noeffort$X_sigma_ik <- standat_noeffort$X_sigma_ik[, -effort_columns]
standat_noeffort$J <- ncol(standat_noeffort$X_ij)
standat_noeffort$K <- ncol(standat_noeffort$X_sigma_ik)

m_noeffort <- stan("portfolio/analysis/portfolio-offset.stan",
  data = standat_noeffort, iter = 1000, chains = 4,
  pars = c("mu", "sigma"), include = FALSE)
save(m_noeffort, standat, file = "portfolio/data-generated/m_noeffort.rda")

## m_ns <- stan("portfolio/analysis/portfolio-offset-nosigma.stan",
##   data = standat, iter = 120, chains = 2,
##   pars = c("mu", "sigma", "b0_str_yr"), include = FALSE)
## save(m_ns, file = "portfolio/data-generated/m_ns.rda")
## broom::tidy(m_ns, rhat = T, ess = T) %>% filter(grepl("b_j", term))
## broom::tidy(m_ns, rhat = T, ess = T) %>% filter(grepl("tau", term))
## broom::tidy(m, rhat = T, ess = T) %>% filter(grepl("b_j", term))
## broom::tidy(m, rhat = T, ess = T) %>% filter(grepl("tau", term))
## library(lme4)
##
## b1 <- function(x, bp = 0) ifelse(x < bp, x, 0)
## b2 <- function(x, bp = 0) ifelse(x < bp, 0, x)
##
## library(lme4)
## m_lmer <- lmer(
##   log(revenue) ~ -1 + b1(spec_change) + b2(spec_change) +
##     days_change + b1(spec_change):days_change + b2(spec_change):days_change +
##     (-1 + b1(spec_change) |strategy) +
##     (-1 + b2(spec_change) |strategy)+
##     (1|strategy_year),
##   data = dat, offset = log(revenue.prev))
## arm::display(m_lmer)
## broom::tidy(m_ns, rhat = T, ess = T) %>% filter(grepl("b_j", term))
##
