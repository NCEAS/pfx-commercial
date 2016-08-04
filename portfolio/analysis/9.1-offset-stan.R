library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("portfolio/analysis/cull-dat.R")
dat <- cullDat(diff = TRUE)
# downsample for fast testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.04)
set.seed(123)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

dat <- dat %>% group_by(strategy) %>% mutate(n=n()) %>% filter(n > 30)
nrow(dat)
length(unique(dat$strategy))

dat <- mutate(dat,
  revenue_change = log(revenue/revenue.prev),
  days_change = log((days_permit+1)/(days_permit.prev+1)),
  spec_change = log(specDiv/specdiv.prev))
dat$strategy_year <- paste(dat$strategy, dat$year, sep = ":")
dat$strategy_id <- as.numeric(as.factor(as.character(dat$strategy)))
dat$str_yr_id <- as.numeric(as.factor(as.character(dat$strategy_year)))
dat <- dat %>% group_by(strategy) %>%
  mutate(strategy_mean_div = mean(specDiv)) %>% as_data_frame

mm <- model.matrix(log(revenue) ~ -1 + days_change * spec_change, data = dat)
mm2 <- model.matrix(log(revenue) ~ -1 + days_change * spec_change, data = dat)

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

  b1_cov_i = dat$scaled_spec_div,
  # b2_cov_i = dat$log_days_permit,
  g1_cov_i = dat$scaled_spec_div,

  mean_div = dat$strategy_mean_div)

# custom tighter inits:
beta_init <- function() runif(standat$J, -0.1, 0.1)
sigma_init <- function() runif(standat$K, -0.1, 0.1)
# start at 0 in case no strategy-level variation:
dev_str_init <- function() runif(standat$n_strategy, 0, 0)
dev_yr_init <- function() runif(standat$n_str_yr, 0, 0)
tau_init <- function() runif(1, 0.3, 0.6)

init_fun <- function(seed = sample(seq_len(1e4), 1)) {
  set.seed(seed)
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
    g1_strategy_tau = tau_init())
}

m <- stan("portfolio/analysis/portfolio-offset.stan",
  data = standat, iter = 80, chains = 2,
  pars = c("mu", "sigma", "b0_str_yr"), include = FALSE, init = init_fun)
