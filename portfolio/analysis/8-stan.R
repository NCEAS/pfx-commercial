source("portfolio/analysis/cull-dat.R")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

nrow(dat)
sum(!is.na(dat$length))
dat<-dat[!is.na(dat$length), ]
nrow(dat)

dat <- group_by(dat, strategy) %>%
  mutate(ntotal = n(), ngreater1 = sum(specDiv>1.1), nratio = ngreater1/ntotal) %>%
  as_data_frame() %>%
  mutate(spec_div_all_1 = ifelse(nratio < 0.1, 1L, 0L)) %>%
  select(-ngreater1)
dat <- dat %>% filter(spec_div_all_1 == 0L)

# Downsample for speed of testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.03)
set.seed(3)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

dat <- dat %>% group_by(strategy) %>% mutate(n=n()) %>% filter(n > 70)
nrow(dat)

ggplot(dat, aes(scaled_spec_div, log(revenue), colour = log_days_permit)) +
  geom_point(alpha=0.1) +
  facet_wrap(~strategy)

ggplot(dat, aes(log_days_permit, log(revenue), colour = scaled_spec_div)) +
  geom_point(alpha=0.1) +
  facet_wrap(~strategy)

dat$p_holder <- paste(dat$p_holder, dat$strategy, sep = ":")

mm <- model.matrix(revenue ~ I(scaled_spec_div^2) + I(log_days_permit^2) + I(log_length^2) + (scaled_spec_div + log_days_permit + log_length)^2, data = dat)[,-1]
mm2 <- model.matrix(revenue ~ (scaled_spec_div + log_days_permit + log_length)^2, data = dat)[,-1]

standat <- list(N = nrow(dat),
  J = ncol(mm),
  X_ij = as.matrix(mm),

  J_sig = ncol(mm2),
  X_sig_ij = as.matrix(mm2),

  y = log(dat$revenue/1e4),

  n_strategy = length(unique(dat$strategy)),
  strategy_i = as.numeric(as.factor(as.character(dat$strategy))),
  n_person = length(unique(dat$p_holder)),
  person_i = as.numeric(as.factor(as.character(dat$p_holder))),

  z1_i = dat$scaled_spec_div,
  # z2_i = dat$log_days_permit,
  z1_sig_i = dat$scaled_spec_div
  )

beta_init <- function() runif(standat$J, -0.1, 0.1)
sigma_init <- function() runif(standat$J_sig, -0.1, 0.1)
dev_str_init <- function() runif(standat$n_strategy, -0.05, 0.05)
dev_per_init <- function() runif(standat$n_person, -0.05, 0.05)
tau_init <- function() runif(1, 0.3, 0.6)

init_fun <- function() { 
  list(
    b0 = rnorm(1, mean(standat$y), 0.2),
    b0_dev_str = dev_str_init(),
    b0_dev_per = dev_per_init(),
    b0_tau_str = tau_init(),
    b0_tau_per = tau_init(),
    b1 = beta_init(),
    b1_dev = dev_str_init(),
    b1_tau = tau_init(),
    b2_tau = tau_init(),
    sigma0 = beta_init()[1],
    sigma0_dev = dev_str_init(),
    sigma0_tau = tau_init(),
    sigma1 = sigma_init(),
    b1_sig_dev = dev_str_init(),
    b1_sig_tau = tau_init())
} 

m <- stan("portfolio/analysis/portfolio2.stan", data = standat, iter = 100, chains = 3, 
  pars = c("mu", "sigma"), include = FALSE, init = init_fun)

m

pdf("portfolio/figs/stan-traces.pdf", width = 10, height = 8)
traceplot(m, pars = c(
  "b0_tau_str", 
  "b0_tau_per", 
  "b1_tau", 
  # "b2_tau", 
  "sigma0_tau", 
  "b1_sig_tau"), inc_warmup = T)
traceplot(m, pars = c("b0_dev_str"), inc_warmup = F)
# traceplot(m, pars = c("b0_dev_per"), inc_warmup = F)
traceplot(m, pars = c("b1_dev"), inc_warmup = F)
# traceplot(m, pars = c("b2_dev"), inc_warmup = T)
traceplot(m, pars = c("b1_sig_dev"), inc_warmup = F)
traceplot(m, pars = c("sigma0_dev"), inc_warmup = T)
colnames(mm2)
traceplot(m, pars = c("sigma0", "sigma1"), inc_warmup = T)
colnames(mm)
traceplot(m, pars = c("b0", "b1"), inc_warmup = T)
dev.off()
