set.seed(1234)
library(dplyr)
library(rstan)
options(mc.cores = 4L)

J <- 150 # number of groups
Jn <- 1500 # number of observations per group
N <- J * Jn # total number of observations

# j indexes groups

xb1 <- runif(N, -1, 1) # global mean + variance predictor 1
xb2 <- runif(N, -1, 1) # global mean + variance predictor 2
xh1 <- runif(J, -1, 1) # group level predictor 1
xh1 <- xh1 - mean(xh1) # center
xh2 <- runif(J, -1, 1) # group level predictor 2
xh2 <- xh2 - mean(xh2) # center
b1 <- 0.5 # global mean coefficient 1
b2 <- -0.1 # global mean coefficient 2
g1 <- 0.1 # global variance coefficient 1
g2 <- 0.1 # global variance coefficient 2
h1 <- -0.8 # group level coefficient 1
h2 <- -0.1 # group level coefficient 2
g0 <- -0.7 # global intercept on variance
offset <- rep(0, N)
taus <- 0.1 # standard deviation on group level deviations
b1j <- rnorm(J, 0, taus) %>% rep(each = Jn)
g1j <- rnorm(J, 0, taus) %>% rep(each = Jn)
b2j <- rnorm(J, 0, taus) %>% rep(each = Jn)
g2j <- rnorm(J, 0, taus) %>% rep(each = Jn)
g0j <- rnorm(J, h1 * xh1 + h2 * xh2, 0.07) %>% rep(each = Jn) # group level variance deviations

mu <- (b1 + b1j) * xb1 + (b2 + b2j) * xb2 + offset
sigma <- exp(g0 + g0j + (g1 + g1j) * xb1 + (g2 + g2j) * xb2)
# sigma <- 0.1
median(sigma)

y <- rnorm(N, mu, sigma)

# library(ggplot2)
# data_frame(groups, y, xb1, xb2, mu) %>%
#   ggplot(aes(xb1, y, colour = xb2)) + geom_point() + facet_wrap(~groups)

par(mfrow = c(2, 2))
# showing the partial effect of h1:
residual <- unique(g0j) - (h1 * xh1 + h2 * xh2)
sd(residual)
plot(xh1, g0 + h1 * xh1 + residual, ylim = c(-1.3, 0.3))
abline(a = g0, b = h1)

# showing the partial effect of h2:
plot(xh2, g0 + h2 * xh2 + residual, ylim = c(-1.3, 0.3))
abline(a = g0, b = h2)

# without removing the effect of days fished:
plot(xh1, g0 + unique(g0j), ylim = c(-1.3, 0.3))
abline(a = g0, b = h1)

# same:
# plot(xh1, g0 + h1 * xh1 + h2 * xh2 + residual)
# abline(a = g0, b = h1)

plot(xh2, g0 + unique(g0j), ylim = c(-1.3, 0.3))
abline(a = g0, b = h2)

mm <- model.matrix(~-1 + xb1 + xb2)
mm2 <- mm
groups <- rep(seq_len(J), each = Jn)

library(lme4)
mlmer <- lmer(y ~ -1 + xb1 + xb2 + (-1 + xb1|groups) + (-1 + xb2|groups))
summary(mlmer)

standat <- list(
  N = N,
  J = ncol(mm),
  X_ij = as.matrix(mm),

  K = ncol(mm2),
  X_sigma_ik = as.matrix(mm2),

  y_i = y,
  offset = offset, # ignore

  n_strategy = J,
  strategy_i = groups,

  n_str_yr = 1, # ignore
  str_yr_i = rep(1, N), # ignore

  b1_cov_i = xb1,
  b2_cov_i = xb2,
  g1_cov_i = xb1,
  g2_cov_i = xb2,

  mean_div_str = xh1,
  mean_day_str = xh1
)

beta_init <- function() rnorm(standat$J)
sigma_init <- function() rnorm(standat$K)
dev_str_init <- function() rnorm(standat$n_strategy, 0, 0.05)
dev_yr_init <- function() rnorm(standat$n_str_yr, 0, 0.05)
tau_init <- function() runif(1, 0.05, 0.1)
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

msim <- stan("portfolio/analysis/portfolio-offset-sim.stan",
  data = standat, iter = 400, chains = 4,
  pars = c("mu", "sigma"), include = FALSE, init = init_fun)
save(msim, file = "portfolio/data-generated/nsim.rda")
load("portfolio/data-generated/nsim.rda")

b <- broom::tidy(msim, estimate.method = "median", rhat = T, ess = T)
filter(b, grepl("^b_j|^g_k|^h", term))
filter(b, grepl("^g0$", term))
filter(b, grepl("tau", term))

stan_g0 <- filter(b, grepl("^g0$", term))$estimate
stan_g0j <- filter(b, grepl("^g0_strategy\\[", term))$estimate
stan_h1 <- filter(b, grepl("^h1", term))$estimate
stan_h2 <- filter(b, grepl("^h2", term))$estimate

par(mfrow = c(2, 2))

plot(g0 + unique(g0j), stan_g0 + stan_g0j, asp = 1)
abline(a = 0, b = 1)

stan_residual <- stan_g0j - (stan_h1 * xh1 + stan_h2 * xh2)
plot(residual, stan_residual, asp = 1)
abline(a = 0, b = 1)

plot(xh1, g0 + h1 * xh1 + residual)
abline(a = g0, b = h1)

plot(xh1, stan_g0 + stan_h1 * xh1 + stan_residual)
abline(a = stan_g0, b = stan_h1)
