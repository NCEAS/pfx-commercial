set.seed(1)
library(dplyr)
library(rstan)
options(mc.cores = 4L)

J <- 35 # number of groups
Jn <- 2000 # number of observations per group
N <- J * Jn # total number of observations

# j indexes groups

xb1 <- runif(N, -1, 1) # global mean + variance predictor 1
xb2 <- runif(N, -1, 1) # global mean + variance predictor 2
xh1 <- runif(J, -1, 1) # group level predictor 1
xh2 <- runif(J, -1, 1) # group level predictor 2
b1 <- 0.8 # global mean coefficient 1
b2 <- 0 # global mean coefficient 2
g1 <- 0.1 # global variance coefficient 1
g2 <- 0.1 # global variance coefficient 2
h1 <- -0.5 # group level coefficient 1
h2 <- -0.3 # group level coefficient 2
g0 <- -0.6 # global intercept on variance
offset <- rep(0, N)
taus <- 0.1 # standard deviation on group level deviations
b1j <- rnorm(J, 0, taus) %>% rep(each = Jn)
g1j <- rnorm(J, 0, taus) %>% rep(each = Jn)
b2j <- rnorm(J, 0, taus) %>% rep(each = Jn)
g2j <- rnorm(J, 0, taus) %>% rep(each = Jn)
g0j <- rnorm(J, h1 * xh1 + h2 * xh2, 0.05) %>% rep(each = Jn) # group level variance deviations

mu <- (b1 + b1j) * xb1 + (b2 + b2j) * xb2 + offset
sigma <- exp(g0 + g0j + (g1 + g1j) * xb1 + (g2 + g2j) * xb2)
# sigma <- 0.1
median(sigma)

y <- rnorm(N, mu, sigma)

# library(ggplot2)
# data_frame(groups, y, xb1, xb2, mu) %>%
#   ggplot(aes(xb1, y, colour = xb2)) + geom_point() + facet_wrap(~groups)

# proof of how to derive the effect of h1:
# at strategy-level mean days fished (xh2 values):
plot(xh1, g0 + (h1 * xh1 + unique(g0j) - h2 * xh2)/2)
abline(a = g0, b = h1)
# at overall mean xh2 (days fished):
plot(xh1, g0 + (h1 * xh1 + unique(g0j) - h2 * mean(xh2))/2)
abline(a = g0, b = h1)

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

msim <- stan("portfolio/analysis/portfolio-offset-sim.stan",
  data = standat, iter = 500, chains = 4,
  pars = c("mu", "sigma"), include = FALSE)
save(msim, file = "portfolio/data-generated/nsim.rda")
load("portfolio/data-generated/nsim.rda")

b <- broom::tidy(msim, estimate.method = "median", rhat = T, ess = T)
filter(b, grepl("^b_j|^g_k|^h", term))
filter(b, grepl("^g0$", term))
filter(b, grepl("tau", term))
plot(filter(b, grepl("^coef_g0", term))$estimate, (unique(g0j)), asp = 1)
abline(a = 0, b = 1)

plot(filter(b, grepl("^coef_g0", term))$estimate, (g0 + unique(g0j)), asp = 1)
abline(a = 0, b = 1)

plot(filter(b, grepl("^g0_strategy\\[", term))$estimate + g0, (g0 + unique(g0j)), asp = 1)
abline(a = 0, b = 1)

plot(filter(b, grepl("^g0_strategy\\[", term))$estimate, (unique(g0j)), asp = 1)
abline(a = 0, b = 1)

filter(b, grepl("^h", term))$estimate[1]
g0_str <- filter(b, grepl("^g0_strategy\\[", term))$estimate
plot(g0_str + g0, g0 + unique(g0j), asp = 1)
abline(a = 0, b = 1)


plot(xh1, g0 + (h1 * xh1 + unique(g0j) - h2 * xh2)/2)
abline(a = g0, b = h1)

plot(xh1, g0 + (h1 * xh1 + unique(g0j) - h2 * mean(xh2))/2)
abline(a = g0, b = h1)


sd(unique(g0j) - h1 * xh1 - h2 * xh2)

mean_g0j = h1 * xh1 + h2 * xh2
plot(g0 + mean_g0j, g0 + unique(g0j))
abline(a = 0, b = 1)

plot(xh1, (h1 * xh1 + mean_g0j - h2 * xh2)/2)
abline(a = 0, b = h1)

plot(xh1, filter(b, grepl("^coef_g0", term))$estimate/2+g0/2 - h2 * xh2)
abline(a = 0, b = h1)
