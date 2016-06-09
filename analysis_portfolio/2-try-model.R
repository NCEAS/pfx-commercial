library(dplyr)
library(rstan) # >= 2.7.0-1
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(ggplot2)

set.seed(123)
b0 <- 0.5
b1 <- 0.3
sigma0 <- 0.2
sigma1 <- -0.2
N <- 500
s <- base::sample(seq(1, 10), N, replace = TRUE)
u <- (rnorm(N, b0 + b1*s, sqrt(exp(2*(sigma0 + sigma1*s)))))

plot(s, u)

d <- readRDS("generated-data/species_diversity_metrics.rds")
ggplot(d, aes(log(diversity_by_earnings), log(10^m))) + geom_point(alpha = 0.1)
ggplot(d, aes(log(diversity_by_earnings), log(cv))) + geom_point(alpha = 0.1)

library(nlme)
m_gls1 <- gls(u ~ s, weights = varExp(form = ~s))
m_gls2 <- gls(log(10^m) ~ log(diversity_by_earnings), weights = varExp(form = ~log(diversity_by_earnings)), data = d)
summary(m_gls1)
summary(m_gls2)

de <- seq(1, 5, length.out = 100)
predicted <- data.frame(diversity_by_earnings = de)
predicted <- mutate(predicted, pm = predict(m_gls2, newdata = predicted))
se <- summary(m_gls2)$model[[1]][[1]]
predicted <- mutate(predicted, pl = pm -1.96*sqrt(exp(2*(se*log(diversity_by_earnings)))))
predicted <- mutate(predicted, pu = pm +1.96*sqrt(exp(2*(se*log(diversity_by_earnings)))))

ggplot(d, aes(diversity_by_earnings, log(10^m))) + geom_point(alpha = 0.1) +
  geom_line(data = predicted, aes(x = (diversity_by_earnings), pm), color = "red") +
  geom_line(data = predicted, aes(x = (diversity_by_earnings), pl), color = "red") +
  geom_line(data = predicted, aes(x = (diversity_by_earnings), pu), color = "red")

m_stan <- stan("portfolio.stan", iter = 400,
  data = list(
    N = N,
    y = u,
    x = s
    ), pars = c("b0", "b1", "sigma0", "sigma1"))

m_stan

nd <- nrow(d)
d2 <- d[sample(seq_len(nd), round(nd/3, 0)),]
m_stan <- stan("portfolio.stan", iter = 300,
  data = list(
    N = nrow(d2),
    y = log(10^d2$m),
    x = log(d2$diversity_by_earnings)
    ), pars = c("b0", "b1", "sigma0", "sigma1"))

m_stan
p <- d2

# devtools::install_github("seananderson/stanhelpers")
library(stanhelpers)
e <- extract_df(m_stan, "long_df") %>%
  filter(variable != "lp__")
ew <- extract_df(m_stan, "wide_df")
ggplot(e, aes(value)) + geom_density() + facet_wrap(~variable, scales = "free")

with(d2, plot(diversity_by_earnings, log(10^m), col = "#00000030"))
de <- seq(1, 5, length.out = 100)
p <- sapply(de, function(x) { median(exp(ew$b0 + ew$b1*log(x))) })
u <- sapply(de, function(x) { median(
    exp(ew$b0 + ew$b1*log(x) + 1.96*(sqrt(exp(2*(ew$sigma0 + ew$sigma1*log(x)))) ))
    ) })
l <- sapply(de, function(x) { median(
    exp(ew$b0 + ew$b1*log(x) - 1.96*(sqrt(exp(2*(ew$sigma0 + ew$sigma1*log(x)))) ))
    ) })
lines(log(de), log(p), col = "red", lwd = 2)
lines(log(de), log(u), col = "red", lwd = 2)
lines(log(de), log(l), col = "red", lwd = 2)

with(d2, plot(log(diversity_by_earnings), log(10^m), col = "#00000030"))
p <- sapply(de, function(x) { median(exp(ew$b0 + ew$b1*log(x))) })
u <- sapply(de, function(x) { median(
    exp(ew$b0 + ew$b1*log(x) + 1.96*(sqrt(exp(2*(ew$sigma0 + ew$sigma1*log(x)))) ))
    ) })
l <- sapply(de, function(x) { median(
    exp(ew$b0 + ew$b1*log(x) + 1.96*(sqrt(exp(2*(ew$sigma0 + ew$sigma1*log(x)))) ))
    ) })
lines(log(de), p, col = "red", lwd = 2)
lines(log(de), u, col = "red", lwd = 2)
lines(log(de), l, col = "red", lwd = 2)



b0 <- extract(m_stan)$b0
b1 <- extract(m_stan)$b0
sigma0 <- extract(m_stan)$sigma0
sigma1 <- extract(m_stan)$sigma1



a2 <- extract(m)$alphas_eco
med <- apply(a, 2, median)
med2 <- apply(a2, 2, median)
# jit <- jitter(rep(0, n_groups), 0.3)
plot(alphas2, med)
plot(alphas, med2)


