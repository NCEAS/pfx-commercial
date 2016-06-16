library(dplyr)
library(rstan) # >= 2.7.0-1
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(ggplot2)

set.seed(1234)
b0 <- 0.5
b1 <- 0.3
sigma0 <- 0.2
sigma1 <- -0.2
N <- 20000
s <- base::sample(seq(1, 10), N, replace = TRUE)
u <- (rnorm(N, b0 + b1*s, sqrt(exp(sigma0 + sigma1*s))))

plot(s, u)

# d <- readRDS("generated-data/species_diversity_metrics.rds")
# ggplot(d, aes(log(diversity_by_earnings), log(10^m))) + geom_point(alpha = 0.1)
# ggplot(d, aes(log(diversity_by_earnings), log(cv))) + geom_point(alpha = 0.1)

library(nlme)
m_gls1 <- gls(u ~ s, weights = varExp(form = ~s))

library(TMB)
compile("revenue1.cpp")
dyn.load(dynlib("revenue1"))

obj <- MakeADFun(
  data = list(x = s, y = u),
  parameters = list(b0 = 0, b1 = 0, sigma0 = 0, sigma1 = 0),
  DLL = "revenue1")
opt <- nlminb(start=obj$env$last.par.best, objective=obj$fn, gradient=obj$gr)
opt$par
rep <- sdreport(obj)
rep

# with matrix notation
compile("revenue2.cpp")
dyn.load(dynlib("revenue2"))

mm <- model.matrix(~ s)

obj <- MakeADFun(
  data = list(x_ij = mm, y_i = u),
  parameters = list(b_j = c(0, 0), sigma_j = c(0, 0)),
  DLL = "revenue2")

opt <- nlminb(start=obj$env$last.par.best, objective=obj$fn, gradient=obj$gr)
opt$par
rep <- sdreport(obj)
rep

# Add a random intercept
set.seed(1234)
b0 <- 0.5
b1 <- 0.3
sigma0 <- 0.2
sigma1 <- -0.2
log_b0_sigma <- -1.8
n_k <- 30
b0_eta <- rnorm(n_k, 0, exp(log_b0_sigma))
b0_eta_i <- rep(b0_eta, each = 1000)
k_i <- rep(0:(n_k - 1), each = 1000)
N <- n_k * 1000
s <- base::sample(seq(1, 10), N, replace = TRUE)
u <- rnorm(N, b0 + b0_eta_i + b1*s, sqrt(exp(sigma0 + sigma1*s)))

compile("revenue3.cpp")
dyn.load(dynlib("revenue3"))

mm <- model.matrix(~ s)
obj <- MakeADFun(
  data = list(x_ij = mm, y_i = u, k_i = k_i, n_k = n_k),
  parameters = list(b_j = c(0, 0), sigma_j = c(0, 0), log_b0_sigma = -1, b0_k = rep(0, n_k)),
  random = c("b0_k", "b_j"),
  DLL = "revenue3")
obj$fn( obj$par )
obj$gr( obj$par )

opt <- nlminb( start=obj$par, objective=obj$fn, gradient=obj$gr, control=list("trace"=1) )
opt$par
sd_report <- sdreport(obj)
rep <- obj$report()
summary(sd_report)
summary(sd_report, "fixed")

# At a random slope
set.seed(123)
b0 <- 0.5
b1 <- 0.3
sigma0 <- 0.2
sigma1 <- -0.2
log_b0_sigma <- -1.8
log_b1_sigma <- -1.2
n_k <- 30
b0_eta <- rnorm(n_k, 0, exp(log_b0_sigma))
b0_eta_i <- rep(b0_eta, each = 1000)
b1_eta <- rnorm(n_k, 0, exp(log_b1_sigma))
b1_eta_i <- rep(b1_eta, each = 1000)
k_i <- rep(0:(n_k - 1), each = 1000)
N <- n_k * 1000
s <- base::sample(seq(1, 10), N, replace = TRUE)
u <- rnorm(N, b0 + b0_eta_i + b1*s + b1_eta_i*s, sqrt(exp(sigma0 + sigma1*s)))

compile("revenue4.cpp")
dyn.load(dynlib("revenue4"))

mm <- model.matrix(~ s)
obj <- MakeADFun(
  data = list(x_ij = mm, y_i = u, k_i = k_i, n_k = n_k, x = s),
  parameters = list(b_j = c(0, 0), sigma_j = c(0, 0), log_b0_sigma = -1, b0_k = rep(0, n_k),
  b1_k = rep(0, n_k), log_b1_sigma = -1),
  random = c("b0_k", "b_j", "b1_k"),
  DLL = "revenue4")
# obj$fn( obj$par )
# obj$gr( obj$par )

opt <- nlminb( start=obj$par, objective=obj$fn, gradient=obj$gr, control=list("trace"=1) )
# opt$par
sd_report <- sdreport(obj)
# rep <- obj$report()
summary(sd_report)
summary(sd_report, "fixed")

# Add a random slope also in the variance
set.seed(12345)
b0 <- 0.5
b1 <- 0.3
sigma0 <- 0.2
sigma1 <- -0.2
log_b0_sigma <- -1.4
log_b1_sigma <- -1.2
n_k <- 40
n_per_k <- 250
b0_eta <- rnorm(n_k, 0, exp(log_b0_sigma))
b0_eta_i <- rep(b0_eta, each = n_per_k)
b1_eta <- rnorm(n_k, 0, exp(log_b1_sigma))
b1_eta_i <- rep(b1_eta, each = n_per_k)
k_i <- rep(0:(n_k - 1), each = n_per_k)
N <- n_k * n_per_k

log_sigma0_sigma <- -1.1
log_sigma1_sigma <- -1.2
sigma0_eta <- rnorm(n_k, 0, exp(log_sigma0_sigma))
sigma0_eta_i <- rep(sigma0_eta, each = n_per_k)
sigma1_eta <- rnorm(n_k, 0, exp(log_sigma1_sigma))
sigma1_eta_i <- rep(sigma1_eta, each = n_per_k)

s <- base::sample(seq(1, 10), N, replace = TRUE)
u <- rnorm(N, b0 + b0_eta_i + b1*s + b1_eta_i*s,
  sqrt(exp(sigma0 + sigma0_eta_i + (sigma1 + sigma1_eta_i)*s)))

compile("revenue5.cpp")
dyn.load(dynlib("revenue5"))

mm <- model.matrix(~ s)
obj <- MakeADFun(
  data = list(x_ij = mm, y_i = u, k_i = k_i, n_k = n_k, x = s),
  parameters = list(b_j = c(0, 0), sigma_j = c(0, 0), log_b0_sigma = -1, b0_k = rep(0, n_k),
    b1_k = rep(0, n_k), log_b1_sigma = -1, sigma0_k = rep(0, n_k), sigma1_k = rep(0, n_k),
    log_sigma0_sigma = -1, log_sigma1_sigma = -1),
  random = c("b0_k", "b_j", "b1_k", "sigma0_k", "sigma1_k"),
  DLL = "revenue5")

opt <- nlminb( start=obj$par, objective=obj$fn, gradient=obj$gr, control=list("trace"=1) )
obj$gr(opt$par)
sd_report <- sdreport(obj)
# rep <- obj$report()

(r <- summary(sd_report, "random"))
(f <- summary(sd_report, "fixed"))
parameters <- row.names(r)
r <- as.data.frame(r)
r$parameter <- parameters

ggplot(r, aes(Estimate, parameter)) + geom_point()

par(mfrow = c(2, 2))
plot(sigma1_eta, filter(r, parameter == "sigma1_k")$Estimate)
abline(a = 0, b = 1)
plot(sigma0_eta, filter(r, parameter == "sigma0_k")$Estimate)
abline(a = 0, b = 1)

plot(b1_eta, filter(r, parameter == "b1_k")$Estimate)
abline(a = 0, b = 1)
plot(b0_eta, filter(r, parameter == "b0_k")$Estimate)
abline(a = 0, b = 1)
##################################################
compile("revenue6.cpp")
dyn.load(dynlib("revenue6"))

mm <- model.matrix(~ s)
obj <- MakeADFun(
  data = list(x_ij = mm, y_i = u, k_i = k_i, n_k = n_k, 
    b1_cov_re_i = s, sigma1_cov_re_i = s),
  parameters = list(b_j = c(0, 0), sigma_j = c(0, 0), log_b0_sigma = -1, b0_k = rep(0, n_k),
    b1_k = rep(0, n_k), log_b1_sigma = -1, sigma0_k = rep(0, n_k), sigma1_k = rep(0, n_k),
    log_sigma0_sigma = -1, log_sigma1_sigma = -1),
  random = c("b0_k", "b_j", "b1_k", "sigma0_k", "sigma1_k"),
  DLL = "revenue6")

opt <- nlminb( start=obj$par, objective=obj$fn, gradient=obj$gr, control=list("trace"=1) )
obj$gr(opt$par)
sd_report <- sdreport(obj)
# rep <- obj$report()

(r <- summary(sd_report, "random"))
(f <- summary(sd_report, "fixed"))
parameters <- row.names(r)
r <- as.data.frame(r)
r$parameter <- parameters

ggplot(r, aes(Estimate, parameter)) + geom_point()

par(mfrow = c(2, 2))
plot(sigma1_eta, filter(r, parameter == "sigma1_k")$Estimate)
abline(a = 0, b = 1)
plot(sigma0_eta, filter(r, parameter == "sigma0_k")$Estimate)
abline(a = 0, b = 1)

plot(b1_eta, filter(r, parameter == "b1_k")$Estimate)
abline(a = 0, b = 1)
plot(b0_eta, filter(r, parameter == "b0_k")$Estimate)
abline(a = 0, b = 1)
##################################################
##################################################

# Try with real data 
library(TMB)
library(dplyr)
compile("revenue6.cpp")
dyn.load(dynlib("revenue6"))
d <- readRDS("../data-generated/nonsalmon_linearModeling_complete.rds")
d <- readRDS("../data-generated/salmon_linearModeling_complete.rds")
d <- as.data.frame(d)
keep <- group_by(d, permit)  %>% summarize(n = n())  %>% 
  filter(n>50)
d <- d[d$permit %in% keep$permit, ]
d$permit_id <- as.numeric(as.factor(d$permit))
d$pholder_id <- as.numeric(as.factor(d$p_holder))
library(lme4)
x <- log(d$specDiv)
hist(x)
x <- scale(x)
mm <- model.matrix(~ x)
n_k = max(d$permit_id)
n_k = n_k
obj <- MakeADFun(
  data = list(x_ij = mm, 
    y_i = log(d$revenue), 
    k_i = d$permit_id - 1,
    n_k = n_k,
    n_j = ncol(mm) -1,
    b1_cov_re_i = x, 
    sigma1_cov_re_i = x),
  parameters = list(b_j = c(0, 0), sigma_j = c(0, 0), log_b0_sigma = -1, b0_k = rep(0, n_k),
    b1_k = rep(0, n_k), log_b1_sigma = -1, sigma0_k = rep(0, n_k), sigma1_k = rep(0, n_k),
    log_sigma0_sigma = -1, log_sigma1_sigma = -1),
  random = c("b0_k1","b0_k2", "b_j", "b1_k", "sigma0_k", "sigma1_k"),
  DLL = "revenue6")

opt <- nlminb( start=obj$par, objective=obj$fn, gradient=obj$gr, control=list(trace=1) )
obj$gr(opt$par)
sd_report <- sdreport(obj)
rep <- obj$report()

(r <- summary(sd_report, "random"))
(f <- summary(sd_report))

parameters <- row.names(r)
r <- as.data.frame(r)
r$parameter <- parameters
r <- rename(r, estimate = Estimate, se = `Std. Error`) %>% 
  mutate(l = estimate-2*se, u = estimate+2*se)

parameters <- row.names(f)
f <- as.data.frame(f)
f$parameter <- parameters
f <- rename(f, estimate = Estimate, se = `Std. Error`) %>% 
  mutate(l = estimate-2*se, u = estimate+2*se)

plot_re <- function(dat = r, re_name) {
  re <- dplyr::filter(dat, parameter == re_name)
  re$permit_id <- 1:nrow(re)
  re <- dplyr::left_join(re, unique(dplyr::select(d, permit, permit_id)) )
  library(ggplot2)
  p <- ggplot(re, aes(permit, estimate)) + 
    geom_pointrange(aes(ymin = l, ymax = u)) +
      coord_flip()
    print(p)
}
plot_re(re_name = "b1_k")
plot_re(f, re_name = "b1_b1_k")
plot_re(f, re_name = "sigma1_sigma1_k")
plot_re(f, re_name = "sigma_j")
plot_re(f, re_name = "b_j")
plot_re(f, re_name = "sigma0_sigma")
plot_re(f, re_name = "b0_sigma")
hist(log(log(d$specDiv)))
hist(sqrt(d$specDiv))

head(f)
filter(f, parameters == "sigma_j")
ss <- filter(f, parameters == "sigma1_sigma1_k")
ss$ids <- 1:nrow(ss)
head(ss)
  p <- ggplot(ss, aes(ids, estimate)) + 
    geom_pointrange(aes(ymin = l, ymax = u)) +
      coord_flip()
    print(p)
    library(plyr)

filter(f, parameters == "b_j")
d <- group_by(d, p_holder) %>% 
  mutate(sd_ = sd(revenue))

j <- d %>% filter(permit == "S03E")
ggplot(j, aes(specDiv, revenue, color = log(length) )) + geom_point(alpha = 0.6)
ggplot(j, aes(log(length), revenue, color = log(length) )) + geom_point(alpha = 0.6)
ggplot(d, aes(days, revenue, color = log(length) )) + geom_point(alpha = 0.6) + facet_wrap(~permit, scales = "free")
ggplot(j, aes(days, sd_, color = log(length) )) + geom_point(alpha = 0.6)
ggplot(j, aes(length, sd_, color = log(length) )) + geom_point(alpha = 0.6)
ggplot(j, aes(revenue.prev, sd_, color = log(length) )) + geom_point(alpha = 0.6)
ggplot(j, aes(revenue.prev, revenue, color = log(length) )) + geom_point(alpha = 0.6)
ggplot(j, aes(year, revenue, color = log(length) )) + geom_point(alpha = 0.6)
ggplot(j, aes(year, revenue, color = log(length) )) + geom_point(alpha = 0.6)
summary(d$pholder_id)
length(unique(d$p_holder))
names(d)

  ggplot(d, aes(x = specDiv, y = revenue)) + geom_point(alpha = 0.1) + facet_wrap(~ permit, scales = "free")
  ggsave("salmon-rev-facet.png")
  ggplot(d, aes(x = specDiv, y = sd_)) + geom_point(alpha = 0.1) + facet_wrap(~ permit, scales = "free")
  ggsave("salmon-sigma-facet.png")


b1_k <- filter(r, parameter == "b1_k")
b1_k$permit_id <- 1:nrow(b1_k)
b1_k <- left_join(b1_k, unique(select(d, permit, permit_id)) )
library(ggplot2)
ggplot(b1_k, aes(permit, estimate)) + 
  geom_pointrange(aes(ymin = l, ymax = u)) +
  # geom_point() +
    coord_flip()


##################################################

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

library(rstan)
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


