library(dplyr)
library(ggplot2)
source("portfolio/analysis/cull-dat.R")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

nrow(dat)
sum(is.na(dat$length))
# dat<-dat[!is.na(dat$length), ]
nrow(dat)

# dat <- group_by(dat, strategy) %>%
#   mutate(ntotal = n(), ngreater1 = sum(specDiv>1.1), nratio = ngreater1/ntotal) %>%
#   as_data_frame() %>%
#   mutate(spec_div_all_1 = ifelse(nratio < 0.1, 1L, 0L)) %>%
#   select(-ngreater1)
# # dat <- dat %>% filter(spec_div_all_1 == 0L)

# Downsample for speed of testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.20)
set.seed(123)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

dat <- dat %>% group_by(strategy) %>% mutate(n=n()) %>% filter(n > 30)
nrow(dat)

g <- ggplot(dat, aes(scaled_spec_div, log(revenue/1e5), colour = log_days_permit)) +
  geom_point(alpha=0.1) +
  facet_wrap(~strategy)
ggsave("portfolio/figs/stan-specdiv-vs-revenue.pdf", width = 9, height = 9)

# median(log(dat$revenue/1e5))
# median(p$b0[[1]])
# median(p$b0[[1]]) + filter(b, grepl("b0_str", term))$estimate %>% exp

# ggplot(dat, aes(log_days_permit, log(revenue), colour = scaled_spec_div)) +
#   geom_point(alpha=0.1) +
#   facet_wrap(~strategy)
# 
# ggplot(dat, aes(log_length, log(revenue), colour = scaled_spec_div)) +
#   geom_point(alpha=0.1) +
#   facet_wrap(~strategy)

dat$p_holder_strategy <- paste(dat$p_holder, dat$strategy, sep = ":")
dat$strategy_id <- as.numeric(as.factor(as.character(dat$strategy)))
dat$person_id <- as.numeric(as.factor(as.character(dat$p_holder_strategy)))

dat$log_spec_div <- scale2(log(dat$specDiv))
dat$scaled_spec_div <- scale2(dat$specDiv)
dat$log_length <- scale2(log(dat$length))
dat$log_days <- scale2(log(dat$days + 1))
dat$log_npermit <- scale2(log(dat$npermit))
dat$log_days_permit <- scale2(log(dat$days_permit+1))

dat <- dat %>% group_by(strategy) %>% mutate(strategy_mean_div = mean(scaled_spec_div))

mm <- model.matrix(revenue ~ 
  I(scaled_spec_div^2) + I(log_days_permit^2) + I(log_length^2) +
  (scaled_spec_div + log_days_permit + log_length)^2, data = dat)[,-1]
# mm <- mm/3
mm2 <- model.matrix(revenue ~ (scaled_spec_div + log_days_permit + log_length)^2, 
  data = dat)[,-1]
# mm2 <- mm2/3

standat <- list(N = nrow(dat),
  J = ncol(mm),
  X_ij = as.matrix(mm),

  K = ncol(mm2),
  X_sigma_ik = as.matrix(mm2),

  y_i = log(dat$revenue/1e5),

  n_strategy = length(unique(dat$strategy_id)),
  strategy_i = dat$strategy_id,
  n_person = length(unique(dat$person_id)),
  person_i = dat$person_id,

  b1_cov_i = dat$scaled_spec_div,
  # b2_cov_i = dat$log_days_permit,
  g1_cov_i = dat$scaled_spec_div,

  mean_div = dat$strategy_mean_div
  )

# strategy deviation IDs:
sink("portfolio/figs/stan-strategy-ids.txt")
sapply(seq_along(unique(dat$strategy)), function(i) {
  cbind(i,
    dat[as.numeric(as.factor(as.character(dat$strategy))) == i,
    c("strategy")][[1]][1])
}) %>% t
sink()

# ----------------------------------------
# custom tighter inits:
beta_init <- function() runif(standat$J, -0.1, 0.1)
sigma_init <- function() runif(standat$K, -0.1, 0.1)
# start at 0 in case no strategy-level variation:
dev_str_init <- function() runif(standat$n_strategy, 0, 0)
dev_per_init <- function() runif(standat$n_person, 0, 0)
tau_init <- function() runif(1, 0.3, 0.6)

init_fun <- function(seed = sample(seq_len(1e4), 1)) {
# init_fun <- function(seed = 1) {
  set.seed(seed)
  list(
    b0 = rnorm(1, mean(standat$y), 0.2),
    b0_strategy = dev_str_init(),
    b0_pholder = dev_per_init(),
    b0_strategy_tau = tau_init(),
    b0_pholder_tau = tau_init(),
    b_j = beta_init(),
    b1_strategy = dev_str_init(),
    b1_strategy_tau = tau_init(),
    b2_strategy = dev_str_init(),
    b2_strategy_tau = tau_init(),
    g0 = beta_init()[1],
    g0_strategy = dev_str_init(),
    g0_strategy_tau = tau_init(),
    g_k = sigma_init(),
    g1_strategy = dev_str_init(),
    g1_strategy_tau = tau_init())
} 

# ----------------------------------------
# fit model
mean(standat$y_i)
m <- stan("portfolio/analysis/portfolio2.stan",
  data = c(standat, b0_prior_mean=0,b0_prior_sd=10,b0_strategy_tau_prior_sd=2),
  iter = 80, chains = 2,
  pars = c("mu", "sigma", "b0_pholder"), include = FALSE, init = init_fun)
# mod <- brms::make_stancode(log(revenue) ~ scaled_spec_div*log_days_permit +
#     I(log_days_permit^2) + I(scaled_spec_div^2) +
#     (1 |strategy), data = dat)

library(lme4)
m2 <- lmer(log(revenue/1e5)~
  I(scaled_spec_div^2) + I(log_days_permit^2) + I(log_length^2) +
  (scaled_spec_div + log_days_permit + log_length)^2 + (1+scaled_spec_div|strategy_id) + (1|person_id), data = dat)
qq <- broom::tidy(m) %>%
  filter(grepl("b0_strategy\\[", term)) %>%
  select(estimate)
b0 <- broom::tidy(m) %>%
  filter(grepl("b0$", term)) %>%
  select(estimate)
qq2 <- ranef(m2)$strategy_id[,1]
plot(qq2, qq[[1]])
exp(coef(m2)$strategy_id$`(Intercept)`)
# exp(fixef(m2)[[1]] + ranef(m2)$strategy_id$`(Intercept)`)
exp(b0$estimate + qq$estimate)
qq4 <- group_by(dat, strategy_id) %>% summarise(m = median(log(revenue/1e5)))
qq5 <- qq4$m %>% exp
plot(qq5, exp(coef(m2)$strategy_id$`(Intercept)`))

sink("portfolio/figs/stan-output.txt")
print(m)
sink()
saveRDS(m, file = "portfolio/data-generated/stan-july31.rds")
m <- readRDS("portfolio/data-generated/stan-july31.rds")

# ----------------------------------------
# check mixing:
pdf("portfolio/figs/stan-traces.pdf", width = 10, height = 8)
traceplot(m, pars = c(
  "b0_strategy_tau",
  "b0_pholder_tau",
  "b1_strategy_tau",
  "g0_strategy_tau",
  "g1_strategy_tau"), inc_warmup = T)
traceplot(m, pars = c("b0_strategy"), inc_warmup = T)
# traceplot(m, pars = c("b0_dev_per"), inc_warmup = F)
traceplot(m, pars = c("b1_strategy"), inc_warmup = T)
# traceplot(m, pars = c("b2_dev"), inc_warmup = T)
traceplot(m, pars = c("g0_strategy"), inc_warmup = T)
traceplot(m, pars = c("g1_strategy"), inc_warmup = T)
colnames(mm2)
traceplot(m, pars = c("g0", "g_k", "h1"), inc_warmup = T)
colnames(mm)
traceplot(m, pars = c("b0", "b_j"), inc_warmup = T)
dev.off()

# ----------------------------------------
# plot coefs:
b <- broom::tidy(m, conf.int = T, estimate.method = "median", rhat = T, ess = T)
pdf("portfolio/figs/stan-coefs.pdf", width = 8, height = 11)
filter(b, !grepl("_strategy\\[", term)) %>%
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high, colour = rhat)) + 
  geom_pointrange() + coord_flip() + geom_hline(yintercept = 0, lty = 2)

filter(b, grepl("_strategy\\[", term)) %>%
  mutate(
    strategy_id = as.numeric(sub("[a-z_0-9]+\\[([0-9]+)\\]", "\\1", term)),
    coef = sub("\\[[0-9]+\\]", "", term)) %>%
  inner_join(unique(select(dat, strategy, strategy_id))) %>%
  ggplot(aes(strategy, estimate, ymin = conf.low, ymax = conf.high, colour = rhat)) + 
  geom_pointrange() + coord_flip()  + geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~coef)
dev.off()

# ----------------------------------------
# check priors/posteriors:
dt2 <- function(x, df, mu, a) 1/a * dt((x - mu)/a, df)
t_prior <- function() {
  x <- seq(0, 10, length.out = 100)
  par(new = T)
  dens <- dt2(x, 3, 0, 2)
  plot(x, dens, type = "l", ylim = c(0, max(dens)), xlim = c(0, 5))
}
tau_samples <- function(p) {
  hist(extract(m)[[p]], xlim = c(0, 5), main = p)
  t_prior()
}
normal_prior <- function(mu = 0, sigma=1) {
  x <- seq(-5, 5, length.out = 100)
  par(new = T)
  dens <- dnorm(x, mu, sigma)
  plot(x, dens, type = "l", ylim = c(0, max(dens)), xlim = c(-5, 5))
}
beta_samples <- function(p, sigma=1) {
  column <- as.numeric(gsub("[a-z0-9_]*\\[([0-9])\\]", "\\1", p))
  term <- gsub("\\[[0-9]\\]", "", p)
  hist(extract(m)[[term]][,column], xlim = c(-5, 5), main = p)
  normal_prior(sigma=sigma)
}

pdf("portfolio/figs/stan-priors-posteriors.pdf", width = 10, height = 9)
par(mfrow = c(2, 3))
ignore <- sapply(b$term[grep("tau", b$term)], tau_samples)

par(mfrow = c(5, 4))
ignore <- b$term[c(grep("b_j\\[", b$term), grep("g_k\\[", b$term))] %>%
  sapply(beta_samples)

hist(extract(m)$b0, xlim = c(-5, 5))
normal_prior(0, 10)

hist(extract(m)$sigma0, xlim = c(-5, 5))
normal_prior(0, 3)
dev.off()

group_model <- function(par = "g0_strategy", type = "diversity") {
  if (type == "diversity")
    md <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = mean(scaled_spec_div))
  if (type == "revenue")
    md <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = mean(log(revenue)))
  if (type == "days")
    md <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = mean(log_days_permit))
  md <- filter(b, grepl(par, term)) %>% mutate(strategy_id = 1:n()) %>% inner_join(md)
  if (par == "g0_strategy") {
    h1 <- filter(b, term == "h1")
    md2 <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = mean(scaled_spec_div))
    md$estimate <- md$estimate + md2$mean_group * h1$estimate
  }
  p <- ggplot(md, aes(mean_group, estimate, weight = 1/std.error)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_text(aes(label = strategy), size = 2) +
    ggtitle(paste(par, type, sep = " vs. ")) +
    stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, se = F)
    # stat_smooth(method = "lm", se = F)
  print(summary(lm(estimate~mean_group, data = md, weights = 1/std.error)))
  print(summary(lm(estimate~poly(mean_group,2), data = md, weights = 1/std.error)))
  p
}

# md <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = mean(scaled_spec_div))
#   filter(b, grepl("sigma0_dev", term)) %>% mutate(strategy_id = 1:n()) %>% inner_join(md)
#   filter(b, grepl("b1_sig_dev", term)) %>% mutate(strategy_id = 1:n()) %>% inner_join(md) %>%
#     select(estimate, std.error, strategy) %>% arrange(estimate)


pdf("portfolio/figs/stan-group-relationships.pdf", width = 8, height = 8)
p1 <- group_model("g0_strategy")
p2 <- group_model("b0_strategy")
p3 <- group_model("g1_strategy")
p4 <- group_model("b1_strategy")
gridExtra::grid.arrange(p1, p2, p3, p4)

p1 <- group_model("g0_strategy", "revenue")
p2 <- group_model("b0_strategy", "revenue")
p3 <- group_model("g1_strategy", "revenue")
p4 <- group_model("b1_strategy", "revenue")
gridExtra::grid.arrange(p1, p2, p3, p4)

p1 <- group_model("g0_strategy", "days")
p2 <- group_model("b0_strategy", "days")
p3 <- group_model("g1_strategy", "days")
p4 <- group_model("b1_strategy", "days")
gridExtra::grid.arrange(p1, p2, p3, p4)
dev.off()

filter(b, grepl("b_j\\[", term) | grepl("g_k\\[", term) | grepl("h1", term)) %>%
  mutate(par = c(paste("[mu]", colnames(mm)), paste("[sigma]", colnames(mm2)), "[str. level] h1")) %>%
   ggplot(aes(par, estimate)) +
   geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
   geom_hline(yintercept = 0, lty = 2) +
   coord_flip()
ggsave("portfolio/figs/stan-main-effects.pdf", width = 6.5, height = 6)

h1 <- extract(m)$h1
m_div <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = strategy_mean_div[1])
x <- sort(m_div$mean_group)
g0 <- extract(m)$g0
g0_strategy <- extract(m)$g0_strategy
sc_mean <- mean(dat$specDiv)
sc_sd <- sd(dat$specDiv)

pdf("portfolio/figs/stan-trial-group-predictor.pdf", width = 5, height = 5)
plot(x, exp(x * h1[1] + g0[1]), type = "n", ylim = c(.3, 0.7), xaxt = "n")
axis(at = (c(1,2,3,4,5)-sc_mean)/(2*sc_sd), side = 1, labels = c(1, 2, 3, 4, 5))
for (i in 1:400) {
  lines(x, exp(x * h1[i] + g0[i]), col = "#00000010")
}
med <- sapply(seq_along(x), function(i) median((x[i] * h1 + g0)))
lines(x, exp(med), col = "red", lwd = 2)
with(m_div, text(mean_group, exp(mean_group * median(h1) + median(g0) + apply(g0_strategy, 2, median)),
                 labels = strategy, col = "#00000060", cex = 0.8))
dev.off()

st_coefs <- filter(b, grepl("_strategy\\[", term)) %>%
  mutate(strategy_id = as.numeric(sub("[a-z_0-9]+\\[([0-9]+)\\]", "\\1", term)),
    coef = sub("\\[[0-9]+\\]", "", term)) %>%
  inner_join(unique(select(dat, strategy, strategy_id))) %>% 
  select(estimate, strategy, coef) %>% tidyr::spread(coef, estimate)
st_coefs$b0_strategy <- st_coefs$b0_strategy + filter(b, term == "b0")$estimate
which(colnames(mm) == "scaled_spec_div")
st_coefs$b1_strategy <- st_coefs$b1_strategy + filter(b, term == "b_j[4]")$estimate
which(colnames(mm2) == "scaled_spec_div")
st_coefs$g1_strategy <- st_coefs$g1_strategy + filter(b, term == "g_k[1]")$estimate
st_coefs$g0_strategy <- st_coefs$g0_strategy + filter(b, term == "g0")$estimate

st <- readxl::read_excel("data/df_output.xlsx") %>% 
  rename(strategy = strategy.permit) %>%
    inner_join(st_coefs)

openxlsx::write.xlsx(st, file = "portfolio/figs/strategies.xlsx")
