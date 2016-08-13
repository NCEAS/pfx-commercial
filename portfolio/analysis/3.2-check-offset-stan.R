library(dplyr)
library(ggplot2)
library(rstan)

source("portfolio/analysis/prep-stan-model-matrix.R")

load("portfolio/data-generated/m.rda")

# ------------------------
b <- broom::tidy(m, conf.int = T, estimate.method = "median", rhat = T, ess = T)
if(nrow(filter(b, rhat > 1.05)) > 0) warning("all rhat not <= 1.05")
if(nrow(filter(b, ess < 200)) > 0) warning("all ess not >= 200")
filter(b, grepl("^h1", term))
filter(b, grepl("^g0", term))
filter(b, grepl("^g_k", term))
filter(b, grepl("^b_j", term))
filter(b, grepl("*_tau$", term))

# check mixing:
pdf("portfolio/figs/stan-traces.pdf", width = 10, height = 8)
traceplot(m, pars = b$term[grepl("tau", b$term)], inc_warmup = F)
traceplot(m, pars = b$term[grepl("^b1_strategy\\[", b$term)], inc_warmup = F)
traceplot(m, pars = b$term[grepl("^b2_strategy\\[", b$term)], inc_warmup = F)
traceplot(m, pars = b$term[grepl("^g1_strategy\\[", b$term)], inc_warmup = F)
traceplot(m, pars = b$term[grepl("^g2_strategy\\[", b$term)], inc_warmup = F)
traceplot(m, pars = b$term[grepl("^g0_strategy\\[", b$term)], inc_warmup = F)
traceplot(m, pars = b$term[grepl("^g_k|^b_j|^g0$|^h1$", b$term)], inc_warmup = F)
dev.off()

# ----------------------------------------
# plot coefs:

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
normal_prior <- function(mu = 0, sigma=2) {
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

par(mfrow = c(3, 4))
ignore <- b$term[c(grep("b_j\\[", b$term), grep("g_k\\[", b$term))] %>%
  sapply(beta_samples)

# hist(extract(m)$b0, xlim = c(-5, 5))
# normal_prior(0, 2)

hist(extract(m)$g0, xlim = c(-5, 5))
normal_prior(0, 2)
dev.off()

group_model <- function(par = "coef_g0_strategy", type = "diversity") {
  if (type == "diversity")
    md <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = mean(scaled_spec_div))
  if (type == "revenue")
    md <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = mean(log(revenue)))
  if (type == "days")
    md <- dat %>% group_by(strategy_id, strategy) %>% summarise(mean_group = mean(log_days_permit))

  md <- filter(b, grepl(par, term)) %>% mutate(strategy_id = 1:n()) %>% inner_join(md)

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

pdf("portfolio/figs/stan-group-relationships.pdf", width = 8, height = 10)
p1 <- group_model("coef_g0_strategy")
#p2 <- group_model("coef_b0_strategy")
p3 <- group_model("coef_g1_strategy")
p4 <- group_model("coef_g2_strategy")
p5 <- group_model("coef_b1_strategy")
p6 <- group_model("coef_b2_strategy")
gridExtra::grid.arrange(p1, p3, p4, p5, p6)

p1 <- group_model("coef_g0_strategy", "revenue")
p3 <- group_model("coef_g1_strategy", "revenue")
p4 <- group_model("coef_g2_strategy", "revenue")
p5 <- group_model("coef_b1_strategy", "revenue")
p6 <- group_model("coef_b2_strategy", "revenue")
gridExtra::grid.arrange(p1, p3, p4, p5, p6)

p1 <- group_model("coef_g0_strategy", "days")
p3 <- group_model("coef_g1_strategy", "days")
p4 <- group_model("coef_g2_strategy", "days")
p5 <- group_model("coef_b1_strategy", "days")
p6 <- group_model("coef_b2_strategy", "days")
gridExtra::grid.arrange(p1, p3, p4, p5, p6)
dev.off()

filter(b, grepl("b_j\\[", term) | grepl("g_k\\[", term) | grepl("h1", term)) %>%
  mutate(par = c(paste("[mu]", colnames(mm)),
    paste("[sigma]", colnames(mm2)), "[str. level] h1")) %>%
  ggplot(aes(par, estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
ggsave("portfolio/figs/stan-main-effects-broom.pdf", width = 6.5, height = 6)
