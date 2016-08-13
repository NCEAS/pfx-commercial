# Experiment with models
# by fitting them quickly with lme4

# 2 separate but more complicated models. More interactions:
# - lmer() model to full dataset: interactions allowed to vary?
# - (1) model for mean, (2) model for variance (log sd)
# random effects in strategy?
# days as a covariate?
# 1. What's benefit for individual person
# 2. Benefit across people within a strategy [does this change with div]
# 3. benefit across strategies? [does this change with div]

rm(list = ls())
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)

dat = readRDS(file="../data-generated/cfec-annual-for-modeling.rds")
dat$strategy_taxa <- dat$strategy
dat$strategy <- dat$strategy_gear
nrow(dat)

# Filters: remove people-year combinations making < $1000
dat = dat[which(dat$revenue >= 5000), ]

dat <- group_by(dat, p_holder) %>%
  mutate(nyr = length(unique(year))) %>%
  mutate(range_div = diff(range(specDiv))) %>%
  as_data_frame() %>%
  filter(nyr >= 10) #%>%
  # filter(range_div > 0)
nrow(dat)

dat <- group_by(dat, strategy) %>%
  mutate(range_div = diff(range(specDiv))) %>%
  as_data_frame() %>%
  filter(range_div > 0) # eliminates "pound"
nrow(dat)


# Downsample for speed of testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.1)
set.seed(1)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)


# many different strategies, need to model only most common,
top.strategies = names(rev(sort(table(dat$strategy)))[1:20])
dat = dat[dat$strategy%in%top.strategies, ]
nrow(dat)
sum(!is.na(dat$length))

dat$log_spec_div <- scale(log(dat$specDiv))
dat$scaled_spec_div <- scale((dat$specDiv))
dat$log_length <- scale(log(dat$length + 1))
dat$log_weight <- scale(log(dat$weight + 1))
dat$log_days <- scale(log(dat$days + 1))
dat$scaled_npermit <- scale(dat$npermit)
dat$scaled_year <- scale(dat$year)

png("../figs/pairs.png", width = 10, height = 10, units = "in", res = 72)
pairs(dat[sample(seq_len(nrow(dat)), 3000),
  c("revenue", "scaled_spec_div", "log_days", "scaled_year", "scaled_npermit")],
  col = "#00000020")
dev.off()

library(glmmTMB)
mod <- glmmTMB(log(revenue/1e4) ~ (scaled_spec_div + log_days + scaled_npermit)^2 +
 I(scaled_spec_div^2) + I(log_days^2) + I(scaled_npermit^2) +
  # (1|strategy_gear),
  (1 + scaled_spec_div + log_days + scaled_npermit|strategy),# +
 # (1|p_holder),
  data = dat)
summary(mod)
AIC(mod)

library("brms")
library("rstanarm")
options(mc.cores = parallel::detectCores())
N01 <- normal(0, 1)
mod1 <- stan_lmer(log(revenue/1e4) ~
  scaled_spec_div + log_days + scaled_year + scaled_npermit +
  # I(scaled_spec_div^2) + I(log_days^2) + I(scaled_year^2) + I(scaled_npermit^2) +
  (1 + scaled_spec_div |strategy),
  data = dat, iter = 100, cores = 2, chains = 2, prior = N01)

mod.brms3 <- brm(log(revenue/1e4) ~
  (scaled_spec_div + log_days + scaled_npermit)^2 +
  I(scaled_spec_div^2) + I(log_days^2) + I(scaled_npermit^2) +
  (1 + scaled_spec_div + log_days + scaled_npermit|strategy),
  data = dat, iter = 200, chains = 3,
  prior =
    c(set_prior("student_t(3,0,3)", class = "sd", group = "strategy"),
      set_prior("student_t(3,0,3)", class = "sigma"),
      set_prior("normal(0,1)", class = "b"),
      set_prior("normal(1,10)", class = "Intercept")))
stancode(mod.brms3)
plot(mod.brms3)

library(rstan)
mm <- model.matrix(revenue ~ I(scaled_spec_div^2) + I(scaled_npermit^2) + I(log_days^2) + (scaled_spec_div + scaled_npermit + log_days)^2, data = dat)[,-1]
mm2 <- model.matrix(revenue ~ (scaled_spec_div + scaled_npermit + log_days)^2, data = dat)[,-1]
# dat <- dat[1:5000,]
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
standat <- list(N = nrow(dat),
  J = ncol(mm),
  J_sig = ncol(mm2),
  X_ij = as.matrix(mm),
  X_sig_ij = as.matrix(mm2),
  y = log(dat$revenue/1e4),
  n_strategy = length(unique(dat$strategy)),
  K = 3,
  K_sig = 1,
  strategy_i = as.numeric(as.factor(as.character(dat$strategy))),
  z1_i = as.numeric(dat$scaled_spec_div),
  z2_i = as.numeric(dat$scaled_npermit),
  z3_i = as.numeric(dat$log_days),
  z1_sig_i = as.numeric(dat$scaled_spec_div)
  )
m <- stan("portfolio2.stan", data = standat, iter = 500, chains = 4, pars = c("mu", "sigma"),
  include = FALSE)
traceplot(m)
saveRDS(m,"../data-generated/stan-out.rds")

dat$residuals = residuals(mod)

# 2. model the residuals / variance model
dat$absResid = log(abs(dat$residuals))
mod.cv <- glmmTMB(absResid ~ (scaled_spec_div + log_days + scaled_year + scaled_npermit)^2 +
    #I(scaled_spec_div^2) + I(log_days^2) + I(scaled_year^2) + I(log_npermit^2) +
  # I(scaled_spec_div^2) + I(log_days^2) +
   # I(log_days^2) +
    (1 + scaled_spec_div|strategy),# +
    # (1|strategy_gear),
    # (1 + log_spec_div + log_days|strategy) +
    #(1|p_holder),
  data = dat)

mod.cv <- glmmTMB(absResid ~ scaled_spec_div +
    # I(scaled_spec_div^2) + I(log_days^2) +
    # I(log_days^2) +
    # (1 + scaled_spec_div|strategy) +
    # (1|strategy_gear),
    # (1 + log_spec_div + log_days|strategy) +
    (1|p_holder),
  data = dat)

summary(mod.cv)
AIC(mod.cv)

# 3. model downside risk /
# we can't get situations where people lost money from this
# maybe this is where log-diff model could help too?


# plot random effect intercept vs
strategy.summary = group_by(dat, strategy) %>%
  summarize(sdlogrev = sd(log(revenue)),
    meanrev = mean(log(revenue)),
    specDiv = mean(specDiv),
    mean_npermit = mean(npermit))

# plot random effect intercept vs
person.summary = group_by(dat, p_holder) %>%
  summarize(sdlogrev = sd(log(revenue)),
    cvrev = sd(revenue)/mean(revenue),
    meanrev = mean(log(revenue)),
    specDiv = mean((specDiv)),
    log_days = mean((log_days)),
    mean_npermit = mean(npermit))

strategy.summary$randomInt = ranef(mod)[[1]]$strategy$`(Intercept)` + fixef(mod)[[1]][["(Intercept)"]]
strategy.summary$randomSpec = ranef(mod)[[1]]$strategy$`scaled_spec_div` + fixef(mod)[[1]][["scaled_spec_div"]]
strategy.summary$cv_randomInt = ranef(mod.cv)[[1]]$strategy$`(Intercept)` + fixef(mod.cv)[[1]][["(Intercept)"]]
strategy.summary$cv_randomSpec = ranef(mod.cv)[[1]]$strategy$`scaled_spec_div` + fixef(mod.cv)[[1]][["scaled_spec_div"]]

# strategy.summary$randomSpecSq = ranef(mod)[[1]]$strategy$`I(scaled_spec_div^2)` + fixef(mod)[[1]][["I(scaled_spec_div^2)"]]
# strategy.summary$cv_randomSpecSq = ranef(mod)[[1]]$strategy$`I(scaled_spec_div^2)` + fixef(mod)[[1]][["I(scaled_spec_div^2)"]]

person.summary$randomInt = ranef(mod)[[1]]$p_holder$`(Intercept)` + fixef(mod)[[1]][["(Intercept)"]]
person.summary$cv_randomInt = ranef(mod.cv)[[1]]$p_holder$`(Intercept)` + fixef(mod.cv)[[1]][["(Intercept)"]]

person.summary <- mutate(person.summary,
  spec_div_group = findInterval(specDiv, c(0.99, 1.1, 1.5, 2, 99)))
person.summary <- mutate(person.summary,
  log_days_group = findInterval(log_days, c(-99, -0.5, 0.5, 99)))
ggplot(person.summary, aes(cv_randomInt, randomInt)) + geom_point(alpha=0.2) +
  facet_grid(spec_div_group~log_days_group)

ggplot(person.summary, aes(cvrev, meanrev)) + geom_point(alpha=0.2) +
  facet_grid(log_days_group~spec_div_group)

ggplot(person.summary, aes(log_days_group, cvrev, group = log_days_group)) + geom_boxplot(alpha=0.2)
ggplot(person.summary, aes(spec_div_group, log(cvrev), group = spec_div_group)) + geom_boxplot(alpha=0.2)

ggplot(person.summary, aes(mean_npermit, log(cvrev), colour = log_days)) + geom_point(alpha=0.2) +
  stat_smooth()

q <- filter(dat, strategy == "pot gear")
q = group_by(q, p_holder) %>%
  summarize(sdlogrev = sd(log(revenue)),
    cvrev = sd(revenue)/mean(revenue),
    meanrev = mean(log(revenue)),
    specDiv = mean((specDiv)),
    days = mean((days)),
    mean_npermit = mean(npermit))
ggplot(q, aes(specDiv, log(cvrev), colour = days)) + geom_point(alpha=0.2) +
  stat_smooth()


###################
pdf("../figs/tmb-separate-exploration-nopholder-re.pdf", width = 10, height = 8)

plot_coefficients_tmb <- function(model, pholder_re = TRUE) {
  ci <- confint(model)
  term <- row.names(ci)
  ci <- as_data_frame(ci)  %>% mutate(term = term)
  ci$estimate <- fixef(model)[[1]]
  ci <- filter(ci, !grepl("Intercept", term))

  re_s <- attr(summary(model)$varcor$cond$strategy, "stddev")
  # re_s2 <- attr(summary(model)$varcor$cond$strategy_gear, "stddev")

  if (pholder_re) {
    re_p <- attr(summary(model)$varcor$cond$p_holder, "stddev")
    re <- data.frame(term = c(paste0("re.sd.strategy.", names(re_s)),
        paste0("re.sd.p_holder.", names(re_p))))
    re$estimate <- c(re_s, re_p)
  } else {
    re <- data.frame(term = c(paste0("re.sd.strategy.", names(re_s))))
    re$estimate <- c(re_s)

    # re2 <- data.frame(term = c(paste0("re.sd.strategy_gear.", names(re_s2))))
    # re2$estimate <- c(re_s2)

    # re <- rbind(re, re2)
  }
  re$`2.5 %` <- re$estimate
  re$`97.5 %` <- re$estimate

  ci <- bind_rows(ci, re)
  ggplot(ci, aes(y = estimate, ymax = `2.5 %`, ymin = `97.5 %`, x = term)) +
    geom_pointrange() + coord_flip() + geom_hline(yintercept = 0, lty = 2)
}
plot_coefficients_tmb(mod,F) %>% print
plot_coefficients_tmb(mod.cv, F) %>% print

scaled_spec_div <- seq(min(dat$scaled_spec_div), max(dat$scaled_spec_div), length.out = 50)
pr <- select(strategy.summary, strategy, cv_randomSpec)
pr <- pr[rep(seq_len(nrow(strategy.summary)), each = 50L), ]
pr$b1 <- fixef(mod.cv)$cond[["scaled_spec_div"]]
# pr$b1sq <- fixef(mod.cv)$cond[["I(scaled_spec_div^2)"]]
pr$scaled_spec_div <- rep(scaled_spec_div, nrow(strategy.summary))
pr <- mutate(pr, pre_cv = (b1 + cv_randomSpec) * scaled_spec_div)# + b1sq * scaled_spec_div^2)
ggplot(pr, aes(scaled_spec_div, pre_cv, group = strategy)) + geom_line()

# p <- tidyr::gather(person.summary, model, intercept, randomInt, cv_randomInt) %>%
#   ggplot(aes(specDiv, intercept, color = meanrev)) + geom_point(alpha = 0.2) +
#     facet_wrap(~model, scales = "free_y") +
#     geom_smooth(se = FALSE, color = "red")
# print(p)
#
# p <- tidyr::gather(person.summary, model, intercept, randomInt, cv_randomInt) %>%
#   ggplot(aes(meanrev, intercept, color = specDiv)) + geom_point(alpha = 0.2) +
#     facet_wrap(~model, scales = "free_y") +
#     geom_smooth(se = FALSE, color = "red")
# print(p)

p <- tidyr::gather(strategy.summary, model, intercept, contains("random")) %>%
  ggplot(aes(specDiv, intercept, color = meanrev)) + geom_point() +
    facet_wrap(~model, scales = "free_y") +
    geom_smooth(se = FALSE, color = "red", method = "lm")
print(p)

p <- tidyr::gather(strategy.summary, model, intercept, contains("random")) %>%
  ggplot(aes(meanrev, intercept, color = specDiv)) + geom_point() +
    facet_wrap(~model, scales = "free_y") +
    geom_smooth(se = FALSE, color = "red", method = "lm")
print(p)

dev.off()

person.summary$mean_npermit_cut <- findInterval(person.summary$mean_npermit, c(0, 1.1, 2, 99))
ggplot(person.summary, aes(cv_randomInt, randomInt, color = mean_npermit)) + geom_point() +
  facet_wrap(~mean_npermit_cut)
ggplot(person.summary, aes(mean_npermit, randomInt, color = mean_npermit)) + geom_point()
ggplot(person.summary, aes(mean_npermit, cv_randomInt, color = mean_npermit)) + geom_point()

ggplot(person.summary, aes(specDiv, randomInt, color = mean_npermit)) + geom_point()
ggplot(person.summary, aes(specDiv, cv_randomInt, color = mean_npermit)) + geom_point()

ggplot(strategy.summary, aes(cv_randomInt, randomInt, color = mean_npermit)) + geom_point()
ggplot(strategy.summary, aes(mean_npermit, randomInt, color = mean_npermit)) + geom_point()
ggplot(strategy.summary, aes(mean_npermit, cv_randomInt, color = mean_npermit)) + geom_point()

# Look at the strategy identity of the random effects:
strategy.summary <- arrange(strategy.summary, cv_randomSpec) %>%
  mutate(order = 1:n()) %>%
  mutate(strategy_ordered = reorder(strategy, order))

# p <- person.summary %>%
#   left_join(select(dat, p_holder, strategy) %>% distinct()) %>%
#   left_join(select(strategy.summary, strategy, strategy_ordered)) %>%
#   ggplot(aes(specDiv, cv_randomInt, color = meanrev)) + geom_point(alpha = 0.2) +
#     facet_wrap(~strategy_ordered) +
#     theme(strip.text.x = element_text(size = 4)) +
#     geom_smooth(se = FALSE, color = "red")
# ggsave("../figs/pholder-re-strategies.pdf", width = 26, height = 20, units = "cm")

p <- tidyr::gather(strategy.summary, model, intercept, contains("random")) %>%
  ggplot(aes(intercept, strategy_ordered, color = meanrev)) + geom_point() +
    facet_wrap(~model, scales = "free_x", ncol = 2)
ggsave("../figs/re-strategies.pdf", width = 10, height = 10)

p <- tidyr::gather(strategy.summary, model, intercept, randomSpec, cv_randomSpec) %>%
  ggplot(aes(intercept, strategy_ordered, color = meanrev)) + geom_point() +
    facet_wrap(~model, scales = "free_x", ncol = 1) +
    geom_vline(xintercept = 0, lty = 2)
ggsave("../figs/re-strategies-slim-nopholder-re.pdf", width = 8, height = 10)


# plot fitted vs observed
fitted_mod <- fitted.values(mod)
fitted_mod_cv <- fitted.values(mod.cv)
residuals_cv <- residuals(mod.cv)

# ggplot(dat, aes(x=fitted_mod, y=log(revenue), col = log(revenue))) +
#   facet_wrap(~ strategy, scale="free") +
#   geom_point(alpha = 0.3) +
#   geom_abline(slope=1, intercept=0, color = "grey50")
# ggsave("residuals_rev/fitted_v_observed.pdf", width = 40, height = 40, units = "cm")

# plot fitted vs residuals
ggplot(dat, aes(x=fitted_mod, y=residuals, col = log(revenue))) + facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/fitted_v_residuals.png", width = 40, height = 40, units = "cm")

ggplot(dat, aes(x=fitted_mod_cv, y=residuals_cv, col = log(revenue))) + facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/fitted_v_residuals_cv.png", width = 40, height = 40, units = "cm")

# plot specDiv vs residuals
dat$strategy_ordered <- NULL
dat <- left_join(dat, select(strategy.summary, strategy, strategy_ordered))
p <- ggplot(dat, aes(x=scaled_spec_div, y=residuals, col = log(revenue))) +
  facet_wrap(~strategy_ordered) +
  geom_point(alpha = 0.1) + geom_hline(yintercept=0) +
  ylim(-2,2) +
  theme(strip.text.x = element_text(size = 4)) +
  geom_smooth(se = FALSE, color = "red")
ggsave("residuals_rev/specdiv_v_residuals.pdf", width = 28, height = 22, units = "cm")

p <- ggplot(dat, aes(x=scaled_spec_div, y=residuals)) +
  geom_point(alpha = 0.1) + geom_hline(yintercept=0) +
  ylim(-2,2) +
  geom_smooth(se = FALSE, color = "red")

p <- ggplot(dat, aes(x=specDiv, y=residuals_cv, col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.1) + geom_hline(yintercept=0) +
  geom_smooth(se = FALSE, color = "red")
ggsave("residuals_rev/specdiv_v_residuals_cv.png", width = 40, height = 40, units = "cm")

# plot vessel length vs residuals
# everything looks okay here except for miscellaneous groundfish which curve
# up
ggplot(dat, aes(x=log_length, y=residuals, col = log(revenue))) +
  facet_wrap(~strategy, scale="free_x") +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/length_v_residuals.png", width = 40, height = 40, units = "cm")

ggplot(dat, aes(x=log_length, y=residuals_cv, col = log(revenue))) +
  facet_wrap(~strategy, scale="free_x") +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/length_v_residuals_cv.png", width = 40, height = 40, units = "cm")

# plot days vs residuals
ggplot(dat, aes(x=log_days, y=residuals, col = log(revenue))) +
  facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/days_v_residuals.png", width = 40, height = 40, units = "cm")

ggplot(dat, aes(x=log_days, y=residuals_cv, col = log(revenue))) +
  facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/days_v_residuals_cv.png", width = 40, height = 40, units = "cm")

## ## THINGS I'M SEEING HERE
## # 1. Many of the strategies have no variance w/respect to the predictors. Specifically, permits that are
## #   targeting 1 spp (herring, halibut, sablefish) are essentially ~ 0. We can't do a good job of estimating
## #   random effects on slopes for these strategies, and probably need to just set them to 0.
## #
## # 2. For the differenced model, inclusion of previous revenue may be important as a covariate -- some strategies
## #   have negative relationships, contrary to the "rich get richer" idea.
## #
## # 3. The distribution of the log differenced revenue isn't close to normal -- it's a t distribution with
## #   df ~ 2.4, so changing to an alternate transformation / distribution would help some of the residual patterns
## #   fit.t = fitdistr(testData$rev.pctChange, "t")
## #
## # 4. I found that many of the extreme events occurred for people making very little money. When we
## #   restrict the analysis to people making> 1000 or more (or any floor) the distributions are much more normal
##
## # Can alternate distribution help w/residuals? What this shows is that reducing the extreme tails worsens
## # the model R^2 a bit, but improves the QQ plot and the scale - location plot (eliminating the U or V shape)
## # As an example of the residuals w/permit S15B
## lm.norm = lm(rev.pctChange ~ -1 + specDiv.pctChange+days.pctChange + log(revenue.prev),
##   data = testData[which(testData$strategy=="S15B"),])
## # change to standard normal based on cdf / percentiles.
## fit.t = fitdistr(testData$rev.pctChange, "t")
## testData$rev.pctChange.transform = qnorm(pt(testData$rev.pctChange, df = fit.t$estimate[3]), 0, 1)
## lm.norm2 = lm(rev.pctChange.transform ~ as.factor(year) + specDiv.pctChange+days.pctChange + log(revenue.prev),
##   data = testData[which(testData$strategy=="S15B"),])
