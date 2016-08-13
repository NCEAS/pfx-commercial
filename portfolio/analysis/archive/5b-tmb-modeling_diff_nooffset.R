# Experiment with models
# by fitting them quickly with TMB
rm(list = ls())
library(TMB)
library(dplyr)
library(ggplot2)
library(lme4)
compile("revenue_diff_nooffset.cpp")
dyn.load(dynlib("revenue_diff_nooffset"))

# LOAD DATA
cfecAnnual.diff = readRDS(file="../data-generated/cfec-diff-for-modeling.rds")
nrow(cfecAnnual.diff)
# cfecAnnual.diff <- group_by(cfecAnnual.diff, p_holder) %>%
#   mutate(nyr = length(unique(year))) %>%
#   mutate(range_div = diff(range(specDiv))) %>%
#   as_data_frame() %>%
#   filter(nyr >= 10) %>%
#   filter(range_div >= 0)
# nrow(cfecAnnual.diff)


# 4200 different strategies, need to model only most common,
top.strategies = names(rev(sort(table(cfecAnnual.diff$strategy)))[1:100])
cfecAnnual.diff = cfecAnnual.diff[cfecAnnual.diff$strategy%in%top.strategies, ]
# restrict analysis to people who don't change strategies (keeps ~ 90%)
testData = cfecAnnual.diff[which(cfecAnnual.diff$strategy == cfecAnnual.diff$strategy.prev), ]
nrow(testData)

p <- cfecAnnual.diff %>%
  ggplot(aes(log(specDiv/specdiv.prev),
    log(revenue/revenue.prev),
    color = specDiv)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~strategy,scales = "free_y") +
  ggsave("../figs/rev-diff-vs-spec-diff-strategy.pdf",width = 33, height = 24)

p <- cfecAnnual.diff %>%
  group_by(p_holder) %>%
  summarize(mean_spec_div_diff = mean(log(specDiv/specdiv.prev)),
    sd_revenue_diff = sd(log(revenue/revenue.prev)),
    strategy = strategy[1]) %>%
  ggplot(aes(mean_spec_div_diff,
    sd_revenue_diff,
    color = mean_spec_div_diff)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE)# +
  # facet_wrap(~strategy,scales = "free_y")
print(p)
ggsave("../figs/rev-diff-sd-vs-spec-diff-strategy.pdf",width = 33, height = 24)

p <- cfecAnnual.diff %>%
  group_by(strategy) %>%
  summarize(mean_spec_div_diff = mean(log(specDiv/specdiv.prev)),
    sd_revenue_diff = sd(log(revenue/revenue.prev))) %>%
  ggplot(aes(mean_spec_div_diff,
    sd_revenue_diff,
    color = mean_spec_div_diff)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE)# +
# facet_wrap(~strategy,scales = "free_y")
print(p)

# FIT SOME QUICK MODEL WITH LMER
testData$log_spec_div_scaled <- log(testData$specDiv/testData$specdiv.prev)
testData$log_days_scaled <- log((testData$days+1)/(testData$days.prev+1))

# lm1 = lmer(log(revenue/revenue.prev) ~ (1|strategy) + specDiv + offset(log((days+1)/(days.prev+1))),
  # data = testData) # random intercept, fixed slope
lm2 = lmer(log(revenue/revenue.prev) ~ -1 + (-1+log_spec_div_scaled|strategy) + (log_days_scaled+log_spec_div_scaled)^2,
  data = testData) # random slope, fixed intercept
summary(lm2)
lm3 = lmer(abs(log(revenue/revenue.prev) - fitted.values(lm2)) ~ (1+log_spec_div_scaled|strategy) +
  (log_days_scaled+log_spec_div_scaled)^2, data=testData) # ad hoc 2-stage analysis of the variance
summary(lm3)

# library(gamm4)
library(mgcv)
lm2 = gam(log(revenue/revenue.prev) ~ s(log_days_scaled)+s(log_spec_div_scaled),
  data = testData)
lm4 = gam(abs(log(revenue/revenue.prev) - fitted.values(lm2)) ~
    s(log_days_scaled)+s(log_spec_div_scaled) + log_days_scaled:log_spec_div_scaled, data=testData) # ad hoc 2-stage analysis of the variance
summary(lm4)
par(mfrow = c(2,2))
plot(lm2)
plot(lm4)


# lm4 = stan_glmer(log(revenue/revenue.prev) ~
#     (-1+specDiv|strategy) + offset(log((days+1)/(days.prev+1))),
#   data = testData, algorithm = "meanfield") # random slope, fixed intercept
#
# lm4 = stan_glmer(log(revenue/revenue.prev) ~
#     (-1+specDiv|strategy) + offset(log((days+1)/(days.prev+1))),
#   data = testData, algorithm = "meanfield") # random slope, fixed intercept

format_data <- function(x, n_obs = 100) {
  keep <- group_by(x, strategy)  %>% summarize(n = n())  %>% filter(n>=n_obs)
  x <- x[x$strategy %in% keep$strategy, ]
  #x <- x[!is.na(x$length), ]
  x$permit_id <- as.numeric(as.factor(x$strategy))
  x$log_spec_diff <- log(x$specDiv / x$specdiv.prev)
  x$log_spec = log(x$specdiv.prev)
  #x$log_length <- scale(log(x$length + 1))
  x$log_days <- log((x$days + 1) / (x$days.prev+1))
  x$log_weight <- log((x$weight) / (x$weight.prev))
  x$offset = x$log_days#log(x$weight / x$weight.prev)

  mm <- model.matrix(~ (log_spec_diff+log_days)^2, data = x)
  n_k <- max(x$permit_id)
  n_fe <- ncol(mm)

  list(
    raw_data = x,
    data = list(x_ij = mm,
      y_i = log(x$revenue / x$revenue.prev),
      k_i = x$permit_id - 1,
      n_k = n_k,
      n_j = 1, # spec_div column
      b1_cov_re_i = x$log_spec_diff,
      sigma1_cov_re_i = x$log_spec_diff),
      #offset = x$offset),
    parameters = list(
      b_j = rep(0, n_fe),
      sigma_j = rep(0, n_fe),
      # b0_k = rep(0, n_k),
      b1_k = rep(0, n_k),
      # log_b0_sigma = -1,
      log_b1_sigma = -1,
      sigma1_k = rep(0, n_k),
      sigma0_k = rep(0, n_k),
      log_sigma0_sigma = -1,
      log_sigma1_sigma = -1))
}

fit_model <- function(data, n_obs = 100) {
  random <- c("b_j", "sigma_j", "b1_k", "sigma0_k", "sigma1_k")
  d_tmb <- format_data(data, n_obs)
  obj <- MakeADFun(
    data = d_tmb$data,
    parameters = d_tmb$parameters,
    random = random,
    DLL = "revenue_diff_nooffset")
  opt <- nlminb(start=obj$par, objective=obj$fn, gradient=obj$gr)
  sd_report <- sdreport(obj)
  f <- summary(sd_report)
  parameters <- row.names(f)
  f <- as.data.frame(f)
  f$parameter <- parameters
  f <- rename(f, estimate = Estimate, se = `Std. Error`) %>%
    mutate(l = estimate-2*se, u = estimate+2*se)
  list(model = obj, sd_report = sd_report, summary = f, data = d_tmb$raw_data,
    gradient = obj$gr(opt$par))
}

fit_diff <- fit_model(testData, n_obs = 1)

names(fit_diff$summary)
unique(fit_diff$summary$parameter)

d <- fit_diff$summary %>%
  group_by(parameter) %>%
  mutate(parameter_id = 1:n()) %>%
  as_data_frame()

strategy.levels = lapply(lapply(strsplit(levels(as.factor(fit_diff$data$strategy)), " "), substr, 1, 1), paste, collapse=" ")
strategy.levels = unlist(strategy.levels)
d$taxa=NA
d$taxa[d$parameter=="b1_b1_k"]=strategy.levels
d$taxa[d$parameter=="sigma0_k"]=strategy.levels
d$taxa[d$parameter=="sigma1_sigma1_k"]=strategy.levels
# d$taxa[d$parameter=="sigma2_k"]=strategy.levels

# examine a possible interaction :
ggplot(filter(testData, log(specDiv/specdiv.prev) < -0.2), aes(log(days+1), log(revenue/revenue.prev), color = log(specDiv/specdiv.prev))) + geom_point(alpha = 0.1) + ylim(-10, 10)
ggplot(filter(testData, log(specDiv/specdiv.prev) >0.2), aes(log(days+1), log(revenue/revenue.prev), color = log(specDiv/specdiv.prev))) + geom_point(alpha = 0.1) + ylim(-10, 10)

# The general interpretation here is sigma0_k is diversification across strategies,
# sigma1_k is diversification within strategy

## # Look at anyone landing halibut/sablefish
## p <- filter(d[grep("B|C",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>%
##   ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) +
##   geom_pointrange(position = position_dodge(width = 0.4)) +
##   facet_wrap(~parameter) +
##   geom_hline(yintercept = 0, lty = 2) +
##   coord_flip() + ggtitle("Diversification of halibut (B) / sablefish (C)")
## print(p)
## ggsave("../figs/tmb-diff-halsab.pdf", width = 7, height = 5)
##
## # Look at anyone landing crabs
## p <- filter(d[grep("D|K|T",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>%
##   ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) +
##   geom_pointrange(position = position_dodge(width = 0.4)) +
##   facet_wrap(~parameter) +
##   geom_hline(yintercept = 0, lty = 2) +
##   coord_flip() + ggtitle("Diversification of crab fishermen (D,K,T)")
## print(p)
## ggsave("../figs/tmb-diff-crab.pdf", width = 7, height = 5)
##
## # Look at anyone landing herring
## p <- filter(d[grep("G",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>%
##   ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) +
##   geom_pointrange(position = position_dodge(width = 0.4)) +
##   facet_wrap(~parameter) +
##   geom_hline(yintercept = 0, lty = 2) +
##   coord_flip() + ggtitle("Diversification of herring fishermen (G)")
## print(p)
## ggsave("../figs/tmb-diff-herr.pdf", width = 7, height = 5)
##
## # Look at anyone landing salmon
## p <- filter(d[grep("S",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>%
##   ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) +
##   geom_pointrange(position = position_dodge(width = 0.4)) +
##   facet_wrap(~parameter) +
##   geom_hline(yintercept = 0, lty = 2) +
##   coord_flip() + ggtitle("Diversification of salmon fishermen (S)")
## print(p)
## ggsave("../figs/tmb-diff-salm.pdf", width = 7, height = 5)
##
## # Look at anyone landing groundfish
## p <- filter(d[grep("M",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>%
##   ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) +
##   geom_pointrange(position = position_dodge(width = 0.4)) +
##   facet_wrap(~parameter) +
##   geom_hline(yintercept = 0, lty = 2) +
##   coord_flip() + ggtitle("Diversification of groundfish fishermen (G)")
## print(p)
## ggsave("../figs/tmb-diff-gfish.pdf", width = 7, height = 5)

# Plot slope in mean versus slope in coefficient of variability
names(d)
strategy <- unique(dplyr::select(as.data.frame(fit_diff$data), strategy, permit_id))

strategy_diversity <- group_by(fit_diff$data, strategy) %>%
  summarize(mean_diversity = mean(specDiv),
    mean_rev = mean(revenue),
    sd_rev = sd(revenue))
strategy <- left_join(strategy, strategy_diversity)
# summarize strategies by taxa / 1st letter
strategy$taxa = unlist(lapply(lapply(strsplit(strategy$strategy, " "),
  substr, 1, 1), paste, collapse=":"))

b0_sigma = dplyr::select(fit_diff$summary, parameter, estimate) %>%
  filter(parameter=="sigma0_k") %>%
  mutate(permit_id = 1:n(), b0_sigma_est=estimate) %>%
  dplyr::select(permit_id, b0_sigma_est)
strategy <- left_join(strategy, b0_sigma)

b1_mu = dplyr::select(fit_diff$summary, parameter, estimate) %>%
  filter(parameter=="b1_b1_k") %>%
  mutate(permit_id = 1:n(), b1_mu_est=estimate) %>%
  dplyr::select(permit_id, b1_mu_est)
strategy <- left_join(strategy, b1_mu)

b1_sigma = dplyr::select(fit_diff$summary, parameter, estimate) %>%
  filter(parameter=="sigma1_sigma1_k") %>%
  mutate(permit_id = 1:n(), b1_sigma_est=estimate) %>%
  dplyr::select(permit_id, b1_sigma_est)
strategy <- left_join(strategy, b1_sigma)

strategy_grouped = rbind(cbind(strategy[grep("M",strategy$taxa),], group="Groundfish"),
  cbind(strategy[grep("S",strategy$taxa),], group="Salmon"),
  cbind(strategy[grep("B|C",strategy$taxa),], group="Halibut-Sablefish"),
  cbind(strategy[grep("K|T|D",strategy$taxa),], group="Crab"),
  cbind(strategy[grep("G",strategy$taxa),], group="Herring"))

p <- group_by(strategy_grouped) %>%
ggplot(aes(x = b1_mu_est, y = b0_sigma_est, col = mean_diversity)) +
  geom_text(aes(label=strategy), size=3) +
  facet_wrap(~group, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("Random intercept in variance vs random slope in mean") +
  ylab("Variabiliy in revenue across strategy") +
  xlab("Benefit of diversity within strategies")
print(p)
ggsave("../figs/sigmaB0_vs_muB1.pdf", width = 7, height = 5)

p <- group_by(strategy_grouped) %>%
  ggplot(aes(x = mean_diversity, y = b0_sigma_est, col = b1_mu_est)) +
  geom_text(aes(label=taxa), size=3) +
  # facet_wrap(~group, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("Random intercept in variance vs mean diversity") +
  ylab("Variabiliy in revenue across strategy") +
  xlab("Mean strategy diversity")
print(p)

p <- group_by(strategy_grouped) %>%
  ggplot(aes(x = mean_diversity, y = b1_mu_est, col = b0_sigma_est)) +
  geom_text(aes(label=taxa), size=3) +
  facet_wrap(~group, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("random slope in mean vs mean diversity") +
  ylab("Benefit of diversity within strategies") +
  xlab("Mean strategy diversity")
print(p)

p <- group_by(strategy_grouped) %>%
  ggplot(aes(x = mean_diversity, y = b1_sigma_est, col = b0_sigma_est)) +
  geom_text(aes(label=taxa), size=3) +
  geom_smooth(method = "lm") #+
  # facet_wrap(~group, scales = "free") #+
 # ggtitle("random slope in sigma vs mean diversity") +
 #ylab("Benefit of diversity for sigma") +
 # xlab("Mean strategy diversity")
print(p)

p <- group_by(strategy_grouped) %>%
  ggplot(aes(x = log(mean_rev), y = b1_sigma_est, col = mean_diversity)) +
  geom_text(aes(label=taxa), size=3) +
  geom_smooth(method = "lm") #+
  # facet_wrap(~group, scales = "free") #+
# ggtitle("random slope in sigma vs mean diversity") +
#ylab("Benefit of diversity for sigma") +
# xlab("Mean strategy diversity")
print(p)
##########
# and by mean revenue
##########
p <- group_by(strategy_grouped) %>%
  ggplot(aes(x = b1_mu_est, y = b0_sigma_est, col = mean_rev)) +
  geom_text(aes(label=taxa), size=3) +
  # facet_wrap(~group, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("Random intercept in variance vs random slope in mean") +
  ylab("Variabiliy in revenue across strategy") +
  xlab("Benefit of diversity within strategies")
print(p)

p <- group_by(strategy_grouped) %>%
  ggplot(aes(x = log(mean_rev), y = b0_sigma_est, col = b1_mu_est)) +
  geom_text(aes(label=taxa), size=3) +
  facet_wrap(~group, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("Random intercept in variance vs mean_rev") +
  ylab("Variabiliy in revenue across strategy")
print(p)

p <- group_by(strategy_grouped) %>%
  ggplot(aes(x = log(mean_rev), y = b1_mu_est, col = b0_sigma_est)) +
  geom_text(aes(label=taxa), size=3) +
  facet_wrap(~group, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("random slope in mean vs mean_rev") +
  ylab("Benefit of diversity within strategies")
print(p)

p <- group_by(strategy_grouped) %>%
  ggplot(aes(x = log(mean_rev), y = mean_diversity, col = b0_sigma_est)) +
  geom_text(aes(label=taxa), size=3) +
  facet_wrap(~group, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("mean_diversity vs mean_rev")
print(p)
