# Experiment with models 
# by fitting them quickly with TMB
rm(list = ls())
library(TMB)
library(dplyr)
library(ggplot2)
compile("revenue_diff.cpp")
dyn.load(dynlib("revenue_diff"))

# LOAD DATA
cfecAnnual.diff = readRDS(file="diff_linearModeling_complete.rds")

# 4200 different strategies, need to model only most common, 
top.strategies = names(rev(sort(table(cfecAnnual.diff$strategy)))[1:100])
cfecAnnual.diff = cfecAnnual.diff[cfecAnnual.diff$strategy%in%top.strategies, ]
# restrict analysis to people who don't change strategies (keeps ~ 90%)
testData = cfecAnnual.diff[which(cfecAnnual.diff$strategy == cfecAnnual.diff$strategy.prev), ]

#P_HOLDER = as.numeric(as.factor(testData$p_holder))
#STRATEGY = as.numeric(as.factor(testData$strategy))
#RESPONSE = log(testData$revenue / testData$revenue.prev)
#PREV_REV = log(testData$revenue.prev)
#SPECDIV = log(testData$specdiv.prev)
#SPEC_DIFF = log(testData$specDiv / testData$specdiv.prev)
#WEIGHT_DIFF = log(testData$weight / testData$weight.prev)
#DAYS_DIFF = log((testData$days + 1) / (testData$days.prev+1))
#OFFSET = WEIGHT_DIFF

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
  
  mm <- model.matrix(~ log_spec_diff, data = x)
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
      sigma1_cov_re_i = x$log_spec_diff,
      offset = x$offset),
    parameters = list(
      b_j = rep(0, n_fe), 
      sigma_j = rep(0, n_fe), 
      b1_k = rep(0, n_k), 
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
    DLL = "revenue_diff")
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

fit_diff <- fit_model(testData, n_obs = 100)

names(fit_diff$summary)
unique(fit_diff$summary$parameter)

d <- group_by(fit_diff$summary, parameter) %>% 
  mutate(parameter_id = 1:n()) %>% 
  as_data_frame()

strategy.levels = lapply(lapply(strsplit(levels(as.factor(fit_diff$data$strategy)), " "), substr, 1, 1), paste, collapse=" ")
strategy.levels = unlist(strategy.levels)
d$taxa=NA
d$taxa[d$parameter=="b1_k"]=strategy.levels
d$taxa[d$parameter=="sigma0_k"]=strategy.levels
d$taxa[d$parameter=="sigma1_k"]=strategy.levels
d$taxa[d$parameter=="sigma2_k"]=strategy.levels

# The general interpretation here is sigma0_k is diversification across strategies,
# sigma1_k is diversification within strategy

# Look at anyone landing halibut/sablefish
p <- filter(d[grep("B|C",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>% 
  ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) + 
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~parameter) + 
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() + ggtitle("Diversification of halibut (B) / sablefish (C)")
print(p)  
ggsave("../figs/tmb-diff-halsab.pdf", width = 7, height = 5)

# Look at anyone landing crabs
p <- filter(d[grep("D|K|T",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>% 
  ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) + 
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~parameter) + 
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() + ggtitle("Diversification of crab fishermen (D,K,T)")
print(p)  
ggsave("../figs/tmb-diff-crab.pdf", width = 7, height = 5)

# Look at anyone landing herring
p <- filter(d[grep("G",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>% 
  ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) + 
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~parameter) + 
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() + ggtitle("Diversification of herring fishermen (G)")
print(p)  
ggsave("../figs/tmb-diff-herr.pdf", width = 7, height = 5)

# Look at anyone landing salmon
p <- filter(d[grep("S",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>% 
  ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) + 
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~parameter) + 
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() + ggtitle("Diversification of salmon fishermen (S)")
print(p)  
ggsave("../figs/tmb-diff-salm.pdf", width = 7, height = 5)

# Look at anyone landing groundfish
p <- filter(d[grep("M",d$taxa),], parameter %in% c("b1_k", "sigma0_k", "sigma1_k", "sigma2_k")) %>% 
  ggplot(aes(parameter_id, estimate, ymin = l, ymax = u, col=taxa)) + 
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~parameter) + 
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() + ggtitle("Diversification of groundfish fishermen (G)")
print(p)  
ggsave("../figs/tmb-diff-gfish.pdf", width = 7, height = 5)

###### ERIC STOPPED HERE

fe_lo <- data.frame(parameter_id = 1:2, par = c("intercept", "species_diversity"))

p <- filter(d, parameter %in% c("b_j", "sigma_j"), parameter_id > 1, parameter_id <= 2) %>% 
  inner_join(fe_lo) %>% 
  ggplot(aes(par, estimate, ymin = l, ymax = u)) + 
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_wrap(~parameter) + 
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()
print(p)  
ggsave("../figs/tmb-fe.pdf", width = 7, height = 5)

names(d)
permits <- unique(select(as.data.frame(fit_diff$data), strategy, permit_id))  %>% 
  mutate(taxa = "all")

permit_diversity <- bind_rows(d) %>% 
  group_by(strategy) %>% 
    summarize(mean_diversity = mean(specDiv))
permits <- left_join(permits, permit_diversity)

p <- filter(d, parameter %in% c("b0_k", "sigma0_k", "b1_b1_k", "sigma1_sigma1_k")) %>% 
  group_by(parameter) %>% 
    mutate(permit_id = 1:n()) %>% 
      left_join(permits) %>% 
  ggplot(aes(permit, estimate, ymin = l, ymax = u, colour = log(mean_diversity))) + 
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_wrap(~parameter) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()
print(p)  
ggsave("../figs/tmb-re.pdf", width = 9.5, height = 12)

p <- filter(d, parameter %in% c("b1_b1_k", "sigma1_sigma1_k")) %>% 
  group_by(parameter) %>% 
    mutate(permit_id = 1:n()) %>% 
      left_join(permits) %>% 
  ggplot(aes(permit, estimate, ymin = l, ymax = u, colour = log(mean_diversity))) + 
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_wrap(~parameter) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()
print(p)  
ggsave("../figs/tmb-re-slopes.pdf", width = 9.5, height = 7)

