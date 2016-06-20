# Experiment with models
# by fitting them quickly with TMB
rm(list = ls())
library(TMB)
library(dplyr)
library(ggplot2)
compile("revenue7.cpp")
dyn.load(dynlib("revenue7"))

dm <- readRDS("../data-generated/cfec-for-modeling.rds")
nrow(dm)

strategies <- readr::read_csv("../data-generated/strategies.csv")
strategies <- strategies[1:100, ] %>% rename(strategy = Var1)
dm <- left_join(strategies,dm)
nrow(dm)

# for now only model those strategies where there is sufficient contrast in diversity
dm <- dm %>% group_by(strategy) %>%
  # mutate(mean_spec_div = mean(specDiv, na.rm = TRUE)) %>%
  mutate(iqr_spec_div = IQR(specDiv, na.rm = TRUE)) %>%
    as_data_frame

# dm <- dm %>% mutate(strategy_diverse = ifelse(mean_spec_div >= 1.05, strategy, NA))
dm <- dm[dm$iqr_spec_div >= 0.10, ]
nrow(dm)

# dm <- group_by(dm, strategy) %>% mutate(n = n()) %>% subset(n>=50) %>%
#   as_data_frame()
# nrow(dm)
dm <- dm[!is.na(dm$length), ]
nrow(dm)

# assign some strategy IDs for the purpose of identifying random effects
strategy_ids  <- select(dm, strategy)  %>%
  unique %>% mutate(strategy_id = 1:n())
pholder_ids <- select(dm, p_holder) %>%
  unique %>% mutate(pholder_id = 1:n())
dm <- inner_join(dm, strategy_ids)  %>% inner_join(pholder_ids)
nrow(dm)

format_data <- function(x) {
  x$log_spec_div <- scale(log(x$specDiv))
  x$log_length <- scale(log(x$length + 1))
  x$log_weight <- scale(log(x$weight + 1))
  x$log_days <- scale(log(x$days + 1))
  mm <- model.matrix(~ log_spec_div + log_days + log_length, data = x)
  n_pholder_k <- max(x$pholder_id)
  n_strategy_k <- max(x$strategy_id)
  n_fe <- ncol(mm)
  # re_estimate = unique(x[,c("strategy","strategy_diverse")])$strategy_diverse

  list(
    raw_data = x,

    data = list(x_ij = mm,
      y_i = log(x$revenue),
      pholder_k_i = x$pholder_id - 1,
      strategy_k_i = x$strategy_id - 1,
      n_pholder_k = n_pholder_k,
      n_strategy_k = n_strategy_k,
      n_j = 1, # spec_div column
      b1_cov_re_i = x$log_spec_div,
      sigma1_cov_re_i = x$log_spec_div),

    parameters = list(
      b_j = rep(0, n_fe),
      sigma_j = rep(0, n_fe),

      log_b0_pholder_tau = -1,
      log_b0_strategy_tau = -1,
      log_b1_tau = -1,

      b0_pholder_k = rep(0, n_pholder_k),
      b0_strategy_k = rep(0, n_strategy_k),
      b1_k = rep(0, n_strategy_k),

      log_sigma0_pholder_tau = -1,
      log_sigma0_strategy_tau = -1,
      log_sigma1_tau = -1,

      sigma0_pholder_k = rep(0, n_pholder_k),
      sigma0_strategy_k = rep(0, n_strategy_k),
      sigma1_k = rep(0, n_strategy_k))

    # map = list(
    #   b1_k = re_estimate,
    #   sigma1_k = re_estimate
    #   )

    )
}

fit_model <- function(data) {
  random <- c("b0_pholder_k", "b0_strategy_k", "b_j",
    "sigma_j", "b1_k", "sigma0_pholder_k", "sigma0_strategy_k", "sigma1_k")
  d_tmb <- format_data(data)
  obj <- MakeADFun(
    data = d_tmb$data,
    parameters = d_tmb$parameters,
    random = random,
    DLL = "revenue7")
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

m <- fit_model(dm)
m$gradient
m$sd_report

d <- m$summary %>%
  group_by(parameter) %>%
  mutate(parameter_id = 1:n()) %>%
    as_data_frame()

fe_lo <- data.frame(parameter_id = 1:4,
  par = c("intercept", "species_diversity", "log_days", "log_length"))

p <- filter(d, parameter %in% c("b_j", "sigma_j"), parameter_id > 1, parameter_id <= 4) %>%
  inner_join(fe_lo) %>%
  ggplot(aes(par, estimate, ymin = l, ymax = u)) +
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_wrap(~parameter) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()
print(p)
ggsave("../figs/tmb-fe.pdf", width = 7, height = 5)

# plot_re(ns_m$summary, name = "b_j", raw_data = ns_m$data, add_permits = F, ind = 2:4)

# ids <- unique(select(as.data.frame(m$data), strategy, strategy_id, p_holder, pholder_id))
ids <- unique(select(as.data.frame(m$data), strategy, strategy_id))
strategy_diversity <- bind_rows(dm) %>%
  group_by(strategy) %>%
    summarize(mean_diversity = mean(specDiv, na.rm = TRUE))
ids <- left_join(ids, strategy_diversity)

p <- filter(d, parameter %in%
  c("b0_strategy_k", "sigma0_strategy_k", "b1_b1_k", "sigma1_sigma1_k")) %>%
# p <- filter(d, parameter %in% c("b1_b1_k", "sigma1_sigma1_k")) %>%
  group_by(parameter) %>%
    mutate(strategy_id = 1:n()) %>%
      left_join(ids)

junk <- p %>% as_data_frame %>% filter(parameter == "sigma1_sigma1_k") %>%
  mutate(strategy_ordered = reorder(strategy, -estimate)) %>%
  select(strategy, strategy_ordered)
p <- inner_join(p, junk)

p1 <- ggplot(p, aes(strategy_ordered, estimate, ymin = l, ymax = u, colour = log(mean_diversity))) +
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_wrap(~parameter, ncol = 4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()
print(p1)
ggsave("../figs/tmb-re.pdf", width = 16, height = 13)

j <- filter(d, parameter %in% c("b0_strategy_k", "sigma0_strategy_k")) %>%
  group_by(parameter) %>%
    mutate(strategy_id = 1:n()) %>%
      left_join(ids) %>%
        as_data_frame() %>%
          na.omit()
ggplot(j, aes(log(mean_diversity), estimate, label = strategy, ymin = l, ymax = u)) + geom_pointrange(alpha=.4) +
  facet_grid(~parameter, scales = "free")
  # geom_text(aes(label = strategy) )
ggsave("../figs/tmb-re-int-vs-diversity.pdf", width = 13, height = 10)

p <- reshape2::dcast(j,  strategy ~ parameter, value.var = "estimate") %>%
  ggplot(aes(sigma0_strategy_k, b0_strategy_k, label = strategy)) + geom_point() +
    geom_text()
p
ggsave("../figs/tmb-re-int-vs.pdf", width = 10, height = 9)

j1 <- filter(d, parameter %in% c("b1_b1_k", "sigma1_sigma1_k")) %>%
  group_by(parameter) %>%
    mutate(strategy_id = 1:n()) %>%
      left_join(ids) %>%
        as_data_frame() %>%
          na.omit()
reshape2::dcast(j1, strategy  ~ parameter, value.var = "estimate") %>%
  ggplot(aes(sigma1_sigma1_k, b1_b1_k, label = strategy)) + geom_point() +
    geom_text()
ggsave("../figs/tmb-re-slope-vs.pdf", width = 10, height = 10)

ggplot(j1, aes(mean_diversity, estimate)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~parameter) +
geom_text(aes(label = strategy), size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2)
ggsave("../figs/tmb-re-slope-vs-diversity.pdf", width = 15, height = 9)

##### p_holders

ids <- unique(select(as.data.frame(m$data), strategy, strategy_id, p_holder, pholder_id))
pholder_diversity <- bind_rows(dm) %>%
  group_by(pholder_id) %>%
    summarize(mean_diversity = mean(specDiv, na.rm = TRUE))
ids <- left_join(ids, pholder_diversity)

p <- filter(d, parameter %in%
  c("sigma0_pholder_k")) %>%
# p <- filter(d, parameter %in% c("b1_b1_k", "sigma1_sigma1_k")) %>%
  group_by(parameter) %>%
    mutate(pholder_id = 1:n()) %>%
      left_join(ids)

ggplot(p, aes(mean_diversity, estimate)) + geom_point(alpha = 0.1)
ggsave("../figs/tmb-re-pholder-sigma0-vs-diversity.pdf", width = 7, height = 7)

group_by(dm, p_holder) %>%
  summarise(sp = mean(specDiv), cv=sd(log(revenue))) %>%
ggplot(aes(sp, log(cv))) + geom_point(alpha=0.2)
ggsave("../figs/raw-specdiv-vs-diversity.pdf", width = 7, height = 7)
