# Experiment with models
# by fitting them quickly with TMB
rm(list = ls())
library(TMB)
library(dplyr)
library(ggplot2)
compile("revenue7.cpp")
dyn.load(dynlib("revenue7"))

dm <- readRDS("../data-generated/cfec-for-modeling.rds")
dat = readRDS(file="../data-generated/cfec-annual-for-modeling.rds")
dat$strategy_taxa <- dat$strategy
dat$strategy <- dat$strategy_gear
nrow(dat)
dat = dat[which(dat$revenue >= 5000), ]
nrow(dat)

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
n_sample <- round(length(unique_holders)*0.3)
set.seed(1234)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

# many different strategies, need to model only most common,
top.strategies = names(rev(sort(table(dat$strategy)))[1:25])
dat = dat[dat$strategy%in%top.strategies, ]
nrow(dat)
sum(!is.na(dat$length))

# strategies <- readr::read_csv("../data-generated/strategies.csv")
# strategies <- strategies[1:100, ] %>% rename(strategy = Var1)
# dm <- left_join(strategies,dm)
# nrow(dm)

# # for now only model those strategies where there is sufficient contrast in diversity
# dm <- dm %>% group_by(strategy) %>%
#   # mutate(mean_spec_div = mean(specDiv, na.rm = TRUE)) %>%
#   mutate(iqr_spec_div = IQR(specDiv, na.rm = TRUE)) %>%
#     as_data_frame

# # dm <- dm %>% mutate(strategy_diverse = ifelse(mean_spec_div >= 1.05, strategy, NA))
# dm <- dm[dm$iqr_spec_div >= 0.10, ]
# nrow(dm)

# # dm <- group_by(dm, strategy) %>% mutate(n = n()) %>% subset(n>=50) %>%
# #   as_data_frame()
# # nrow(dm)
# dm <- dm[!is.na(dm$length), ]
# nrow(dm)

# assign some strategy IDs for the purpose of identifying random effects
strategy_ids  <- select(dat, strategy)  %>%
  unique %>% mutate(strategy_id = 1:n())
pholder_ids <- select(dat, p_holder) %>%
  unique %>% mutate(pholder_id = 1:n())
dat <- inner_join(dat, strategy_ids)  %>% inner_join(pholder_ids)
nrow(dat)

format_data <- function(x) {
  x$spec_div <- scale(x$specDiv)
  x$log_length <- scale(log(x$length + 1))
  x$log_weight <- scale(log(x$weight + 1))
  x$log_days <- scale(log(x$days + 1))
  x$scaled_npermit <- scale(x$npermit)
  mm <- model.matrix(~ (spec_div + log_days + scaled_npermit)^2 +
    I(spec_div^2) + I(log_days^2) + I(scaled_npermit^2), data = x)
  mm_sigma <- model.matrix(~ (spec_div + log_days + scaled_npermit)^2, data = x)
  n_pholder <- max(x$pholder_id)
  n_strategy <- max(x$strategy_id)
  n_fe <- ncol(mm)
  n_fe_sigma <- ncol(mm_sigma)
  # re_estimate = unique(x[,c("strategy","strategy_diverse")])$strategy_diverse

  list(
    mm = mm,
    mm_sigma = mm_sigma,
    raw_data = x,

    data = list(
      x_ij = mm,
      x_sigma_ij = mm_sigma,
      y_i = log(x$revenue/1e4),
      pholder_i = x$pholder_id - 1,
      strategy_i = x$strategy_id - 1,
      n_pholder = n_pholder,
      n_strategy = n_strategy,
      diversity_column = 1, # spec_div column
      b1_cov_re_i = x$spec_div,
      b2_cov_re_i = x$log_days,
      b3_cov_re_i = x$scaled_npermit,
      g1_cov_re_i = x$spec_div),

    parameters = list(
      b_j = rep(0, n_fe),
      sigma_j = rep(0, n_fe_sigma),

      # log_b0_pholder_tau = -1,
      log_b0_strategy_tau = -1,
      log_b1_strategy_tau = -1,
      log_b2_strategy_tau = -1,
      log_b3_strategy_tau = -1,

      # log_g0_pholder_tau = -1,
      log_g0_strategy_tau = -1,
      log_g1_strategy_tau = -1,

      # b0_pholder = rep(0, n_pholder),
      b0_strategy = rep(0, n_strategy),
      b1_strategy = rep(0, n_strategy),
      b2_strategy = rep(0, n_strategy),
      b3_strategy = rep(0, n_strategy),

      # g0_pholder = rep(0, n_pholder),
      g0_strategy = rep(0, n_strategy),
      g1_strategy = rep(0, n_strategy))

    # map = list(
    #   b1_strategy = re_estimate,
    #   g1_strategy = re_estimate
    #   )

    )
}

fit_model <- function(data) {
  random <- c(
  # "b0_pholder",
  "b0_strategy", "b_j",
    "sigma_j",
    "b1_strategy",
    "b2_strategy",
    "b3_strategy",
    # "g0_pholder",
    "g0_strategy", "g1_strategy")
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
    gradient = obj$gr(opt$par), mm = d_tmb$mm, mm_sigma = d_tmb$mm_sigma)
}

m <- fit_model(dat)
m$gradient
m$sd_report
m$summary %>% filter(parameter=="b_j")
m$mm %>% head

d <- m$summary %>%
  group_by(parameter) %>%
  mutate(parameter_id = 1:n()) %>%
    as_data_frame()

fe_lo <- data.frame(parameter_id = 1:ncol(m$mm),
  par = colnames(m$mm))

p <- filter(d, parameter %in% c("b_j"), parameter_id > 1) %>%
  inner_join(fe_lo) %>%
  ggplot(aes(par, estimate, ymin = l, ymax = u)) +
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_wrap(~parameter) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()
print(p)

fe_lo_sigma <- data.frame(parameter_id = 1:ncol(m$mm_sigma),
  par = colnames(m$mm_sigma))

p <- filter(d, parameter %in% c("sigma_j"), parameter_id > 1) %>%
  inner_join(fe_lo_sigma) %>%
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
  c("b0_strategy", "g0_strategy", "combined_b1_strategy", "combined_g1_strategy")) %>%
# p <- filter(d, parameter %in% c("combined_b1_strategy", "combined_g1_strategy")) %>%
  group_by(parameter) %>%
    mutate(strategy_id = 1:n()) %>%
      left_join(ids)

junk <- p %>% as_data_frame %>% filter(parameter == "combined_g1_strategy") %>%
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

j <- filter(d, parameter %in% c("b0_strategy", "g0_strategy")) %>%
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
  ggplot(aes(g0_strategy, b0_strategy, label = strategy)) + geom_point() +
    geom_text()
p
ggsave("../figs/tmb-re-int-vs.pdf", width = 10, height = 9)

j1 <- filter(d, parameter %in% c("combined_b1_strategy", "combined_g1_strategy")) %>%
  group_by(parameter) %>%
    mutate(strategy_id = 1:n()) %>%
      left_join(ids) %>%
        as_data_frame() %>%
          na.omit()
reshape2::dcast(j1, strategy  ~ parameter, value.var = "estimate") %>%
  ggplot(aes(combined_g1_strategy, combined_b1_strategy, label = strategy)) + geom_point() +
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
  c("g0_pholder")) %>%
# p <- filter(d, parameter %in% c("combined_b1_strategy", "combined_g1_strategy")) %>%
  group_by(parameter) %>%
    mutate(pholder_id = 1:n()) %>%
      left_join(ids)

ggplot(p, aes(mean_diversity, estimate)) + geom_point(alpha = 0.1)
ggsave("../figs/tmb-re-pholder-g0-vs-diversity.pdf", width = 7, height = 7)

group_by(dm, p_holder) %>%
  summarise(sp = mean(specDiv), cv=sd(log(revenue))) %>%
ggplot(aes(sp, log(cv))) + geom_point(alpha=0.2)
ggsave("../figs/raw-specdiv-vs-diversity.pdf", width = 7, height = 7)
