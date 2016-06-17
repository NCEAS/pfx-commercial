# Experiment with models 
# by fitting them quickly with TMB
rm(list = ls())
library(TMB)
library(dplyr)
library(ggplot2)
compile("revenue6.cpp")
dyn.load(dynlib("revenue6"))
dn <- readRDS("../data-generated/nonsalmon_linearModeling_complete.rds")
ds <- readRDS("../data-generated/salmon_linearModeling_complete.rds")

format_data <- function(x, n_obs = 100) {
  keep <- group_by(x, permit)  %>% summarize(n = n())  %>% filter(n>=n_obs)
  x <- x[x$permit %in% keep$permit, ]
  x <- x[!is.na(x$length), ]
  x$permit_id <- as.numeric(as.factor(x$permit))
  x$log_spec_div <- scale(log(x$specDiv))
  x$log_length <- scale(log(x$length + 1))
  x$log_days <- scale(log(x$days + 1))
  mm <- model.matrix(~ log_spec_div + log_days + log_length, data = x)
  n_k <- max(x$permit_id)
  n_fe <- ncol(mm)
 
  list(
    raw_data = x,
    data = list(x_ij = mm, 
      y_i = log(x$revenue), 
      k_i = x$permit_id - 1,
      n_k = n_k,
      n_j = 1, # spec_div column
      b1_cov_re_i = x$log_spec_div, 
      sigma1_cov_re_i = x$log_spec_div),
    parameters = list(
      b_j = rep(0, n_fe), 
      sigma_j = rep(0, n_fe), 
      log_b0_sigma = -1, 
      b0_k = rep(0, n_k),
      b1_k = rep(0, n_k), 
      log_b1_sigma = -1,
      sigma0_k = rep(0, n_k), 
      sigma1_k = rep(0, n_k),
      log_sigma0_sigma = -1, 
      log_sigma1_sigma = -1))
}

fit_model <- function(data) {
  random <- c("b0_k", "b_j", "sigma_j", "b1_k", "sigma0_k", "sigma1_k")
  d_tmb <- format_data(data)
  obj <- MakeADFun(
    data = d_tmb$data,
    parameters = d_tmb$parameters,
    random = random,
    DLL = "revenue6")
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

# plot_re <- function(dat = r, name, raw_data, add_permits = T, ind = NULL) {
#   re <- dplyr::filter(dat, parameter == name)
#   if (!is.null(ind)) re <- re[ind, ]
#   re$permit_id <- 1:nrow(re)
#   if (add_permits) {
#     re <- dplyr::left_join(re, unique(dplyr::select(raw_data, permit, permit_id)) )
#     re$permit_id <- re$permit
#   }
#   p <- ggplot(re, aes(permit_id, estimate)) + 
#     geom_pointrange(aes(ymin = l, ymax = u)) +
#       coord_flip()
#     print(p)
# }

ns_m <- fit_model(dn)
s_m <- fit_model(ds)

names(s_m$summary)
unique(s_m$summary$parameter)

s_m$summary$taxa <- "salmon"
ns_m$summary$taxa <- "non-salmon"
d <- bind_rows(s_m$summary, ns_m$summary) %>% 
  group_by(parameter, taxa) %>% 
  mutate(parameter_id = 1:n()) %>% 
    as_data_frame()

fe_lo <- data.frame(parameter_id = 1:4, par = c("intercept", "species_diversity", 
    "log_days", "log_length"))

  unique(d$parameter)
p <- filter(d, parameter %in% c("b_j", "sigma_j"), parameter_id > 1, parameter_id <= 4) %>% 
  inner_join(fe_lo) %>% 
  ggplot(aes(par, estimate, ymin = l, ymax = u, colour = taxa)) + 
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_wrap(~parameter) + 
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()
print(p)  
ggsave("../figs/tmb-fe.pdf", width = 7, height = 5)

# plot_re(ns_m$summary, name = "b_j", raw_data = ns_m$data, add_permits = F, ind = 2:4)


names(ds)
ns_permits <- unique(select(as.data.frame(ns_m$data), permit, permit_id))  %>% 
  mutate(taxa = "non-salmon")
s_permits <- unique(select(as.data.frame(s_m$data), permit, permit_id))  %>% 
  mutate(taxa = "salmon")
permits <- bind_rows(ns_permits, s_permits)

permit_diversity <- bind_rows(ds, dn) %>% 
  group_by(permit) %>% 
    summarize(mean_diversity = mean(specDiv))
permits <- left_join(permits, permit_diversity)

p <- filter(d, parameter %in% c("b0_k", "sigma0_k", "b1_b1_k", "sigma1_sigma1_k")) %>% 
  group_by(parameter, taxa) %>% 
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
  group_by(parameter, taxa) %>% 
    mutate(permit_id = 1:n()) %>% 
      left_join(permits) %>% 
  ggplot(aes(permit, estimate, ymin = l, ymax = u, colour = log(mean_diversity))) + 
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_wrap(~parameter) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip()
print(p)  
ggsave("../figs/tmb-re-slopes.pdf", width = 9.5, height = 7)

# plot_re(ns_m$summary, name = "b_j", raw_data = ns_m$data, add_permits = F, ind = 2:4)
# plot_re(s_m$summary, name = "b_j", raw_data = s_m$data, add_permits = F, ind = 2:4)
# plot_re(ns_m$summary, name = "b_j", raw_data = ns_m$data, add_permits = F, ind = 2:4)
# plot_re(s_m$summary, name = "b_j", raw_data = s_m$data, add_permits = F, ind = 2:4)

# plot_re(ns_m$summary, name = "b1_b1_k", raw_data = ns_m$data)
# plot_re(ns_m$summary, name = "sigma1_sigma1_k", raw_data = ns_m$data)

# plot_re(s_m$summary, name = "b1_b1_k", raw_data = s_m$data)
# plot_re(s_m$summary, name = "sigma1_sigma1_k", raw_data = s_m$data)

