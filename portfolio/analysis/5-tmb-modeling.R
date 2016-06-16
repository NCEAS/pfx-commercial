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
      n_j = ncol(mm) -1,
      b1_cov_re_i = x$log_spec_div, 
      sigma1_cov_re_i = x$log_spec_div),
    parameters = list(b_j = rep(0, n_fe), sigma_j = rep(0, n_fe), log_b0_sigma = -1, b0_k = rep(0, n_k),
      b1_k = rep(0, n_k), log_b1_sigma = -1, sigma0_k = rep(0, n_k), sigma1_k = rep(0, n_k),
      log_sigma0_sigma = -1, log_sigma1_sigma = -1))
}

fit_model <- function(data) {
  random <- c("b0_k", "b_j", "b1_k", "sigma0_k", "sigma1_k")
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

plot_re <- function(dat = r, re_name, raw_data) {
  re <- dplyr::filter(dat, parameter == re_name)
  re$permit_id <- 1:nrow(re)
  re <- dplyr::left_join(re, unique(dplyr::select(raw_data, permit, permit_id)) )
  library(ggplot2)
  p <- ggplot(re, aes(permit, estimate)) + 
    geom_pointrange(aes(ymin = l, ymax = u)) +
      coord_flip()
    print(p)
}

ns_m <- fit_model(dn)
s_m <- fit_model(ds)

plot_re(ns_m$summary, re_name = "b1_b1_k", raw_data = ns_m$data)
plot_re(ns_m, re_name = "sigma1_sigma1_k", raw_data = ns_m$data)

plot_re(s_m$summary, re_name = "b1_b1_k", raw_data = s_m$data)
plot_re(s_m$summary, re_name = "sigma1_sigma1_k", raw_data = s_m$data)

