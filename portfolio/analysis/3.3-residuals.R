library(dplyr)
library(ggplot2)
load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m-2016-08-23.rda")
devtools::load_all("pfxr")

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m)

dat$str_label <- NULL
dat <- inner_join(dat, select(md, strategy_id, str_label))

# names(p$b_j) <- colnames(mm)
# names(p$g_k) <- colnames(mm2)

colnames(p$coef_g0_strategy) <- md$strategy
colnames(p$coef_g1_strategy) <- md$strategy
colnames(p$coef_g2_strategy) <- md$strategy
colnames(p$coef_b1_strategy) <- md$strategy
colnames(p$coef_b2_strategy) <- md$strategy

library(tidyr)

# g0_strategy <- gather(p$coef_g0_strategy[1,], strategy, g0_strategy)
# g1_strategy <- gather(p$coef_g1_strategy[1,], strategy, g1_strategy)
# g2_strategy <- gather(p$coef_g2_strategy[1,], strategy, g2_strategy)

iters <- seq(1, nrow(p$b_j), 10)
length(iters)
out <- matrix(ncol = length(iters), nrow = nrow(dat))
dat_res_select <- select(dat, strategy, str_yr_id, revenue.prev)
for(i in seq_along(iters)) {
  message(round(i/length(iters), 2)*100)
  b1_strategy <- gather(p$coef_b1_strategy[iters[i],], strategy, b1_strategy)
  b2_strategy <- gather(p$coef_b2_strategy[iters[i],], strategy, b2_strategy)
  b0_str_yr <- gather(p$b0_str_yr[iters[i],], str_yr_id, b0_str_yr) %>%
    mutate(str_yr_id = as.numeric(substring(str_yr_id, 11)))

  dat_res <- dat_res_select
  dat_res <- inner_join(dat_res, b0_str_yr, by = "str_yr_id")
  dat_res <- inner_join(dat_res, b1_strategy, by = "strategy")
  dat_res <- inner_join(dat_res, b2_strategy, by = "strategy")

  eta <- as.matrix(mm) %*% t(as.matrix(p$b_j[iters[i],]))
  eta <- eta + log(dat$revenue.prev)
  eta <- eta + dat_res$b0_str_yr +
    mm[,1] * dat_res$b1_strategy +
    mm[,2] * dat_res$b2_strategy
  out[,i] <- eta[,1]
}

dat_res <- dat
# out <- group_by(out, row_n) %>% summarise(eta = median(eta))
dat_res$eta <- apply(out, 1, median)

# dat_res$eta <- eta[,1]

dat_res <- mutate(dat_res,
  inc = ifelse(spec_change > 0, "inc", ifelse(spec_change == 0, "zero", "dec")))

g <- ggplot(dat_res,
  aes(spec_change, log(abs(log(revenue) - eta)), group = inc)) +
  geom_point(alpha = 0.07) +
  facet_wrap(~str_label) +
  geom_smooth(se=F, colour = "red", method = "lm") +
  theme_gg() + ylim(-10, 2) +
  ylab("log(|residual|)") + xlab("Change in species diversity") +
  theme(
    panel.grid.major = element_line(colour = "grey92", size = 0.2),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 6.5))
ggsave("portfolio/figs/stan-offset-panel-mean-resid.png", dpi = 150,
  width = 8, height = 7)

# downside:
g <- dat_res %>%
  mutate(resid = log(revenue) - eta) %>%
  mutate(downside = ifelse(resid < 0, TRUE, FALSE)) %>%
  filter(downside) %>%
  mutate(log_abs_resid = log(abs(resid))) %>%
  ggplot(aes(spec_change, log_abs_resid, group = inc)) +
  geom_point(alpha = 0.07) +
  facet_wrap(~str_label) +
  geom_smooth(se=F, colour = "red", method = "lm") +
  theme_gg() + ylim(-10, 2) +
  ylab("log(|negative residual|)") + xlab("Change in species diversity") +
  theme(
    panel.grid.major = element_line(colour = "grey92", size = 0.2),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 6.5))
ggsave("portfolio/figs/stan-offset-panel-mean-resid-down.png", dpi = 150,
  width = 8, height = 7)

# ---------------------------------------------------------
# str-yrs:

iters <- seq(1, 1000, 50)
print(length(iters))
o <- plyr::ldply(iters, function(i) {
  b0_str_yr <- gather(p$b0_str_yr[i,], str_yr_id, b0_str_yr) %>%
    mutate(str_yr_id = as.numeric(substring(str_yr_id, 11)))
  dat_res2 <- dat
  dat_res2 <- inner_join(dat_res2, b0_str_yr, by = "str_yr_id")
  str_yr <- group_by(dat_res2, str_label, year) %>%
    summarize(b0_str_yr = b0_str_yr[1]) %>%
    mutate(iter = i)
  str_yr
})

fake <- expand.grid(year = seq(min(o$year), max(o$year)),
  str_label = unique(o$str_label), iter = iters, stringsAsFactors = FALSE)

missing <- anti_join(fake, o) %>%
  mutate(missing = NA)
o <- full_join(o, missing)

g <- ggplot(o, aes(year, b0_str_yr, group = iter)) + geom_line(alpha = 0.1) +
  facet_wrap(~str_label) +
  theme_gg() +
  theme(panel.grid.major = element_line(colour = "grey85",
    size = 0.2), panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 6)) +
  scale_x_continuous(breaks = seq(1980, 2015, 10)) +
  ylab("Strategy-year intercept (b0)")
ggsave("portfolio/figs/strategy-year-effects.pdf", width = 10, height = 9)

# dat_ann = readRDS(file="portfolio/data-generated/cfec-annual-for-modeling.rds")
# dat_diff = readRDS(file="portfolio/data-generated/cfec-diff-for-modeling.rds")

# filter(dat_diff, substr(strategy_permit,1,3)%in%c("D09","D9C","D9D")) %>%

# rm(mm)
# source("portfolio/analysis/prep-stan-model-matrix.R")
# dat$str_label <- NULL
# dat <- inner_join(dat, select(md, strategy_id, str_label))
# ggplot(filter(dat, strategy == "D09"), aes(year, revenue)) + geom_point(alpha = 0.1) +
#   facet_wrap(~strategy) + geom_hline(yintercept = 5000) + xlim(1986, 2015)
# length(unique(dat$strategy))

