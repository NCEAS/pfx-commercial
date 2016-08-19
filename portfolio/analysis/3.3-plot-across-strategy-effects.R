library(dplyr)
library(ggplot2)
library(ggrepel)
devtools::load_all("pfxr")
load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m.rda")

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m)

# colnames(p$coef_g0_strategy) <- md$strategy
#

# g0.0 <- p$g0[[1]]
# g0 <- tidyr::gather(p$coef_g0_strategy, strategy, posterior) %>%
#   group_by(strategy) %>%
#   summarise(
#     g0.l = quantile(posterior/2 + g0.0/2, probs = 0.025),
#     g0.l.5 = quantile(posterior/2 + g0.0/2, probs = 0.25),
#     g0.m = quantile(posterior/2+ g0.0/2, probs = 0.5),
#     g0.u.5 = quantile(posterior/2 + g0.0/2, probs = 0.75),
#     g0.u = quantile(posterior/2 + g0.0/2, probs = 0.975)) %>%
#   inner_join(md)

g0_temp <- p$g0_strategy %>%
  tidyr::gather(strategy, posterior) %>%
  mutate(strategy_id =
      as.numeric(sub("g0_strategy_([0-9]+)", "\\1", strategy))) %>%
  select(-strategy) %>%
  inner_join(md) %>%
  mutate(
    h1 = rep(p$h1[[1]], nrow(md)),
    h2 = rep(p$h2[[1]], nrow(md)),
    g0main = rep(p$g0[[1]], nrow(md))) %>%
  rename(g0j = posterior)

# # see 3.6-sim for the proof of the algebra behind this, ouch:
# plot(xh1, g0 + (h1 * xh1 + unique(g0j) - h2 * xh2)/2)
# abline(a = g0, b = h1)
g0_temp <- mutate(g0_temp, part_resid =
    g0main +
    (g0j + h1 * scaled_strategy_mean_div -
           h2 * scaled_strategy_mean_days)/2)

g0 <- g0_temp %>% group_by(strategy) %>%
  summarise(
    g0.m = median(part_resid),
    g0.l = quantile(part_resid, probs = 0.05),
    g0.u = quantile(part_resid, probs = 0.95),
    g0.l.5 = quantile(part_resid, probs = 0.25),
    g0.u.5 = quantile(part_resid, probs = 0.75)) %>%
  ungroup() %>%
  inner_join(md)

x_scaled <- seq(min(md$scaled_strategy_mean_div),
  max(md$scaled_strategy_mean_div), length.out = 200)
x_true <- seq(min(md$strategy_mean_div),
  max(md$strategy_mean_div), length.out = 200)
dd <- plyr::ldply(seq_along(x_scaled), function(i) {
  xx <- x_scaled[i]
  data.frame(strategy_mean_div = x_true[i],
    m = median(p$h1[[1]] * xx + p$g0[[1]]),
    l = quantile(p$h1[[1]] * xx + p$g0[[1]], probs = 0.05),
    u = quantile(p$h1[[1]] * xx + p$g0[[1]], probs = 0.95),
    l.5 = quantile(p$h1[[1]] * xx + p$g0[[1]], probs = 0.25),
    u.5 = quantile(p$h1[[1]] * xx + p$g0[[1]], probs = 0.75)
  )})

mr <- group_by(dat, strategy) %>%
  summarise(strategy_med_rev = median(revenue)/1e3,
    nn = length(unique(p_holder)))
g0$strategy_med_rev <- NULL
g0$nn <- NULL
g0 <- inner_join(g0, mr)

pl <-  g0 %>% mutate(str_label = ifelse(nn > 50, str_label, NA)) %>%
  ggplot(aes(x = strategy_mean_div, y = exp(g0.m))) +
  geom_ribbon(data = dd, aes(strategy_mean_div, y = exp(m), ymax=exp(u),
    ymin=exp(l)), fill = "grey85", alpha = 0.3) +
  geom_ribbon(data = dd, aes(strategy_mean_div, y = exp(m), ymax=exp(u.5),
    ymin=exp(l.5)), fill = "grey80", alpha = 0.8) +
  geom_line(data = dd, aes(strategy_mean_div, exp(m)), lwd = 0.8,
    col = "grey20") +
  geom_segment(aes(y = exp(g0.l), yend = exp(g0.u),
    xend = strategy_mean_div), lwd = 0.2, color = "grey60") +
  geom_segment(aes(y = exp(g0.l.5), yend = exp(g0.u.5),
    xend = strategy_mean_div),
  lwd = 0.7, color = "grey35") +
  geom_text_repel(aes(label = str_label), size = 2.7, colour = "grey60",
  alpha = 1, segment.color = "grey80",
  point.padding = unit(0.0001, "lines"), max.iter = 3e4, segment.size = 0.2) +
  geom_point(aes(bg = nn), pch = 21, size = 1.9) +
  scale_fill_distiller(palette = "YlOrRd", trans = "log10") +
  coord_cartesian(ylim = c(0.2, 0.95)) + theme_gg() +
  labs(x = "Mean species diversity", y = "Estimated revenue variability",
    fill = "Number of\npermit holders") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
    legend.title = element_text(size = rel(0.75)))

ggsave("portfolio/figs/stan-across-strategy-variability.pdf", width = 5.4, height = 4.3)
