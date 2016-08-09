library(dplyr)
library(ggplot2)
devtools::load_all("pfxr")
source("portfolio/analysis/prep-stan-model-matrix.R")
load("portfolio/data-generated/m.rda")

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m)

colnames(p$coef_g0_strategy) <- md$strategy

g0 <- tidyr::gather(p$coef_g0_strategy, strategy, posterior) %>%
  group_by(strategy) %>%
  summarise(
    g0.l = quantile(posterior, probs = 0.025),
    g0.l.5 = quantile(posterior, probs = 0.25),
    g0.m = quantile(posterior, probs = 0.5),
    g0.u.5 = quantile(posterior, probs = 0.75),
    g0.u = quantile(posterior, probs = 0.975)) %>%
  inner_join(md)

x <- seq(min(md$strategy_mean_div), max(md$strategy_mean_div), length.out = 300)
dd <- plyr::ldply(x, function(xx)
  data.frame(strategy_mean_div = xx,
    m = median(p$h1[[1]] * xx + p$g0[[1]]),
    l = quantile(p$h1[[1]] * xx + p$g0[[1]], probs = 0.05),
    u = quantile(p$h1[[1]] * xx + p$g0[[1]], probs = 0.95),
    l.5 = quantile(p$h1[[1]] * xx + p$g0[[1]], probs = 0.25),
    u.5 = quantile(p$h1[[1]] * xx + p$g0[[1]], probs = 0.75)
  ))

mr <- group_by(dat, strategy) %>% summarise(strategy_med_rev = median(revenue)/1e3)
g0$strategy_med_rev <- NULL
g0 <- inner_join(g0, mr)

pl <- ggplot(g0, aes(x = strategy_mean_div, y = exp(g0.m))) +
  geom_segment(aes(y = exp(g0.l), yend = exp(g0.u), xend = strategy_mean_div),
    lwd = 0.2, color = "grey50") +
  geom_segment(aes(y = exp(g0.l.5), yend = exp(g0.u.5), xend = strategy_mean_div),
    lwd = 0.7, color = "grey30") +
  # geom_point(aes(size = strategy_med_rev)) +
  # scale_size(range = c(0.5, 3)) +
  geom_ribbon(data = dd, aes(strategy_mean_div, y = exp(m), ymax=exp(u), ymin=exp(l)),
    alpha = 0.2) +
  geom_ribbon(data = dd, aes(strategy_mean_div, y = exp(m), ymax=exp(u.5), ymin=exp(l.5)),
    alpha = 0.2) +
  geom_line(data = dd, aes(strategy_mean_div, exp(m))) +
  geom_text_repel(aes(label = str_label), size = 2.7, colour = "grey55", alpha = 0.9, segment.color = "grey80") +
  geom_point(aes(bg = (strategy_med_rev)), pch = 21, colour = "grey50") +
  scale_fill_distiller(palette = "YlOrRd") +
  coord_cartesian(ylim = c(0.2, 1.07)) + theme_gg() +
  labs(x = "Mean species diversity", y = "Revenue variability",
    fill = "Median revenue\n(1,000 $)") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  theme(legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_text(size = rel(0.75)))

ggsave("portfolio/figs/stan-across-strategy-variability.pdf", width = 5.3, height = 4)
