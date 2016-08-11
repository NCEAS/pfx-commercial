library(dplyr)
library(ggplot2)
library(ggrepel)
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

mr <- group_by(dat, strategy) %>%
  summarise(strategy_med_rev = median(revenue)/1e3, nn = length(unique(p_holder)))
g0$strategy_med_rev <- NULL
g0$nn <- NULL
g0 <- inner_join(g0, mr)

pl <-  g0 %>% mutate(str_label = ifelse(nn > 50, str_label, NA)) %>%
  ggplot(aes(x = strategy_mean_div, y = exp(g0.m))) +
  geom_ribbon(data = dd, aes(strategy_mean_div, y = exp(m), ymax=exp(u), ymin=exp(l)),
    # fill = "#65B2E7", alpha = 0.3) +
    fill = "grey85", alpha = 0.3) +
  geom_ribbon(data = dd, aes(strategy_mean_div, y = exp(m), ymax=exp(u.5), ymin=exp(l.5)),
    # fill = "#65B2E7", alpha = 0.8) +
    fill = "grey80", alpha = 0.8) +
  geom_line(data = dd, aes(strategy_mean_div, exp(m)), lwd = 0.8) +
  geom_segment(aes(y = exp(g0.l), yend = exp(g0.u), xend = strategy_mean_div),
    lwd = 0.2, color = "grey60") +
  geom_segment(aes(y = exp(g0.l.5), yend = exp(g0.u.5), xend = strategy_mean_div),
    lwd = 0.7, color = "grey35") +
  geom_text_repel(aes(label = str_label), size = 2.7, colour = "grey60",
    alpha = 1, segment.color = "grey80",
    point.padding = unit(0.0001, "lines"), max.iter = 3e4, segment.size = 0.2) +
  geom_point(aes(bg = nn), pch = 21, size = 1.9) +
  scale_fill_distiller(palette = "YlOrRd", trans = "log10") +
  coord_cartesian(ylim = c(0.2, 1.27)) + theme_gg() +
  labs(x = "Mean species diversity", y = "Estimated revenue variability",
    fill = "Number of\npermit holders") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
    legend.title = element_text(size = rel(0.75)))

ggsave("portfolio/figs/stan-across-strategy-variability.pdf", width = 5.4, height = 4.1)
