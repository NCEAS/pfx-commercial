library(dplyr)

source("portfolio/analysis/prep-stan-model-matrix.R")

load("portfolio/data-generated/m.rda")

# -----------------------------------------------
# posterior beanplots

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m)

names(p$b_j) <- colnames(mm)
names(p$g_k) <- colnames(mm2)

colnames(p$coef_g0_strategy) <- md$strategy
colnames(p$coef_g1_strategy) <- md$strategy
colnames(p$coef_g2_strategy) <- md$strategy
colnames(p$coef_b1_strategy) <- md$strategy
colnames(p$coef_b2_strategy) <- md$strategy

g0 <- tidyr::gather(p$coef_g0_strategy, term, posterior) %>%
  mutate(parameter = "g0")
g1 <- tidyr::gather(p$coef_g1_strategy, term, posterior) %>%
  mutate(parameter = "g1")
g2 <- tidyr::gather(p$coef_g2_strategy, term, posterior) %>%
  mutate(parameter = "g2")
b1 <- tidyr::gather(p$coef_b1_strategy, term, posterior) %>%
  mutate(parameter = "b1")
b2 <- tidyr::gather(p$coef_b2_strategy, term, posterior) %>%
  mutate(parameter = "b2")
po <- bind_rows(g0, g1, g2, b1, b2)

pl <- ggplot(po, aes(term, posterior)) + geom_violin() +
  facet_wrap(~parameter, nrow=1) + ylab("")
  coord_flip()
ggsave("portfolio/figs/stan-str-posteriors.pdf", width = 15, height = 9)

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

ggplot(g0, aes(strategy_mean_div, exp(g0.m))) +
  geom_pointrange(aes(ymin = exp(g0.l), ymax = exp(g0.u)), lwd = 0.1) +
  geom_pointrange(aes(ymin = exp(g0.l.5), ymax = exp(g0.u.5))) +
  geom_line(data = dd, aes(strategy_mean_div, exp(m))) +
  geom_ribbon(data = dd, aes(strategy_mean_div, y = exp(m), ymax=exp(u), ymin=exp(l)),
    alpha = 0.2) +
  geom_ribbon(data = dd, aes(strategy_mean_div, y = exp(m), ymax=exp(u.5), ymin=exp(l.5)),
    alpha = 0.2) +
  coord_cartesian(ylim = c(0.15, 1.1)) + theme_light()
