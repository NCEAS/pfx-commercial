library(dplyr)
library(ggplot2)
library(viridis)
library(ggrepel)
load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m_noeffort.rda")
devtools::load_all("pfxr")

b <- broom::tidyMCMC(m_noeffort, conf.int = T, estimate.method = "median",
  conf.level = 0.5, conf.method = "quantile")

g1 <- b[grepl("coef_g1", b$term), ]
b1 <- b[grepl("coef_b1", b$term), ]
g2 <- b[grepl("coef_g2", b$term), ]
b2 <- b[grepl("coef_b1", b$term), ]

res <- data.frame(strategy_id = 1:nrow(g1)) %>%
  inner_join(md) %>%
  mutate(strategy = as.character(strategy))
res$dec <- g1$estimate
res$dec_rev <- b1$estimate
res$dec.l <- g1$conf.low
res$dec_rev.l <- b1$conf.low
res$dec.u <- g1$conf.high
res$dec_rev.u <- b1$conf.high
ns <- dat %>% group_by(strategy) %>%
  summarise(nn = length(unique(p_holder)))
res <- inner_join(res, ns)

ann_col <- "grey40"
p2 <- res %>%
  mutate(dec = -dec, dec_rev = -dec_rev) %>%
  mutate(dec.l = -dec.l, dec_rev.l = -dec_rev.l) %>%
  mutate(dec.u = -dec.u, dec_rev.u = -dec_rev.u) %>%
  mutate(strategy_label = ifelse(nn > 250, str_label, NA)) %>%
  ggplot(aes(x = dec_rev, y = dec)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey65") +
  geom_vline(xintercept = 0, lty = 2, col = "grey65") +
  geom_segment(aes(x = dec_rev.l, xend = dec_rev.u, y = dec, yend = dec),
    alpha = 0.1, lwd = 0.4) +
  geom_segment(aes(x = dec_rev, xend = dec_rev, y = dec.l, yend = dec.u),
    alpha = 0.1, lwd = 0.4) +
  geom_text_repel(aes(label = strategy_label, x = dec_rev, y = dec),
    size = 2.9, colour = "grey60", #segment.color = "grey80",
    point.padding = unit(0.3, "lines"), max.iter = 6e3, segment.size = 0.3) +
  geom_point(aes(color = strategy_mean_div, size = nn)) +
  xlab("Effect of specializing on revenue variability") +
  ylab("Effect of specializing on variability") +
  scale_color_viridis() +
  labs(colour = "Mean sp.\ndiversity", size = "Number of\npermit holders") +
  theme(legend.title = element_text(size = rel(0.85))) +
  theme_gg()

ggsave("portfolio/figs/stan-offset-break-anti-spaghetti-specializing-no-effort.pdf", plot = p2,
  width = 6, height = 4.75)
