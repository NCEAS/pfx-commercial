library(dplyr)
library(ggplot2)
library(viridis)
library(ggrepel)
load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m_ifq.rda")
devtools::load_all("pfxr")

b <- broom::tidyMCMC(m_ifq, conf.int = T, estimate.method = "median",
  conf.level = 0.5, conf.method = "quantile")

g1 <- b[grepl("coef_g1", b$term), ]
b1 <- b[grepl("coef_b1", b$term), ]
g2 <- b[grepl("coef_g2", b$term), ]
b2 <- b[grepl("coef_b1", b$term), ]

res <- data.frame(strategy_ifq_id = 1:nrow(g1)) %>%
  inner_join(md_ifq) %>%
  mutate(strategy = as.character(strategy_ifq))
res$inc <- g2$estimate
res$dec <- g1$estimate
res$inc_rev <- b2$estimate
res$dec_rev <- b1$estimate

res$inc.l <- g2$conf.low
res$dec.l <- g1$conf.low
res$inc_rev.l <- b2$conf.low
res$dec_rev.l <- b1$conf.low

res$inc.u <- g2$conf.high
res$dec.u <- g1$conf.high
res$inc_rev.u <- b2$conf.high
res$dec_rev.u <- b1$conf.high

ns <- dat %>% group_by(strategy_ifq) %>%
  summarise(nn = length(unique(p_holder))) %>%
  rename(strategy = strategy_ifq)
res <- inner_join(res, ns)

p1 <- res %>%
  mutate(strategy_label = ifelse(nn > 30, strategy_ifq, NA)) %>%
  mutate(inc = inc) %>%
  ggplot(aes(x = inc_rev, y = inc)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey65") +
  geom_vline(xintercept = 0, lty = 2, col = "grey65") +
  geom_segment(aes(x = inc_rev.l, xend = inc_rev.u, y = inc, yend = inc),
    alpha = 0.1, lwd = 0.4) +
  geom_segment(aes(x = inc_rev, xend = inc_rev, y = inc.l, yend = inc.u),
    alpha = 0.1, lwd = 0.4) +
  geom_text_repel(aes(label = strategy_label, x = inc_rev, y = inc),
    size = 2.8, colour = "grey60", #segment.color = "grey80",
    point.padding = unit(0.3, "lines"), max.iter = 6e3, segment.size = 0.3) +
  geom_point(aes(color = strategy_ifq_mean_div, size = nn)) +
  xlab("Effect of generalizing on revenue") +
  ylab("Effect of generalizing on variability") +
  scale_color_viridis() +
  theme_gg() +
  guides(
    colour = guide_legend(override.aes = list(size=3.5), order = 1),
    size = guide_legend(order = 2, override.aes = list(pch = 21))) +
  annotate("text", x = min(res$inc_rev.l), y = max(res$inc.u), label = "A",
    fontface = "bold", size = 5) +
  labs(colour = "Mean sp.\ndiversity", size = "Number of\npermit holders") +
  theme(legend.title = element_text(size = rel(0.85)))

p2 <- res %>%
  mutate(dec = -dec, dec_rev = -dec_rev) %>%
  mutate(dec.l = -dec.l, dec_rev.l = -dec_rev.l) %>%
  mutate(dec.u = -dec.u, dec_rev.u = -dec_rev.u) %>%
  mutate(strategy_label = ifelse(nn > 30, strategy_ifq, NA)) %>%
  ggplot(aes(x = dec_rev, y = dec)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey65") +
  geom_vline(xintercept = 0, lty = 2, col = "grey65") +
  geom_segment(aes(x = dec_rev.l, xend = dec_rev.u, y = dec, yend = dec),
    alpha = 0.1, lwd = 0.4) +
  geom_segment(aes(x = dec_rev, xend = dec_rev, y = dec.l, yend = dec.u),
    alpha = 0.1, lwd = 0.4) +
  geom_text_repel(aes(label = strategy_label, x = dec_rev, y = dec),
    size = 2.8, colour = "grey60", #segment.color = "grey80",
    point.padding = unit(0.3, "lines"), max.iter = 6e3, segment.size = 0.3) +
  geom_point(aes(color = strategy_mean_div, size = nn)) +
  xlab("Effect of specializing on revenue") +
  ylab("Effect of specializing on variability") +
  scale_color_viridis() +
  annotate("text", x = min(-1*res$dec_rev.u), y = max(-1*res$dec.l), label = "B",
    fontface = "bold", size = 5) +
  theme_gg()


pdf("portfolio/figs/stan-offset-break-anti-spaghetti.pdf", width = 10, height = 4)
grid_arrange_shared_legend(p1, p2, ncol = 2, nrow = 1, position = "right")
dev.off()

# ifq

ifq <- readr::read_csv("data/ifq_pfshy_year.csv")
ifq <- ifq %>% filter(!is.na(year_catch_share))
ifq

ifq2 <- c("B61B", "B61B C61B", "B61B C61B S15B", "B61B S01", "B61B S03",
  "B61B S03A", "B61B S03E", "B61B S15B", "C61B", "K91", "K91 T09",
    "M7HB")

res <- res %>% mutate(ifq = strategy %in% ifq2)
