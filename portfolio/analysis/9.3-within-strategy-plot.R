library(dplyr)
library(ggplot2)
library(viridis)
source("portfolio/analysis/prep-stan-model-matrix.R")
source("portfolio/analysis/grid_arrange_shared_legend.R")
load("portfolio/data-generated/m.rda")
devtools::load_all("pfxr")

b <- broom::tidyMCMC(m, conf.int = T, estimate.method = "median",
  conf.level = 0.8, conf.method = "quantile")

g1 <- b[grepl("coef_g1", b$term), ]
b1 <- b[grepl("coef_b1", b$term), ]
g2 <- b[grepl("coef_g2", b$term), ]
b2 <- b[grepl("coef_b1", b$term), ]

res <- data.frame(strategy_id = 1:nrow(g1)) %>%
  inner_join(md) %>%
  mutate(strategy = as.character(strategy))
res$inc <- g2$estimate
res$dec <- g1$estimate
res$inc_rev <- b2$estimate
res$dec_rev <- b1$estimate

ns <- dat %>% group_by(strategy) %>%
  summarise(nn = length(unique(p_holder)))
res <- inner_join(res, ns)

p1 <- res %>%
  mutate(strategy_label = ifelse(nn > 250, str_label, NA)) %>%
  mutate(inc = -inc) %>%
  ggplot(aes(x = inc_rev, y = inc)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_text_repel(aes(label = strategy_label, x = inc_rev, y = inc), nudge_y = 0.05,
    size = 3, colour = "grey50") +
  scale_color_viridis() +
  geom_point(aes(color = strategy_mean_div, size = nn)) +
  theme_gg() +
  xlab("Effect of generalizing on revenue") +
  ylab("Effect of generalizing on variability") +
  labs(colour = "Mean sp.\ndiversity", size = "Number of\npermits")

p2 <- res %>%
  mutate(dec = dec, dec_rev = -dec_rev) %>%
  mutate(strategy_label = ifelse(nn > 250, str_label, NA)) %>%
  ggplot(aes(x = dec_rev, y = dec)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_text_repel(aes(label = strategy_label, x = dec_rev, y = dec), nudge_y = 0.05,
    size = 3, colour = "grey50") +
  scale_color_viridis() +
  geom_point(aes(color = strategy_mean_div, size = nn)) +
  theme_gg() +
  xlab("Effect of specializing on revenue") +
  ylab("Effect of specializing on variability") +
  labs(colour = "Mean sp.\ndiversity", size = "Number of\npermits")

pdf("portfolio/figs/stan-offset-break-anti-spaghetti.pdf", width = 10, height = 4)
grid_arrange_shared_legend(p1, p2, ncol = 2, nrow = 1, position = "right")
dev.off()
