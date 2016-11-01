library(tidyverse)

load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m_ifq.rda")
load("portfolio/data-generated/m.rda")
devtools::load_all("pfxr")

b <- broom::tidyMCMC(m, conf.int = T, estimate.method = "median",
  conf.level = 0.5, conf.method = "quantile")
b_ifq <- broom::tidyMCMC(m_ifq, conf.int = T, estimate.method = "median",
  conf.level = 0.5, conf.method = "quantile")

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

g1_ifq <- b_ifq[grepl("coef_g1", b_ifq$term), ]
b1_ifq <- b_ifq[grepl("coef_b1", b_ifq$term), ]
g2_ifq <- b_ifq[grepl("coef_g2", b_ifq$term), ]
b2_ifq <- b_ifq[grepl("coef_b1", b_ifq$term), ]

res_ifq <- data.frame(strategy_ifq_id = 1:nrow(g1_ifq)) %>%
  inner_join(md_ifq) %>%
  mutate(strategy_ifq = as.character(strategy_ifq))

res_ifq$inc <- g2_ifq$estimate
res_ifq$dec <- g1_ifq$estimate
res_ifq$inc_rev <- b2_ifq$estimate
res_ifq$dec_rev <- b1_ifq$estimate

# Substring from right
substr_r <- function(x, n){
  substr(x, 1, nchar(x)-n)
}
ifqs <- grepl("IFQ", res_ifq$strategy_ifq)
res_ifq <- mutate(res_ifq,
  strategy = ifelse(ifqs, substr_r(strategy_ifq, 4L), strategy_ifq))

# ----------
# plot the effects between the 2 models

plot_ifq_effect <- function(variable = "inc", title = "", flip = FALSE) {
  d1 <- select_(res, "strategy", variable) %>%
    rename_(effect = variable)
  d2 <- select_(res_ifq, "strategy_ifq", "strategy", variable) %>%
    rename_(effect_ifq = variable)
  d <- left_join(d1, d2) %>%
    group_by(strategy) %>%
    mutate(ifq = grepl("IFQ", strategy_ifq)) %>%
    mutate(ifq_any = any(grepl("IFQ", strategy_ifq)))

  if (flip) {
    d$effect <- -1 * d$effect
    d$effect_ifq <- -1 * d$effect_ifq
  }

  d <- d %>%
    filter(ifq_any)
  ggplot(d, aes(x = "Grouped", xend = "IFQ split", y = effect, yend = effect_ifq,
    color = ifq)) + geom_segment() +
    geom_text(data = unique(select(d, effect, strategy)),
      aes(x = "Grouped", y = effect, label = strategy),
      hjust = "right", color = "black", inherit.aes = FALSE) +
    labs(title = title, color = "IFQ") +
    theme_light()
}

p1 <- plot_ifq_effect("inc", "Generalizing - variability")
p2 <- plot_ifq_effect("dec", "Specializing  - variability", flip = TRUE)
p3 <- plot_ifq_effect("inc_rev", "Generalizing  - revenue")
p4 <- plot_ifq_effect("dec_rev", "Specializing  - revenue", flip = TRUE)

pdf("portfolio/figs/ifq-within-comparison.pdf", width = 10, height = 11)
grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2, position = "right")
dev.off()

# ----------
# plot the effects just within the model that splits ifqs

plot_ifq_effect2 <- function(variable = "inc", title = "", flip = FALSE) {

  d2 <- select_(res_ifq, "strategy_ifq", "strategy", variable) %>%
    rename_(effect = variable)
  d <- d2 %>%
    group_by(strategy) %>%
    mutate(ifq = grepl("IFQ", strategy_ifq)) %>%
    mutate(ifq_any = any(grepl("IFQ", strategy_ifq))) %>%
    mutate(salmon = grepl("S", strategy_ifq))

  if (flip) {
    d$effect <- -1 * d$effect
  }

  d <- d %>%
    filter(ifq_any)

  select(d, -strategy_ifq) %>%
  spread(ifq, effect) %>%
  ggplot(aes(x = "Before-IFQ", xend = "IFQ", y = `FALSE`, yend = `TRUE`)) +
    geom_segment(aes(color = salmon)) +
    geom_text(aes(x = "IFQ", y = `TRUE`, label = strategy),
      hjust = "left", color = "black", inherit.aes = FALSE) +
    labs(title = title, y = "Effect", x = "") +
    theme_light()
}
p1 <- plot_ifq_effect2("inc", "Generalizing - variability")
p2 <- plot_ifq_effect2("dec", "Specializing  - variability", flip = TRUE)
p3 <- plot_ifq_effect2("inc_rev", "Generalizing  - revenue")
p4 <- plot_ifq_effect2("dec_rev", "Specializing  - revenue", flip = TRUE)

pdf("portfolio/figs/ifq-within-comparison-no-grouped.pdf", width = 10, height = 9)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
dev.off()

