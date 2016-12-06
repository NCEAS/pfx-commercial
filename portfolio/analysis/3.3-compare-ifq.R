library(tidyverse)

load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m_ifq.rda")
load("portfolio/data-generated/m.rda")
devtools::load_all("pfxr")

b <- broom::tidyMCMC(m, conf.int = T, estimate.method = "median",
  conf.level = 0.5, conf.method = "quantile")
b_ifq <- broom::tidyMCMC(m_ifq, conf.int = T, estimate.method = "median",
  conf.level = 0.5, conf.method = "quantile")

g0 <- b[grepl("coef_g0", b$term), ]
g1 <- b[grepl("coef_g1", b$term), ]
b1 <- b[grepl("coef_b1", b$term), ]
g2 <- b[grepl("coef_g2", b$term), ]
b2 <- b[grepl("coef_b1", b$term), ]

res <- data.frame(strategy_id = 1:nrow(g1)) %>%
  inner_join(md) %>%
  mutate(strategy = as.character(strategy))
res$g0 <- g0$estimate
res$g0_lwr <- g0$conf.low
res$g0_upr <- g0$conf.high
res$inc <- g2$estimate
res$dec <- g1$estimate
res$inc_rev <- b2$estimate
res$dec_rev <- b1$estimate

g0_ifq <- b_ifq[grepl("coef_g0", b_ifq$term), ]
g1_ifq <- b_ifq[grepl("coef_g1", b_ifq$term), ]
b1_ifq <- b_ifq[grepl("coef_b1", b_ifq$term), ]
g2_ifq <- b_ifq[grepl("coef_g2", b_ifq$term), ]
b2_ifq <- b_ifq[grepl("coef_b1", b_ifq$term), ]

res_ifq <- data.frame(strategy_ifq_id = 1:nrow(g1_ifq)) %>%
  inner_join(md_ifq) %>%
  mutate(strategy_ifq = as.character(strategy_ifq))

res_ifq$g0 <- g0_ifq$estimate
res_ifq$g0_lwr <- g0_ifq$conf.low
res_ifq$g0_upr <- g0_ifq$conf.high
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

plot_ifq_effect <- function(variable = "inc", title = "", flip = FALSE, lab = "",
  confidence_intervals = FALSE, cols = RColorBrewer::brewer.pal(8, "Greys")[c(4, 8)]) {
  d1 <- select_(res, "strategy", variable) %>%
    rename_(effect = variable)
  d2 <- select_(res_ifq, "strategy_ifq", "strategy", variable) %>%
    rename_(effect_ifq = variable)
  d <- left_join(d1, d2) %>%
    group_by(strategy) %>%
    mutate(ifq = grepl("IFQ", strategy_ifq)) %>%
    mutate(ifq_any = any(grepl("IFQ", strategy_ifq))) %>%
    left_join(select(md, strategy, str_label))

  if (flip) {
    d$effect <- -1 * d$effect
    d$effect_ifq <- -1 * d$effect_ifq
  }

  d <- d %>%
    filter(ifq_any)
  g <- ggplot(d, aes(x = 1, xend = 2, y = effect, yend = effect_ifq,
    color = ifq, linetype = ifq)) + geom_segment() +
    geom_text(data = unique(select(d, effect, strategy, str_label)),
      aes(x = 1, y = effect, label = str_label),
      hjust = "right", color = "black", inherit.aes = FALSE, size = 3) +
    labs(color = "IFQ", linetype = "IFQ") +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c(1, 1)) +
    theme_light() +
    scale_x_continuous(limits = c(-0.5, 2.3), breaks = c(1, 2), labels = c("Grouped", "IFQ split")) +
    ylab(lab) +
    xlab("") +
    annotate("text", y = 2.9, x = -0.5, label = "B",
      fontface = "bold", size = 5)
    # theme(legend.position=c(.9,.85))
  if (confidence_intervals) {
    warning("Not implemented")
  }
  g
}


p0 <- plot_ifq_effect("g0", "Variability intercept")
p1 <- plot_ifq_effect("inc", "", lab = "Effect of generalizing on variability")
p1
p2 <- plot_ifq_effect("dec", "Specializing  - variability", flip = TRUE)
p3 <- plot_ifq_effect("inc_rev", "Generalizing  - revenue")
p4 <- plot_ifq_effect("dec_rev", "Specializing  - revenue", flip = TRUE)

pdf("portfolio/figs/ifq-within-comparison.pdf", width = 10, height = 11)
grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2, position = "right")
dev.off()

# ----------
# plot the effects just within the model that splits ifqs

## plot_ifq_effect2 <- function(variable = "inc", title = "", flip = FALSE) {
##
##   d2 <- select_(res_ifq, "strategy_ifq", "strategy", variable) %>%
##     rename_(effect = variable)
##   d <- d2 %>%
##     group_by(strategy) %>%
##     mutate(ifq = grepl("IFQ", strategy_ifq)) %>%
##     mutate(ifq_any = any(grepl("IFQ", strategy_ifq))) %>%
##     mutate(salmon = grepl("S", strategy_ifq))
##
##   if (flip) {
##     d$effect <- -1 * d$effect
##   }
##
##   d <- d %>%
##     filter(ifq_any)
##
##   select(d, -strategy_ifq) %>%
##   spread(ifq, effect) %>%
##   ggplot(aes(x = "Before-IFQ", xend = "IFQ", y = `FALSE`, yend = `TRUE`)) +
##     geom_segment(aes(color = salmon)) +
##     geom_text(aes(x = "IFQ", y = `TRUE`, label = strategy),
##       hjust = "left", color = "black", inherit.aes = FALSE) +
##     labs(title = title, y = "Effect", x = "") +
##     theme_light()
## }
##
## p0 <- plot_ifq_effect2("g0", "Variability intercept")
## p1 <- plot_ifq_effect2("inc", "Generalizing - variability")
## p2 <- plot_ifq_effect2("dec", "Specializing  - variability", flip = TRUE)
## p3 <- plot_ifq_effect2("inc_rev", "Generalizing  - revenue")
## p4 <- plot_ifq_effect2("dec_rev", "Specializing  - revenue", flip = TRUE)
##
## pdf("portfolio/figs/ifq-within-comparison-no-grouped.pdf", width = 10, height = 9)
## gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
## dev.off()
##
## mean_diversity <- dat %>%
##   group_by(strategy) %>%
##   summarize(mean_diversity = mean(specDiv)) %>%
##   ungroup()
##
## mean_diversity_ifq <- dat %>%
##   group_by(strategy_ifq, strategy) %>%
##   summarize(mean_diversity_ifq = mean(specDiv)) %>%
##   ungroup()
##
## mean_diversity <- inner_join(mean_diversity, mean_diversity_ifq)
##
## mean_diversity <- mean_diversity %>%
##   group_by(strategy) %>%
##   mutate(ifq = grepl("IFQ", strategy_ifq)) %>%
##   mutate(ifq_any = any(grepl("IFQ", strategy_ifq))) %>%
##   mutate(salmon = grepl("S", strategy_ifq)) %>%
##   filter(ifq_any) %>%
##   ungroup()
##
## p0 <- plot_ifq_effect2("g0", "Variability intercept")
##
## p0_diversity <- select(mean_diversity, -strategy_ifq, -mean_diversity) %>%
##   spread(ifq, mean_diversity_ifq) %>%
##   ggplot(aes(x = "Before-IFQ", xend = "IFQ", y = `FALSE`, yend = `TRUE`)) +
##     geom_segment(aes(color = salmon)) +
##     geom_text(aes(x = "IFQ", y = `TRUE`, label = strategy),
##       hjust = "left", color = "black", inherit.aes = FALSE) +
##     labs(title = "Mean diversity", y = "Mean diversity", x = "") +
##     theme_light()
##
## pdf("portfolio/figs/ifq-g0s.pdf", width = 4.5, height = 7)
## gridExtra::grid.arrange(p0, p0_diversity)
## dev.off()

# So it looks like the variability intercept is lower with
# ifqs especially for those strategies that don't have diversity from
# salmon. Diversity might have lowered a bit with ifqs, but only
# for those strategies that also include salmon.
# Let's check if the declines in variability are meaningful.

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m_ifq)

g0_temp <- p$g0_strategy %>%
  tidyr::gather(strategy, posterior) %>%
  mutate(strategy_ifq_id =
      as.numeric(sub("g0_strategy_([0-9]+)", "\\1", strategy))) %>%
  select(-strategy) %>%
  inner_join(md_ifq)

get_ifq_ratio <- function(strategy) {
  before <- filter(g0_temp, strategy_ifq == strategy)
  after <- filter(g0_temp, strategy_ifq == paste(strategy, "IFQ"))
  # hist(before$posterior)
  # hist(after$posterior)
  ratio <- exp(after$posterior) / exp(before$posterior)
  data.frame(strategy = strategy, ratio = ratio)
}

s <- unique(g0_temp$strategy_ifq)
strategies <- gsub(" IFQ", "", s[grepl("IFQ", s)])

out <- plyr::ldply(strategies, get_ifq_ratio)
out <- out %>% filter(ratio < 7) %>%
  group_by(strategy) %>%
  mutate(median_ratio = median(ratio)) %>%
  ungroup() %>%
  left_join(select(md, strategy, str_label))

labs <- unique(select(out, median_ratio, str_label)) %>%
  mutate(str_label_ordered = factor(str_label,
      levels = str_label[order(-median_ratio)])) %>%
  select(-median_ratio)

out$str_label_ordered <- NULL
out <- inner_join(out, labs)

g <- ggplot(out, aes(str_label_ordered, ratio)) +
  geom_hline(yintercept = 1, col = "grey60") +
  geom_violin(fill = "grey95") +
  scale_y_log10(breaks = c(0.6, 0.8, 1, 1.2, 1.4, 1.6)) +
  xlab("After IFQ / Before IFQ variability") +
  coord_flip() +
  theme_light() +
  annotate("text", y = 0.55, x = 9.2, label = "A",
    fontface = "bold", size = 5)
# g
ggsave("portfolio/figs/itq-ratios.pdf", width = 4.5, height = 4)

pdf("portfolio/figs/itq-combined.pdf", width = 9.5, height = 4.5)
gridExtra::grid.arrange(g, p1, layout_matrix = as.matrix(t(c(1, 1, 1, 2, 2, 2))))
dev.off()
