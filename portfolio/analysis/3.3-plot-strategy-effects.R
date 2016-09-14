library(dplyr)

load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m.rda")
devtools::load_all("pfxr")

# -----------------------------------------------
# posterior beanplots

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m)

names(p$b_j) <- colnames(mm)
names(p$g_k) <- colnames(mm2)

colnames(p$g0_strategy) <- md$strategy
colnames(p$coef_g0_strategy) <- md$strategy
colnames(p$coef_g1_strategy) <- md$strategy
colnames(p$coef_g2_strategy) <- md$strategy
colnames(p$coef_b1_strategy) <- md$strategy
colnames(p$coef_b2_strategy) <- md$strategy

g0 <- tidyr::gather(p$g0_strategy, term, posterior) %>%
  mutate(parameter = "g0", g0main = rep(p$g0[[1]], length(unique(md$strategy))))
g0 <- mutate(g0, posterior = posterior + g0main) %>%
  select(-g0main)
g1 <- tidyr::gather(p$coef_g1_strategy, term, posterior) %>%
  mutate(parameter = "g1") %>%
  mutate(posterior = -1 * posterior) # invert to match fig 4
g2 <- tidyr::gather(p$coef_g2_strategy, term, posterior) %>%
  mutate(parameter = "g2")
b1 <- tidyr::gather(p$coef_b1_strategy, term, posterior) %>%
  mutate(parameter = "b1") %>%
  mutate(posterior = -1 * posterior) # invert to match fig 4
b2 <- tidyr::gather(p$coef_b2_strategy, term, posterior) %>%
  mutate(parameter = "b2")
po <- bind_rows(g0, g1, g2, b1, b2)

g2_prob_dens_above_zero <- round(sum(g2$posterior > 0)/nrow(g2)*100, 0)
saveRDS(g2_prob_dens_above_zero,
  file = "portfolio/data-generated/g2_prob_dens_above_zero.rds")

md2 <- md
md2$str_label <- as.factor(md2$str_label)
md2$str_label <- factor(md2$str_label, levels = md2$str_label[order(md2$strategy_mean_div)])

po <- rename(po, strategy = term) %>%
  inner_join(select(md2, strategy, str_label, strategy_mean_div))

# pl <- ggplot(po, aes(str_label, posterior)) + geom_violin() +
#   facet_wrap(~parameter, nrow=1) + ylab("") +
#   coord_flip() + theme_gg() +
#   theme(panel.grid.major = element_line(colour = "grey90",
#     size = 0.2), panel.grid.minor = element_line(colour = "grey98"))
# ggsave("portfolio/figs/stan-str-posteriors.pdf", width = 15, height = 9)

pos <- group_by(po, str_label, parameter) %>% summarise(
  l = quantile(posterior, probs = 0.025),
  l.5 = quantile(posterior, probs = 0.25),
  m = quantile(posterior, probs = 0.5),
  u.5 = quantile(posterior, probs = 0.75),
  u = quantile(posterior, probs = 0.975),
  strategy_mean_div = strategy_mean_div[1])

pos$line <- 0
pos[pos$parameter == "g0", "line"] <- NA
pos <- as_data_frame(pos)

pos$parameter <- factor(pos$parameter, levels = c("g0", "g1", "g2", "b1", "b2"))
pos$parameter <- factor(pos$parameter,
  labels = c("gamma[0][j]", "gamma[1][j]", "gamma[2][j]", "beta[1][j]", "beta[2][j]"))
pl <- ggplot(pos, aes(x = str_label, y = m)) +
  geom_point() +
  geom_pointrange(aes(ymin = l, ymax = u), size = 0.2) +
  geom_linerange(aes(ymin = l.5, ymax = u.5), size = 0.9) +
  facet_wrap(~parameter, nrow=1, labeller = label_parsed) +
      ylab("Parameter estimate") +
  geom_hline(aes(yintercept = line), lty = 2, col = "grey40") +
  coord_flip() + theme_gg() +
  theme(panel.grid.major = element_line(colour = "grey93",
    size = 0.2), panel.grid.minor = element_blank()) + xlab("")
ggsave("portfolio/figs/stan-str-posteriors-dot.pdf", width = 9.5, height = 6)
