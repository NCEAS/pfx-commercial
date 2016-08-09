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
  facet_wrap(~parameter, nrow=1) + ylab("") +
  coord_flip() + theme_gg() +
  theme(panel.grid.major = element_line(colour = "grey90",
    size = 0.2), panel.grid.minor = element_line(colour = "grey98"))
ggsave("portfolio/figs/stan-str-posteriors.pdf", width = 15, height = 9)

