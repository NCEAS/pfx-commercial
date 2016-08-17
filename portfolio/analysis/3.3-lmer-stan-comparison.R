library(dplyr)
library(ggplot2)
source("portfolio/analysis/prep-stan-model-matrix.R")
load("portfolio/data-generated/m.rda")
devtools::load_all("pfxr")
library(lme4)

b1 <- function(x, bp = 0) ifelse(x < bp, x, 0)
b2 <- function(x, bp = 0) ifelse(x < bp, 0, x)

m_lmer <- lmer(
  log(revenue) ~ -1 + b1(spec_change) + b2(spec_change) +
    days_change + b1(spec_change):days_change + b2(spec_change):days_change +
    (-1 + b1(spec_change) + b2(spec_change) |strategy) + (1|strategy_year),
  data = dat, offset = log(revenue.prev))

re_lmer <- coef(m_lmer)$strategy
re_lmer$strategy <- row.names(re_lmer)
row.names(re_lmer) <- NULL
library(tidyr)

p <- stanhelpers::extract_df(m)
colnames(p$coef_g0_strategy) <- md$strategy
colnames(p$coef_g1_strategy) <- md$strategy
colnames(p$coef_g2_strategy) <- md$strategy
colnames(p$coef_b1_strategy) <- md$strategy
colnames(p$coef_b2_strategy) <- md$strategy

# g0 <- tidyr::gather(p$coef_g0_strategy, term, posterior) %>%
#   mutate(parameter = "g0")
# g1 <- tidyr::gather(p$coef_g1_strategy, term, posterior) %>%
#   mutate(parameter = "g1")
# g2 <- tidyr::gather(p$coef_g2_strategy, term, posterior) %>%
#   mutate(parameter = "g2")
b1 <- tidyr::gather(p$coef_b1_strategy, term, posterior) %>%
  mutate(parameter = "b1")
b2 <- tidyr::gather(p$coef_b2_strategy, term, posterior) %>%
  mutate(parameter = "b2")
po <- bind_rows(g0, g1, g2, b1, b2)

pos <- group_by(po, term, parameter) %>% summarise(
  l = quantile(posterior, probs = 0.025),
  l.5 = quantile(posterior, probs = 0.25),
  m = quantile(posterior, probs = 0.5),
  u.5 = quantile(posterior, probs = 0.75),
  u = quantile(posterior, probs = 0.975)) %>%
  rename(strategy = term)

b1 <- filter(pos, parameter == "b1") %>%
  inner_join(select(re_lmer, strategy, `b1(spec_change)`)) %>%
  mutate(parameter = "b1") %>%
  rename(blmer = `b1(spec_change)`)
b2 <- filter(pos, parameter == "b2") %>%
  inner_join(select(re_lmer, strategy, `b2(spec_change)`)) %>%
  mutate(parameter = "b2") %>%
  rename(blmer = `b2(spec_change)`)
bs <- bind_rows(b1, b2)

g <- ggplot(bs, aes(blmer, y = m)) +
  geom_point(alpha = 0.6, size = 0.9) +
  geom_segment(aes(x = blmer, xend = blmer, y = l.5, yend = u.5), lwd = 1, alpha = 0.5) +
  geom_segment(aes(x = blmer, xend = blmer, y = l, yend = u), lwd = 0.2, alpha = 0.5) +
  coord_flip() +
  facet_wrap(~parameter) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  theme_gg() +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("lme4 estimate") +
  ylab("Stan estimate")
ggsave("portfolio/figs/lmer-vs-stan.pdf", width = 7, height = 3.4)
