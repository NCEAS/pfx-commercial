library(dplyr)
library(ggplot2)
library(rstan)
devtools::load_all("pfxr")

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m, output = "long_df") %>%
  filter(grepl("^b_j", variable) | grepl("^g0$", variable) |
      grepl("^h1", variable) | grepl("^g_k", variable)) %>%
  mutate(variable = as.character(variable)) %>%
  group_by(variable) %>%
  summarize(l = quantile(value, probs = 0.025),
    l.5 = quantile(value, probs = 0.25),
    m = quantile(value, probs = 0.5),
    u.5 = quantile(value, probs = 0.75),
    u = quantile(value, probs = 0.975))

# changed?
stopifnot(p$variable == c("b_j_1", "b_j_2", "b_j_3", "b_j_4", "b_j_5",
  "g_k_1", "g_k_2",
  "g_k_3", "g_k_4", "g_k_5", "g0", "h1"))

term_lu <- data.frame(
  variable = c("b_j_1", "b_j_2", "b_j_3", "b_j_4", "b_j_5", "g_k_1", "g_k_2",
    "g_k_3", "g_k_4", "g_k_5", "g0", "h1"),
  term_clean = c(
    "b1 (specializing)",
    "b2 (generalizing)",
    "b3 (days fished % change)",
    "b4 (days fished %:specializing)",
    "b5 (days fished %:generalizing)",

    "g1 (specializing, variability)",
    "g2 (generalizing, variability)",
    "g3 (days fished % change, variability)",
    "g4 (days fished %:specializing, variability)",
    "g5 (days fished %:generalizing, variability)",

    "g0 (global intercept, variability)",
    "u1 (strategy-level diversity effect, variability)"
  )
)
p$term_clean <- NULL
p <- inner_join(p, term_lu)
p$term_clean <- factor(p$term_clean,
  levels = rev(sort(as.character(p$term_clean))))

p1 <- ggplot(p, aes(x = term_clean, y = m)) +
  geom_point() +
  geom_pointrange(aes(ymin = l, ymax = u), size = 0.2) +
  geom_linerange(aes(ymin = l.5, ymax = u.5), size = 0.9) +
  ylab("Parameter estimate") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey60") +
  geom_vline(aes(xintercept = 7.5), lty = 2, col = "grey60") +
  geom_vline(aes(xintercept = 1.5), lty = 2, col = "grey60") +
  coord_flip() + theme_gg() + xlab("")
ggsave("portfolio/figs/stan-main-effects.pdf", width = 5.6, height = 4)
