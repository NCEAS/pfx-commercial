library(dplyr)
library(ggplot2)
library(rstan)
devtools::load_all("pfxr")

load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m.rda")

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m, output = "long_df") %>%
  filter(grepl("^b_j", variable) | grepl("^g0$", variable) |
      grepl("^h[1-2]", variable) | grepl("^g_k", variable)) %>%
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
  "g_k_3", "g_k_4", "g_k_5", "g0", "h1", "h2"))

term_lu <- data.frame(
  variable = c("b_j_1", "b_j_2", "b_j_3", "b_j_4", "b_j_5", "g_k_1", "g_k_2",
    "g_k_3", "g_k_4", "g_k_5", "g0", "h1", "h2"),
  term_clean = c(
    "β1 (specializing)",
    "β2 (generalizing)",
    "β3 (days fished % change)",
    "β4 (days fished %:specializing)",
    "β5 (days fished %:generalizing)",

    "γ1 (specializing, variability)",
    "γ2 (generalizing, variability)",
    "γ3 (days fished % change, variability)",
    "γ4 (days fished %:specializing, variability)",
    "γ5 (days fished %:generalizing, variability)",

    "γ0 (global intercept, variability)",
    "η1 (strategy-level diversity effect, variability)",
    "η2 (strategy-level days-fished effect, variability)"
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
  geom_vline(aes(xintercept = 8.5), lty = 2, col = "grey60") +
  geom_vline(aes(xintercept = 2.5), lty = 2, col = "grey60") +
  coord_flip() + theme_gg() + xlab("")
# ggsave("portfolio/figs/stan-main-effects.pdf", width = 5.6, height = 4)
cairo_pdf("portfolio/figs/stan-main-effects.pdf", width = 5.6, height = 4)
print(p1)
dev.off()
