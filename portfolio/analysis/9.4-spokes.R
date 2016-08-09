library(dplyr)

source("portfolio/analysis/prep-stan-model-matrix.R")

load("portfolio/data-generated/m.rda")

# -----------------------------------------------
# spoke plots:
mr <- group_by(dat, strategy) %>% summarise(strategy_med_rev = median(revenue)/1e3)
hist(filter(dat, strategy=="M61B")$revenue/1e6)
b <- broom::tidy(m, conf.int = F, estimate.method = "median", rhat = F, ess = F)
md2 <- filter(b, grepl("coef_g0_strategy", term)) %>%
  mutate(strategy_id = 1:n()) %>% inner_join(md) %>% inner_join(mr)

permit_plot <- function(permit) {
  gre1 <- paste0(permit, " ")
  gre2 <- paste0(permit, "$")
  d_permit <- filter(md2, grepl(gre1, strategy) | grepl(gre2, strategy))
  single <- filter(md2, strategy == permit) %>%
    rename(b0_single = strategy_med_rev, g0_single = estimate)
  d_permit$b0_single <- single$b0_single
  d_permit$g0_single <- single$g0_single
  ggplot(d_permit,
    aes(strategy_med_rev/1e3, exp(estimate),
      yend = exp(g0_single), xend = b0_single/1e3,
      label = strategy, colour = strategy_mean_div)) +
    geom_text() +
    geom_segment() + theme(legend.position="none")
}

p1 <- permit_plot("G01")
p2 <- permit_plot("G34")
p3 <- permit_plot("K91")
p4 <- permit_plot("T91Q")
p5 <- permit_plot("B61B")
p6 <- permit_plot("C61B")
p7 <- permit_plot("S01")
p8 <- permit_plot("S03")

pdf("portfolio/figs/stan-gg-spoke-offset.pdf", width = 12, height = 12)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8)
dev.off()

