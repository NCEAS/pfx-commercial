library(dplyr)

if (!exists("cfec")) {
  cfec <- feather::read_feather("data/cfec.feather")
}

simp.div <- function(x) {
  1 / sum((x / sum(x)) ^ 2)
}

effectiveDiversity_by_personYear <- function(dataFrame, variable) {
  group_by_(dataFrame, variable, "year", "p_holder") %>%
    summarize(
      totRev = sum(g_earn, na.rm = TRUE),
      totN = n(),
      totWeight = sum(g_pounds, na.rm = TRUE),
      days = length(unique(day))
    ) %>%
    as.data.frame() %>%
    filter(is.finite(totRev) &
        is.finite(totN) & is.finite(totWeight)) %>%
    as.data.frame() %>%
    group_by(p_holder, year) %>%
    summarize(
      eff.freq = simp.div(totN),
      eff.earn = simp.div(totRev),
      eff.lbs = simp.div(totWeight),
      totIndRev = sum(totRev),
      nCalDays = sum(days)
    ) %>%
    filter(!is.na(eff.freq) &
        !is.na(eff.earn) & !is.na(eff.lbs)) %>%
    as.data.frame()
}

species_diversity_by_year <-
  effectiveDiversity_by_personYear(cfec, variable = "specn")
breaks = c(1, 1.01, 1.5, 2, 2.5, 3, 3.5, 4, 10)
species_diversity <-
  group_by(species_diversity_by_year, p_holder) %>%
  summarize(
    # diversity_by_frequency = mean(eff.freq),
    diversity_by_earnings = mean(eff.earn),
    # diversity_by_weight = mean(eff.lbs),
    m = log10(mean(totIndRev)),
    v = sd(totIndRev) / mean(totIndRev)
  ) %>%
  filter(!is.na(v)) %>%
  mutate(diversity_group = cut(diversity_by_earnings, breaks = breaks,
    right = FALSE))

species_diversity_list <- split(species_diversity,
  species_diversity$diversity_group)

pdf("../figs/earnings_portfolio_by_species_diversity.pdf", height = 5, width = 5)
par(cex = 0.8)
metafolio::plot_cons_plans(
  species_diversity_list,
  plans_name = names(species_diversity_list),
  cols = RColorBrewer::brewer.pal(length(names(species_diversity_list)),
    "YlOrRd"),
  xlab = "CV of gross earnings",
  ylab = "log10 of gross earnings",
  add_all_efs = FALSE)
dev.off()
metafolio::plot_cons_plans(
  species_diversity_list,
  plans_name = names(species_diversity_list),
  cols = RColorBrewer::brewer.pal(length(names(species_diversity_list)),
    "YlOrRd"),
  xlab = "CV of gross earnings",
  ylab = "log10 of gross earnings",
  add_all_efs = FALSE)

# TODO
# - [ ] try a downside risk (semivariance, cvar)
# - [ ] try viridis colors
# - [ ] make plots by various groups: goa-se alaska, boat size, ear
# - [ ] make plots across time windows
