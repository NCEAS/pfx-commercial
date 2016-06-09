library(dplyr)
library(ggplot2)
library(viridis)
source("analysis/downside-risk.R")
source("analysis/contours.R")

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
      days = length(unique(day)),
      mean_vessel_length = mean(vlength),
      area = ifelse(length(unique(area)) == 1, area[1], NA_character_),
      pollock = ifelse("PLCK" %in% spec, TRUE, FALSE),
      region = ifelse(length(unique(region)) == 1, region[1], NA_character_),
      taxa = ifelse(length(unique(taxa_broad)) == 1, taxa_broad[1], NA_character_),
      salmon = ifelse(length(unique(salmon)) == 1, salmon[1], as.logical(NA))
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
      nCalDays = sum(days),
      mean_vessel_length = mean(mean_vessel_length),
      area = ifelse(length(unique(area)) == 1, area[1], NA_character_),
      pollock = ifelse(TRUE %in% pollock, TRUE, FALSE),
      region = ifelse(length(unique(region)) == 1, region[1], NA_character_),
      taxa = ifelse(length(unique(taxa)) == 1, taxa[1], NA_character_),
      salmon = ifelse(length(unique(salmon)) == 1, salmon[1], as.logical(NA))
    ) %>%
    filter(!is.na(eff.freq) &
        !is.na(eff.earn) & !is.na(eff.lbs)) %>%
    as.data.frame()
}

if (!file.exists("generated-data/species_diversity_by_year.rds")) {
  species_diversity_by_year <-
    effectiveDiversity_by_personYear(cfec, variable = "specn")
  saveRDS(species_diversity_by_year, file = "generated-data/species_diversity_by_year.rds")
} else {
  species_diversity_by_year <- readRDS("generated-data/species_diversity_by_year.rds")
}

calculate_metrics <- function(x, breaks = c(1, 1.01, 1.5, 2, 2.5, 10) {
  x %>%
    group_by(p_holder) %>%
    mutate(returns = c(NA, diff(log(totIndRev)))) %>%
    filter(!is.na(returns)) %>%
    summarize(
      # diversity_by_frequency = mean(eff.freq),
      diversity_by_earnings = mean(eff.earn),
      # diversity_by_weight = mean(eff.lbs),

      m = log10(mean(totIndRev)),
      cv = sd(totIndRev) / mean(totIndRev),
      semideviation = semi_deviation(log10(totIndRev)),
      semideviation_upside = semi_deviation(log10(totIndRev), downside = FALSE),
      cvar = cvar(log10(totIndRev)),

      m_returns = mean(returns),
      var_returns = var(returns),
      semivariance_returns = semi_variance(returns),
      cvar_returns = cvar(returns)
  ) %>%
    filter(!is.na(cv)) %>%
    mutate(diversity_group = cut(diversity_by_earnings, breaks = breaks,
  right = FALSE))
}

species_diversity <- calculate_metrics(species_diversity_by_year)
saveRDS(species_diversity, file = "generated-data/species_diversity_metrics.rds")

# Now try grouping this in various ways
# Grouped over time:
portfolio_window <- list()
ii <- 1
for (yr in seq(1985, 2004, 3)) {
  message(yr)
  portfolio_window[[ii]] <- species_diversity_by_year %>%
    filter(year %in% yr:(yr + 10)) %>%
    calculate_metrics()
  portfolio_window[[ii]]$window <- yr
  ii <- ii + 1
}
portfolio_window_data_frame <- bind_rows(portfolio_window)

portfolio_by_taxa <- species_diversity_by_year %>%
  plyr::ddply("taxa", calculate_metrics)

portfolio_by_region <- species_diversity_by_year %>%
  plyr::ddply("region", calculate_metrics)

portfolio_by_salmon <- species_diversity_by_year %>%
  plyr::ddply("salmon", calculate_metrics)
portfolio_by_salmon <- filter(portfolio_by_salmon, !is.na(salmon))

vessel_length_cuts <-
  quantile(species_diversity_by_year$mean_vessel_length, probs =
  c(0, 0.33, 0.66, 1), na.rm = TRUE)

species_diversity_by_year <- mutate(species_diversity_by_year,
  vessel_length_category = cut(mean_vessel_length, breaks = vessel_length_cuts,
  right = FALSE))

portfolio_by_vlength <- species_diversity_by_year %>%
  plyr::ddply("vessel_length_category", calculate_metrics)

portfolio_by_pollock <- species_diversity_by_year %>%
  plyr::ddply("pollock", calculate_metrics)

plot_polygons(species_diversity, "cv", "m",
  xlab = "CV of gross earnings", n = 50,
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cv.pdf", width = 6.5, height = 5)

plot_polygons(species_diversity, "semideviation", "m",
  xlab = "Semideviation of log10 gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-semideviation.pdf", width = 6.5, height = 5)

plot_polygons(species_diversity, "semideviation_upside", "m",
  xlab = "Semideviation (upside) of log10 gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-semideviation-upside.pdf", width = 6.5, height = 5)

plot_polygons(species_diversity, "cvar", "m",
  xlab = "-1 * Expected shortfall (95%) of gross earnings returns",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cvar.pdf", width = 6.5, height = 5)

plot_polygons(species_diversity, "cvar_returns", "m",
  xlab = "-1 * Expected shortfall (95%) of gross earnings returns",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cvar-returns.pdf", width = 6.5, height = 5)

plot_polygons(portfolio_window_data_frame,
  "cv", "m",
  grouping = c("diversity_group", "window"),
  xlab = "Coefficient of variation of gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cv-time-window.pdf", width = 10, height = 10)

plot_polygons(portfolio_by_taxa, "cv", "m",
  grouping = c("diversity_group", "taxa"),
  xlab = "Coefficient of variation of gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cv-taxa.pdf", width = 9, height = 9)

# plot_polygons(portfolio_by_region, "cv", "m",
#   grouping = c("diversity_group", "region"),
#   xlab = "Coefficient of variation of gross earnings",
#   ylab = "log10 of mean gross earnings")
# ggsave("figs/portfolio-gross-earnings-cv-taxa.pdf", width = 9, height = 5)

plot_polygons(portfolio_by_salmon, "cv", "m",
  grouping = c("diversity_group", "salmon"),
  xlab = "Coefficient of variation of gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cv-salmon.pdf", width = 9, height = 5)

plot_polygons(portfolio_by_vlength, "cv", "m",
  grouping = c("diversity_group", "vessel_length_category"),
  xlab = "Coefficient of variation of gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cv-vessel-length.pdf", width = 10, height = 9)

plot_polygons(portfolio_by_pollock, "cv", "m",
  grouping = c("diversity_group", "pollock"),
  xlab = "Coefficient of variation of gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cv-pollock.pdf", width = 9, height = 5)
# group_by(portfolio_by_area, diversity_group, area) %>% summarize(n = n()) %>% as.data.frame() %>% arrange(area, diversity_group)

# TODO
# - [x] try a downside risk (semivariance, cvar)
# - [x] try viridis colors
# - [x] make plots by various groups: goa-se alaska, boat size, groundfish
#       versus salmon, invertebrates versus fish, pelagic versus benthic, gear type
# - [ ] consider doing cvar from a mean or a running mean
# - [x] make plots across time windows
# - [x] try making the plots in ggpllllt2
# - [x] make a plot of upper semideviation
# - [ ] remove other category from species
# - [ ] try modeling the 2 dimensions
# - [x] try pulling out pollock
# - [x] make sure I'm working with the latest data
# - [ ] allow for option for adding points or not
