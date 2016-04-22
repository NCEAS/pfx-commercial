# setwd("../")
library(dplyr)
library(ggplot2)
library(viridis)
source("analysis/plot_cons_plans.R")
source("analysis/downside-risk.R")

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

breaks <- c(1, 1.01, 1.5, 2, 2.5, 3, 3.5, 10)

species_diversity <-
  group_by(species_diversity_by_year, p_holder) %>%
  mutate(returns = c(NA, diff(log(totIndRev)))) %>%
  na.omit %>%
  summarize(
    # diversity_by_frequency = mean(eff.freq),
    diversity_by_earnings = mean(eff.earn),
    # diversity_by_weight = mean(eff.lbs),

    m = log10(mean(totIndRev)),
    cv = sd(totIndRev) / mean(totIndRev),
    semideviation = semi_deviation(log10(totIndRev)),
    cvar = -cvar(log10(totIndRev)),

    m_returns = mean(returns),
    var_returns = var(returns),
    semivariance_returns = semi_variance(returns),
    cvar_returns = -cvar(returns)
  ) %>%
  filter(!is.na(cv)) %>%
  mutate(diversity_group = cut(diversity_by_earnings, breaks = breaks,
    right = FALSE))

# Now try grouping this in various ways
portfolio_window <- list()
ii <- 1
for (yr in seq(1985, 2004, 3)) {
  message(yr)
  portfolio_window[[ii]] <- species_diversity_by_year %>%
    filter(year %in% yr:(yr + 10)) %>%
    group_by(p_holder) %>%
    mutate(returns = c(NA, diff(log(totIndRev)))) %>%
    na.omit %>%
    summarize(
      # diversity_by_frequency = mean(eff.freq),
      diversity_by_earnings = mean(eff.earn),
      # diversity_by_weight = mean(eff.lbs),

      m = log10(mean(totIndRev)),
      cv = sd(totIndRev) / mean(totIndRev),
      semivariance = semi_variance(log10(totIndRev)),
      cvar = cvar(log10(totIndRev)),

      m_returns = mean(returns),
      var_returns = var(returns),
      semivariance_returns = semi_variance(returns),
      cvar_returns = cvar(returns)
  ) %>%
    filter(!is.na(cv)) %>%
    mutate(diversity_group = cut(diversity_by_earnings, breaks = breaks,
  right = FALSE))
    portfolio_window[[ii]]$window <- yr
    ii <- ii + 1
}
portfolio_window_data_frame <- bind_rows(portfolio_window)

plot_polygons <- function(polygon_data, x_column, y_column,
  xlab = "Variance", ylab = "Mean", prob = 0.75) {
  polygon_data <- plyr::ddply(polygon_data, "diversity_group",
    function(x) add_dens_polygon(x[,x_column], x[,y_column], plot = FALSE, alpha = c(0.5, prob)))
  p <- polygon_data %>%
    ggplot(aes(x, y, color = diversity_group, fill = diversity_group)) +
    geom_polygon(alpha = 0.3) +
    scale_fill_viridis(discrete = TRUE) + scale_color_viridis(discrete = TRUE) +
    theme_bw() +
    xlab(xlab) + ylab(ylab)
  print(p)
}

plot_polygons_facets <- function(polygon_data, x_column, y_column,
  xlab = "Variance", ylab = "Mean", prob = 0.75) {
  polygon_data <- plyr::ddply(polygon_data, c("diversity_group", "window"),
    function(x) add_dens_polygon(x[,x_column], x[,y_column], plot = FALSE, alpha = c(0.5, prob)))
  p <- polygon_data %>%
    ggplot(aes(x, y, color = diversity_group, fill = diversity_group)) +
    geom_polygon(alpha = 0.3) +
    scale_fill_viridis(discrete = TRUE) + scale_color_viridis(discrete = TRUE) +
    theme_bw() +
    facet_wrap(~window) +
    xlab(xlab) + ylab(ylab)
  print(p)
}

plot_polygons(species_diversity, "cv", "m",
  xlab = "CV of gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cv.pdf", width = 6.5, height = 5)

plot_polygons(species_diversity, "semideviation", "m",
  xlab = "Semideviation of log10 gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-semideviation.pdf", width = 6.5, height = 5)

plot_polygons(species_diversity, "cvar", "m",
  xlab = "Expected shortfall (95%) of gross earnings returns",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cvar.pdf", width = 6.5, height = 5)

plot_polygons(species_diversity, "cvar_returns", "m",
  xlab = "-1 * Expected shortfall (95%) of gross earnings returns",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cvar-returns.pdf", width = 6.5, height = 5)

plot_polygons_facets(portfolio_window_data_frame,
  "cv", "m",
  xlab = "Coefficient of variation of gross earnings",
  ylab = "log10 of mean gross earnings")
ggsave("figs/portfolio-gross-earnings-cv-time-window.pdf", width = 12, height = 10)

# TODO
# - [x] try a downside risk (semivariance, cvar)
# - [x] try viridis colors
# - [ ] make plots by various groups: goa-se alaska, boat size, ear
# - [x] make plots across time windows
# - [x] try making the plots in ggpllllt2

