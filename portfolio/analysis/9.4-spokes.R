library(dplyr)
library(ggplot2)
source("portfolio/analysis/prep-stan-model-matrix.R")
load("portfolio/data-generated/m.rda")
devtools::load_all("pfxr")

# -----------------------------------------------
# spoke plots:
mr <- group_by(dat, strategy) %>% summarise(strategy_med_rev = median(revenue)/1e3)
b <- broom::tidy(m, conf.int = T, estimate.method = "median", conf.level = 0.5)
md2 <- filter(b, grepl("coef_g0_strategy", term)) %>%
  mutate(strategy_id = 1:n()) %>% inner_join(md) %>% inner_join(mr)


permit_plot <- function(permit) {
  gre1 <- paste0(permit, " ")
  gre2 <- paste0(permit, "$")

  d_permit <-
    filter(md2, grepl(gre1, strategy) | grepl(gre2, strategy))

  permits <-
    sapply(d_permit$strategy, function(x)
      strsplit(x, " ")[[1]])
  d_permit$n_permits <- sapply(permits, function(x)
    n = length(x))

  if (max(d_permit$n_permits) > 3)
    stop("This function assumes a maximum of 3 permits per strategy.")

  single <- filter(d_permit, strategy == permit)
  two <- filter(d_permit, n_permits == 2)
  three <- filter(d_permit, n_permits == 3)

  single$b0_less <- single$strategy_med_rev
  single$g0_less <- single$estimate

  two$b0_less <- single$strategy_med_rev
  two$g0_less <- single$estimate

  for (i in seq_along(three$strategy)) {
    pr <- sub("([A-Z0-9]+ [A-Z0-9]+) [A-Z0-9]+", "\\1", three$strategy[i])
    if (sum(pr == two$strategy) == 1) {
      id <- which(pr == two$strategy)
    } else { # failed match
      pr <- sub("[A-Z0-9]+ ([A-Z0-9]+ [A-Z0-9]+)", "\\1", three$strategy[i])
      if (sum(pr == two$strategy) == 1) {
        id <- which(pr == two$strategy)
      } else { # failed match
        pr <- sub("([A-Z0-9]+ )[A-Z0-9]+ ([A-Z0-9]+)", "\\1\\2", three$strategy[i])
        if (sum(pr == two$strategy) == 1) {
          id <- which(pr == two$strategy)
        } else {
          stop("Failed match")
        }
      }
    }
    three$b0_less[i] <- two[id, "strategy_med_rev"]
    three$g0_less[i] <- two[id, "estimate"]
  }

  out <- bind_rows(single, two, three)
  out$single_permit <- permit
  out
}

sp <- plyr::ldply(c(
  "G01",
  "G34",
  "K91",
  "T91Q",
  "B61B",
  "C61B",
  "S01",
  "S03"
  ),
  permit_plot)
library(viridis)
library(ggrepel)

labs <- data.frame(single_permit = c("S03", "G34", "K91", "B61B"),
  single_permit_clean =
    c("Salmon, drift gillnet", "Herring roe, gillnet", "King crab", "Halibut"))
sp <- inner_join(sp, labs)
sp$single_permit_clean <- factor(sp$single_permit_clean,
  levels =  c(
    "Salmon, drift gillnet",
    "Herring roe, gillnet",
    "King crab",
    "Halibut"))

pl <- ggplot(sp,
  aes(strategy_med_rev, exp(estimate), yend = exp(g0_less), xend = b0_less,
    label = str_label, colour = strategy_mean_div)) +
  geom_segment(aes(x = strategy_med_rev, xend = strategy_med_rev,
    y = exp(conf.low), yend = exp(conf.high)), alpha = 0.6, colour = "grey50") +
  # geom_point() +
  geom_segment() +
  geom_text(size = 3) +
  facet_wrap(~single_permit_clean, scales = "free") +
  theme_gg() +
  labs(x = "Median revenue", y = "Estimated revenue variability",
    colour = "Mean sp.\ndiversity") +
  scale_color_viridis() +
  # theme(legend.position = "right") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  scale_x_continuous(expand = c(.2, .2))
# print(pl)
ggsave("portfolio/figs/stan-gg-spoke2.pdf", width = 7, height = 6)
