library(dplyr)
library(ggplot2)
load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m-2016-08-23.rda")
devtools::load_all("pfxr")

# -----------------------------------------------
# spoke plots:
# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m)

# exp(g0 + unique(g0j)[1] + xh1[1] * h1 + xh2[2] * h2)
g0_temp <- p$g0_strategy %>%
  tidyr::gather(strategy, posterior) %>%
  mutate(strategy_id =
      as.numeric(sub("g0_strategy_([0-9]+)", "\\1", strategy))) %>%
  select(-strategy) %>%
  inner_join(md) %>%
  mutate(
    h1 = rep(p$h1[[1]], nrow(md)),
    h2 = rep(p$h2[[1]], nrow(md)),
    g0main = rep(p$g0[[1]], nrow(md))) %>%
  rename(g0j = posterior)

# controlling for days only:
g0_temp <- mutate(g0_temp, posterior =
    g0main +
    (g0j + h1 * scaled_strategy_mean_div -
        h2 * scaled_strategy_mean_days)/2)

md2 <- g0_temp %>% group_by(strategy) %>%
  summarise(
    estimate = median(posterior),
    conf.low = quantile(posterior, probs = 0.05),
    conf.high = quantile(posterior, probs = 0.95)) %>%
  ungroup() %>%
  inner_join(md)

nperms <- group_by(dat, strategy) %>%
  summarise(strategy_med_rev = median(revenue)/1e3,
    n_permits = npermit[1])

md2 <- inner_join(md2, nperms)

permit_plot <- function(permit) {
  message(permit)
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
    three$b0_less[i] <- as.numeric(two[id, "strategy_med_rev"])
    three$g0_less[i] <- as.numeric(two[id, "estimate"])
  }

  out <- bind_rows(single, two, three)
  out$single_permit <- permit

  out
}

sp <- plyr::ldply(c(
  # "G01",
  "G34",
  "K91",
  "T09",
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
    c("Salmon, drift gillnet", "bold(B)~Herring~roe~gillnet", "King crab", "bold(A)~Halibut"))
sp <- inner_join(sp, labs)
sp$single_permit_clean <- factor(sp$single_permit_clean,
  levels =  c(
    "bold(A)~Halibut",
    "Salmon, drift gillnet",
    "bold(B)~Herring~roe~gillnet",
    "King crab"
    ))
ns <- dat %>% group_by(strategy) %>%
  summarise(nn = length(unique(p_holder)))
sp <- inner_join(sp, ns)

let <- data_frame(x = rep(min(sp$strategy_med_rev[sp$single_permit == "B61B"]), 2),
  y = rep(.7, 2),
  single_permit_clean = unique(sp$single_permit_clean)[c(3, 1)],
  labs=LETTERS[1:2])

pl <- filter(sp, !single_permit %in% c("S03", "K91")) %>%
  mutate(str_label = ifelse(nn > 10, str_label, NA)) %>%
  ggplot(
  aes(strategy_med_rev, exp(estimate), yend = exp(g0_less), xend = b0_less,
    label = str_label, colour = as.factor(n_permits))) +
  geom_segment(aes(x = strategy_med_rev, xend = strategy_med_rev,
    y = exp(conf.low), yend = exp(conf.high)), alpha = 0.5, size = 0.3,
    colour = "grey60") +
  geom_segment(colour = "grey70", alpha = 0.6, size = 0.5) +
  geom_point(aes(size = nn)) +
  # geom_point(data =
  #     filter(sp, n_permits == 1, !single_permit %in% c("S03", "K91")),
  #   aes(size = nn), pch = 21, col = "black") +
  geom_text_repel(size = 2.7,
    point.padding = unit(0.15, "lines"), max.iter = 9e3, segment.size = 0,
    box.padding = unit(0.15, "lines"), nudge_y = -0.03,
    # label.r = unit(0, "lines"), label.size = 0.15, fill = "#FFFFFF40",
    colour = "grey20") +
  # geom_text(size = 2.9, colour = "grey20", vjust = "inward", hjust = "inward") +
  facet_wrap(~single_permit_clean, scales = "free", labeller=label_parsed) +
  theme_gg() +
  labs(x = "Median revenue ($1000/year)", y = "Estimated revenue variability",
    colour = "Number of\npermits",
    size = "Permit\nholders") +
  # scale_color_viridis() +
  scale_size_continuous() +
  # scale_fill_viridis(guide = FALSE) +
  # scale_colour_manual(values = gg_color_hue(3, l = 65)) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(4, "Blues")[4:2]) +
  # scale_color_brewer(palette = "YlGnBu") +
  # theme(legend.position = "right") +
  theme(
# legend.justification = c(1, 1), legend.position = c(1, 1),
    legend.title = element_text(size = rel(0.75)),
    strip.text.x = element_text(hjust = 0)) +
  scale_x_continuous(expand = c(0.05, 0.07)) +
  guides(
    colour = guide_legend(override.aes = list(size=3.5), order = 1),
    size = guide_legend(order = 2, override.aes = list(pch = 21)))
  # geom_ann("text", data = let, aes(x, y, label = labs,
    # yend = NULL, xend = NULL, colour = NULL), size = 5)
# print(pl)
ggsave("portfolio/figs/stan-gg-spoke2.pdf", width = 7, height = 3)


# ------------------------------------------------------
# more for SI
sp <- plyr::ldply(c(
  # "G01",
  "G34",
  "K91",
  "T09",
  "B61B",
  "C61B",
  "S01",
  "S03"
),  permit_plot)

labs <- readr::read_csv("data/strategies-labels.csv") %>%
  rename(single_permit_clean = description,
    single_permit = strategy) %>%
  select(-str_label)
sp <- inner_join(sp, labs, by = "single_permit")

ns <- dat %>% group_by(strategy) %>%
  summarise(nn = length(unique(p_holder)))
sp <- inner_join(sp, ns)

pl <- sp %>%
  mutate(str_label = ifelse(nn > 2, str_label, NA)) %>%
  ggplot(
    aes(strategy_med_rev, exp(estimate), yend = exp(g0_less), xend = b0_less,
      label = str_label, colour = as.factor(n_permits))) +
  geom_segment(aes(x = strategy_med_rev, xend = strategy_med_rev,
    y = exp(conf.low), yend = exp(conf.high)), alpha = 0.5, size = 0.3,
    colour = "grey60") +
  geom_segment(colour = "grey70", alpha = 0.6, size = 0.5) +
  geom_point(aes(size = nn)) +
  # geom_point(data = filter(sp, n_permits == 1), aes(size = nn), pch = 21, col = "black") +
  geom_text_repel(size = 2.7,
    point.padding = unit(0.15, "lines"), max.iter = 9e3, segment.size = 0,
    box.padding = unit(0.15, "lines"), nudge_y = -0.03,
    colour = "grey20") +
  facet_wrap(~single_permit_clean, scales = "free") +
  theme_gg() +
  labs(x = "Median revenue ($1000/year)", y = "Estimated revenue variability",
    colour = "Number of\npermits",
    size = "Permit\nholders") +
  scale_size_continuous() +
  scale_colour_manual(values = RColorBrewer::brewer.pal(4, "Blues")[4:2]) +
  theme(
    legend.title = element_text(size = rel(0.75)),
    strip.text.x = element_text(hjust = 0, size = 8)) +
  scale_x_continuous(expand = c(0.05, 0.07)) +
  guides(
    colour = guide_legend(override.aes = list(size=3.5), order = 1),
    size = guide_legend(order = 2, override.aes = list(pch = 21)))
ggsave("portfolio/figs/stan-gg-spoke-all.pdf", width = 9, height = 7)
