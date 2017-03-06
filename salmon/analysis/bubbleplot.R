dat.annual <- read.table("salmon/data-generated/bubble_plot_data_fig2.csv", stringsAsFactors = FALSE, header = TRUE)

dat.annual <- mutate(dat.annual,
  gear = factor(gear, levels = c("Drift gillnet", "Set gillnet", "Hand troll",
    "Power troll", "Purse seine")))

l <- tibble::tribble(~let, ~label,
  "H", "CI",
  "E", "PWS",
  "L", "CH",
  "K", "KO",
  "M", "AP",
  "T", "BB",
  "A", "SE",
  "B", "State",
  "Z", "NS",
  "W", "KU",
  "D", "YAK",
  "Y", "YUK")

dat.annual <- mutate(dat.annual,
  let = sub("S[0-9]+([A-Z])", "\\1", dat.annual$strategy_permit)) %>%
  left_join(l) %>%
  filter(strategy_permit != "S04Y")

library(tidyverse)
library(ggrepel)
# devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)
dir.create("salmon/figs", showWarnings = FALSE)

g <- ggplot(dat.annual,
  aes(x = mean_rev/1e3, y = cv_rev,
    label = label)) +
  geom_point(alpha = 0.5, aes(fill = gear, size = diversity), pch = 21,
    colour = "grey20") +
  scale_size(range = c(1.5, 15), breaks = seq(1.2, 2.4, 0.4)) +
  guides(fill = guide_legend(override.aes = list(size = 4, alpha = 0.7), order = 1)) +
  geom_text_repel(size = 2.6, colour = "grey20",
    alpha = 1, segment.color = "grey70",
    point.padding = unit(0, "lines"), max.iter = 1e4, segment.size = 0.3) +
  scale_fill_brewer(palette = "Set3") +
  ylab("CV annual revenue") + xlab("Mean revenue ($1000)") +
  labs(fill = "Gear", size = "Diversity") +
  scale_y_continuous(breaks = seq(0.2, 1, 0.2)) +
  theme_sleek() #+
  # scale_x_log10()
# print(g1)

ggsave("salmon/figs/bubbleplot.pdf", width = 5, height = 3.6)
