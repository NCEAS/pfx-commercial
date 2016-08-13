# This figure is meant to illustrate increased specialization

# number of permits / person
library(dplyr)
library(ggplot2)
devtools::load_all("pfxr")

make_fig1_dat <- function() {
  # cfec.feather already inflation adjusted
  if (!exists("cfec")) {
    cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
    cfec$year <- as.numeric(cfec$year)
    # add indicator for single permit landings
    cfec = group_by(cfec, year, p_holder) %>%
      mutate(n.permit = length(unique(p_fshy))) %>%
      mutate(permits = ifelse(n.permit==1, "Single","Multiple")) %>%
      select(-n.permit)
  }

  # ------------------------------------------------
  cfec <- mutate(cfec, sp_id = substr(p_fshy, 1, 1))
  es = group_by(cfec, year, p_holder, specn) %>%
    summarize(earn = sum(g_earn), spp = paste(sp_id, collapse = ""))
  es2 = group_by(es, year, p_holder) %>%
    summarize(effSpec = simp.div(earn), spp = paste(spp, collapse = ""))

  usp <- sort(table(cfec$sp_id)) # unique species
  usp <- names(usp[usp > 75000]) # keep those with > 75,000 records
  usp <- c("x", usp) # add `x` which won't match and therefore get overall values

  es2$unique_spp <- unlist(lapply(lapply(
    strsplit(es2$spp, ""), unique),
    paste, collapse = ""))

  out_div <- plyr::ldply(usp, function(x) {
    message(x)
    filter(es2, !grepl(x, unique_spp)) %>%
      group_by(year) %>%
      summarize(mean_spdiv = mean(effSpec, na.rm = T), jk = x)
  })

  p0a_dat <- group_by(cfec, year, permits) %>%
    summarize(totEarn = sum(g_earn)/1e9,
      salmEarn = sum(g_earn[substr(p_fshy,1,1) == "S"])/1e9,
      nonsalmEarn = sum(g_earn[substr(p_fshy,1,1) != "S"])/1e9)

  # ------------------------------------------------
  permits_person_year <- group_by(cfec, p_holder, year, sp_id) %>%
    filter(g_earn > 0) %>%
    summarize(npermit = length(unique(p_fshy)))

  permits_person_year2 <- group_by(permits_person_year, p_holder, year) %>%
    summarize(npermit = sum(npermit), spp = paste(sp_id, collapse = ""))

  out_permits <- plyr::ldply(usp, function(x) {
    message(x)
    filter(permits_person_year2, !grepl(x, spp)) %>%
      mutate(npermit_one = npermit == 1) %>%
      group_by(year) %>%
      summarize(prop_single = sum(npermit_one)/n(), jk = x)
  })

  save(out_div, out_permits, p0a_dat,
    file = "portfolio/data-generated/fig1-dat.rda")
}

# ----------------------------
# plots

if (!file.exists("portfolio/data-generated/fig1-dat.rda")) make_fig1_dat()

load("portfolio/data-generated/fig1-dat.rda")

cols <- rep(RColorBrewer::brewer.pal(4, "Blues")[3], 99)

pl1 <- filter(out_div, jk != "x", jk != "S") %>%
  ggplot(aes(year, mean_spdiv, group = jk)) +
  geom_line(colour = "grey70", alpha = 0.8) +
  geom_line(data = filter(out_div, jk == "x"),
    aes(year, mean_spdiv), colour = "black", lwd = 1.2) +
  geom_line(data = filter(out_div, jk == "S"),
    aes(year, mean_spdiv), colour = cols[2]) +
  theme_gg() +
  xlab("Year") +
  ylab("Mean species diversity") +
  annotate("text", x = 2000, y = 1.23, label = "Exclude\nsalmon",
    colour = cols[2], size = 3.2)
# print(pl1)

pl2 <- filter(out_permits, jk != "x", jk != "S", jk != "B") %>%
  # pl2 <- filter(out_permits, jk != "x") %>%
  ggplot(aes(year, prop_single, group = jk)) +
  geom_line(colour = "grey70", alpha = 0.8) +
  geom_line(data = filter(out_permits, jk == "S"),
    aes(year, prop_single), colour = cols[2]) +
  geom_line(data = filter(out_permits, jk == "B"),
    aes(year, prop_single), colour = cols[2]) +
  geom_line(data = filter(out_permits, jk == "x"),
    aes(year, prop_single), colour = "black", lwd = 1.2) +
  theme_gg() +
  xlab("Year") +
  ylab("Proportion single permit fishers") +
  annotate("text", x = 2010, y = 0.67, label = "Exclude\nsalmon",
    colour = cols[2], size = 3.2) +
  annotate("text", x = 1999, y = 0.9, label = "Exclude halibut",
    colour = cols[2], size = 3.2)
# print(pl2)

pl3 <- p0a_dat %>%
  ggplot(aes(year, totEarn, group = permits, color = permits)) +
  geom_line(lwd = 0.95) +
  ylab("Total revenue (billions)") +
  theme_gg() +
  scale_color_manual(values = c("grey40", cols[1])) +
  theme(legend.position="none") +
  scale_y_continuous(breaks = c(0.4, 0.6, 0.8, 1, 1.2)) +
  xlab("Year") +
  annotate("text", x = 2005, y = 0.92, label = "Multiple permits",
    colour = "grey40", size = 3.2) +
  annotate("text", x = 1990, y = 0.5, label = "Single\npermit",
    colour = cols[1], size = 3.2)

library(maps)
library(mapdata)
coast_map <- fortify(map("worldHires", fill=TRUE, plot=FALSE))
coast_map <- filter(coast_map, region %in% c("USA", "Canada"))

# downsample resolution:
eo <- rep(c(1L,2L,3L,4L,5L,6L), 2e6) # keep 1/6 of points
coast_map <- coast_map %>% group_by(group) %>%
  mutate(id = eo[seq_along(order)]) %>%
  filter(id == 1L) %>%
  mutate(order = seq_along(order)) %>%
  select(-id)

pts <- readr::read_csv("data/map-points.csv")

gg <- ggplot()
gg <- gg + geom_map(data=coast_map, map=coast_map,
  aes(x=long, y=lat, map_id=region),
  fill="grey75", color="grey75")
gg <- gg + xlim(-181.5,-129) + ylim(50, 71)
gg <- gg + coord_map()
gg <- gg + coord_map()
gg <- gg + theme_gg()
gg <- gg + geom_point(data = pts, aes(-long, lat), colour = cols[2]) +
  ggrepel::geom_text_repel(data = pts, aes(-long, lat, label = name),
    colour = "grey15", segment.size = 0.3, segment.color = "grey50",
    size = 3.2)
gg <- gg + annotate("text", x = -147, y = 66, label = "Alaska",
  colour = "grey15", size = 4.2)
gg <- gg + xlab("Longitude") + ylab("Latitude")
# gg

pdf("portfolio/figs/fig1-jk.pdf", width = 7, height = 6)
print(cowplot::plot_grid(gg, pl3, pl2, pl1, labels = "AUTO",
  ncol = 2, hjust = -5, vjust = 2.3))
dev.off()
