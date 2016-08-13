
# This figure is meant to illustrate increased specialization

# number of permits / person
library(dplyr)
library(knitr)
library(ggplot2)
library(date)
devtools::load_all("pfxr")

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

p0a_dat = group_by(cfec, year, permits) %>%
  summarize(totEarn = sum(g_earn)/1e9, salmEarn = sum(g_earn[substr(p_fshy,1,1) == "S"])/1e9,
    nonsalmEarn = sum(g_earn[substr(p_fshy,1,1) != "S"])/1e9)
# panel 0 is something like total revenue across fleet
#p0a = group_by(cfec, year) %>%
#  summarize(totEarn = sum(g_earn)/1e9, salmEarn = sum(g_earn[substr(p_fshy,1,1) == "S"])/1e9,
#    nonsalmEarn = sum(g_earn[substr(p_fshy,1,1) != "S"])/1e9) %>%
#  ggplot(aes(year, totEarn, colour = "total"), color = "black") + geom_line() +
#  geom_line(aes(year, salmEarn, colour = "salmon")) +
#  geom_line(aes(year, nonsalmEarn, colour = "non-salmon")) + ylab("Total revenue (billions)") + ggtitle("Total revenue (inflation adjusted)")

p0b_dat = group_by(cfec, year, p_holder, permits) %>%
  summarize(earn = sum(g_earn), npermit = length(unique(p_fshy))) %>%
  group_by(year, permits) %>%
  summarize(median = median(earn), low = quantile(earn, 0.25),
    upper = quantile(earn, 0.75))

# panel 1 = something like proportion of single permit holders, broken out by salmon
# this calculates number of salmon and non-salmon permits held per person-year
permits_person_year = group_by(cfec, p_holder, year) %>%
  summarize(n.otherpermit = length(unique(p_fshy[g_earn>0 & substr(p_fshy,1,1) != "S"])),
    n.salpermit = length(unique(p_fshy[g_earn>0 & substr(p_fshy,1,1) == "S"])))
permits_person = group_by(permits_person_year, year) %>%
  summarize(n.other = length(which(n.otherpermit==1 & n.salpermit==0)),
    n.sal = length(which(n.salpermit==1 & n.otherpermit==0)), nTot = n())

p1_dat <- permits_person

# panel 2 = something like species diversity, maybe again splitting out salmon effect?
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

effSpec = group_by(cfec, year, p_holder, specn) %>%
  summarize(earn = sum(g_earn)) %>%
  group_by(year, p_holder) %>%
  summarize(effSpec = simp.div(earn), effSpec.nosal = simp.div(earn[specn%in%c(seq(400,450,10))==FALSE]))

# add salmon specialist label -- basically anyone with single salmon permit
salmon_specialists = filter(permits_person_year, n.salpermit ==1 & n.otherpermit==0) %>%
  select(-n.otherpermit, -n.salpermit)
effSpec$salmon = 0 # ugly, wasn't working with dplyr
effSpec$salmon[paste(effSpec$p_holder,effSpec$year)%in%paste(salmon_specialists$p_holder,salmon_specialists$year)] = 1

# showing uncertainty on this plot isn't very useful (variability across people)
p2_dat = group_by(effSpec, year) %>%
  summarize(mean = mean(effSpec,na.rm=T),
    mean.other = mean(effSpec.nosal[is.finite(effSpec.nosal)],na.rm=T),
    sd = sd(effSpec,na.rm=T),
    sd.other = sd(effSpec.nosal[is.finite(effSpec.nosal)],na.rm=T),
    lower = mean - sd/mean, upper = mean + sd/mean,
    lower.other = mean.other - sd.other/mean.other,
    upper.other = mean.other + sd.other/mean.other,
    med = median(effSpec, na.rm = T),
    lq = quantile(effSpec, probs = 0.25, na.rm = T),
    uq = quantile(effSpec, probs = 0.75, na.rm = T),
    med.other = median(effSpec.nosal[is.finite(effSpec.nosal)],na.rm=T),
    lq.other = quantile(effSpec.nosal[is.finite(effSpec.nosal)],probs=0.25,na.rm=T),
    uq.other = quantile(effSpec.nosal[is.finite(effSpec.nosal)],probs=0.75,na.rm=T)
    )

# --------------------------------------------
this_fig_theme <- theme_gg() +
  # theme(legend.justification = c(1, 1), legend.position = c(1, 1))
  theme(legend.position="none")

p0a <-  p0a_dat %>%
  ggplot(aes(year, totEarn, group = permits, colour = permits)) +
  geom_line() +
  ylab("Total revenue (billions)") +
  # ggtitle("Total revenue (inflation adjusted)") +
  this_fig_theme

p0b <- p0b_dat %>%
  ggplot(aes(year, median, group=permits, color = permits)) +
  geom_line() +
  ylab("Median revenue/permit") +
  xlab("Year") +
  scale_y_log10() +
  # ggtitle("Median revenue") +
  this_fig_theme

p1 <- p1_dat %>%
  ggplot(aes(x = year, y = (n.sal)/nTot, colour = "salmon")) +
  geom_line() +
  geom_line(aes(x = year, y = (n.sal+n.other)/nTot, colour = "total")) +
  xlab("Year") +
  ylab("Proportion of total active") +
  # ggtitle("Single permit fishers") +
  this_fig_theme

p2 <- p2_dat %>%
  ggplot(aes(x = year, y = mean)) + geom_line() +
  geom_line(aes(x = year, y = mean, colour = "all")) +
  geom_line(aes(x = year, y = mean.other, colour = "no salmon specialists")) +
  xlab("Year") +
  ylab("Effective species diversity") +
  # ggtitle("Species diversity") +
  this_fig_theme

# pdf("portfolio/figs/fig1.pdf", width = 6.5, height = 4.75)
# gridExtra::grid.arrange(p0a, p0b, p1, p2, ncol=2)
# grid_arrange_shared_legend(p0a, p0b, p1, p2, ncol = 2, nrow = 2, position = "right")
# grid::grid.draw(rbind(cbind(ggplotGrob(p1), ggplotGrob(p2), size="last"))
# dev.off()

# par(mfrow = c(2, 2))
pdf("portfolio/figs/fig1-map.pdf", width = 5, height = 5)
# layout(rbind(c(1, 2), c(1, 3), c(1, 4)))
# plot(1)
# layout.show(4)
par(mfrow = c(2, 2))
par(cex = 0.6)
par(mar = c(4, 4, 1, 1))
par(oma = c(1, 1, 1, 1))
library(maps)
library(mapdata)
x <- map("worldHires",".", xlim=c(-178,-130), ylim=c(51,73), col="gray80",
 fill=TRUE, border = 0, mar = c(1,1,1,1), boundary = F, plot = F)
 # box(col = "grey40")
plot(x$x, x$y, type = "n", xlim=c(-178,-130), ylim=c(51,73))
# polygon(x$x, x$y, border = "grey80", col = "grey80")

p <- readr::read_csv("data/map-points.csv")

for (i in 1:nrow(p)) {
  points(-1*p$long[i], p$lat[i], col = "red", pch = 19, cex = 1)
  graphics::text(-1*p$long[i], p$lat[i], labels = p$name[i], pos = 4)
}

par(cex = 0.6)
# par(mar = c(1, 1, 1, 1))
# par(oma = c(1, 1, 1, 1))
with(p2_dat, plot(year, med, type = "l", ylim = c(0.9, 1.6),
    col = "red", ylab = "Sp. diversity"))
text(2000, 1.6, "All", col = "red")
with(p2_dat, lines(year, med.other, col = "black"))
text(2000, 1.4, "No salmon specialists")

with(filter(p0a_dat, permits == "Multiple"), plot(year, totEarn, type = "l",
  ylim = range(p0a_dat$totEarn), ylab = "Total revenue (billions)"))
text(1990, 1.0, "Multiple permits", col = "black")
with(filter(p0a_dat, permits != "Multiple"), lines(year, totEarn, col = "blue"))
text(1990, 0.5, "Single permits", col = "blue")

with(p1_dat, plot(year, n.sal/nTot, type = "l", ylim = c(0.4, 0.9),
    col = "red", ylab = "Proportion single permit holders"))
text(2000, 0.6, "Salmon", col = "red")
with(p1_dat, lines(year, (n.sal+n.other)/nTot, col = "black"))
text(2000, 0.75, "All", col = "red")

dev.off()

##############
cfec <- mutate(cfec, sp_id = substr(p_fshy, 1, 1))
es = group_by(cfec, year, p_holder, specn) %>%
  summarize(earn = sum(g_earn), spp = paste(sp_id, collapse = ""))
es2 = group_by(es, year, p_holder) %>%
  summarize(effSpec = simp.div(earn), spp = paste(spp, collapse = ""))

usp <- sort(table(cfec$sp_id))
usp <- names(usp[usp > 75000])
usp <- c("x", usp)

es2$unique_spp <- unlist(lapply(lapply(
      strsplit(es2$spp, ""), unique),
    paste, collapse = ""))
grepl("^S$", es2$unique_spp[1:10])

out_div <- plyr::ldply(usp, function(x) {
  message(x)
  filter(es2, !grepl(x, unique_spp)) %>%
  # filter(es2, !grepl(paste0("^", x, "$"), unique_spp)) %>%
  group_by(year) %>%
  summarize(mean_spdiv = mean(effSpec, na.rm = T), jk = x)
})

########
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

#############
#' ggplot2-like colour scale in HCL space
#'
#' @param n Number of colours to return.
#' @param hue_min Minimum hue value in the range [0,360]
#' @param hue_max Maximum hue value in the range [0,360]
#' @param l Luminance in the range [0,100]
#' @param c Chroma of the colour.
#' @details See the \code{hcl} function for details.
#' @examples
#' gg_color_hue(10)
#'
gg_color_hue <- function(n, hue_min = 10, hue_max = 280, l = 62, c = 100) {
  hues = seq(hue_min, hue_max, length=n+1)
  hcl(h=hues, l=l, c=c)[1:n]
}

cols <- RColorBrewer::brewer.pal(4, "Set2")
cols <- gg_color_hue(3)[c(3, 1, 2)]
cols <- rep(cols[1], 99)

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
  geom_line(data = filter(out_permits, jk == "x"),
    aes(year, prop_single), colour = "black", lwd = 1.2) +
  geom_line(data = filter(out_permits, jk == "S"),
    aes(year, prop_single), colour = cols[2]) +
  geom_line(data = filter(out_permits, jk == "B"),
    aes(year, prop_single), colour = cols[2]) +
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
library(ggrepel)
coast_map <- fortify(map("worldHires", fill=TRUE, plot=FALSE))
coast_map <- filter(coast_map, region %in% c("USA", "Canada"))
gg <- ggplot()
gg <- gg + geom_map(data=coast_map, map=coast_map,
                    aes(x=long, y=lat, map_id=region),
                    fill="grey70", color="grey70")
gg <- gg + xlim(-181.5,-129) + ylim(50, 71)
gg <- gg + coord_map()
gg <- gg + coord_map()
gg <- gg + theme_gg()
gg <- gg + geom_point(data = p, aes(-long, lat), colour = cols[2]) +
  geom_text_repel(data = p, aes(-long, lat, label = name),
    colour = "grey15", segment.size = 0.3, segment.color = "grey50",
    size = 3.2) +
  annotate("text", x = -147, y = 66, label = "Alaska",
    colour = "grey15", size = 4.2) +
  xlab("Longitude") + ylab("Latitude")
# gg

pdf("portfolio/figs/fig1-jk.pdf", width = 7, height = 6)
cowplot::plot_grid(gg, pl3, pl2, pl1, labels = "AUTO",
  ncol = 2, hjust = -5, vjust = 2.3)
dev.off()
