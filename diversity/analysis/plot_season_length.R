library(dplyr)
library(knitr)
library(ggplot2)
library(date)

# crab yield versus high value habitat for whales. TRadeoff between crab catch + whale.
# Observed

cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

# filter out top ~ 50 permits, by earnings?
top_permits = group_by(cfec, p_fshy) %>%
  summarize(g = sum(g_earn)) %>%
  arrange(-g) %>%
  filter(g > g[50]) %>%
  select(-g)
cfec_new = left_join(top_permits, cfec)

# calculate daily diversity by permit/year
# diversity function
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

season_diversity = group_by(cfec_new, year, p_fshy, day) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(year, p_fshy) %>%
  summarize(div = simp.div(g))
pdf("season_diversity.pdf")
ggplot(season_diversity, aes(year, div)) + geom_line() + facet_wrap(~ p_fshy, scale="free_y") +
  ylab("Calendar day diversity") + xlab("Year") + ggtitle("Effective days fished (weighted by g_earn)")
dev.off()
