library(dplyr)
library(ggplot2)
library(reshape2)

cfec = feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

# Seak Inside / Outside

#

# sum up halibut landings by region and permit
pdf("small_vs_large_halibut.pdf")
group_by(cfec[cfec$region%in%c("PWS","Kodiak","Cook Inlet","SEAK Outside"), ], year, region) %>%
  summarize(p1 = sum(g_earn[p_fshy=="B 06B"]), p2 = sum(g_earn[p_fshy=="B 61B"])) %>%
  ggplot(aes(year, p1/(p1+p2))) + geom_line() + facet_wrap(~region) + xlab("Year") +
  ylab("B06B/(B06B+B61B)")
dev.off()
