
# This figure is meant to illustrate increased specialization

# number of permits / person
library(dplyr)
library(knitr)
library(ggplot2)
library(date)

# cfec.feather already inflation adjusted
cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")

cfec$year <- as.numeric(cfec$year)

# add indicator for single permit landings
cfec = group_by(cfec, year, p_holder) %>%
  mutate(n.permit = length(unique(p_fshy))) %>%
  mutate(permits = ifelse(n.permit==1, "Single","Multiple")) %>%
  select(-n.permit)

p0a = group_by(cfec, year, permits) %>%
  summarize(totEarn = sum(g_earn)/1e9, salmEarn = sum(g_earn[substr(p_fshy,1,1) == "S"])/1e9,
    nonsalmEarn = sum(g_earn[substr(p_fshy,1,1) != "S"])/1e9) %>%
  ggplot(aes(year, totEarn, group = permits, colour = permits)) + geom_line() +
  ylab("Total revenue (billions)") + ggtitle("Total revenue (inflation adjusted)")

# panel 0 is something like total revenue across fleet
#p0a = group_by(cfec, year) %>%
#  summarize(totEarn = sum(g_earn)/1e9, salmEarn = sum(g_earn[substr(p_fshy,1,1) == "S"])/1e9,
#    nonsalmEarn = sum(g_earn[substr(p_fshy,1,1) != "S"])/1e9) %>%
#  ggplot(aes(year, totEarn, colour = "total"), color = "black") + geom_line() +
#  geom_line(aes(year, salmEarn, colour = "salmon")) +
#  geom_line(aes(year, nonsalmEarn, colour = "non-salmon")) + ylab("Total revenue (billions)") + ggtitle("Total revenue (inflation adjusted)")

p0b = group_by(cfec, year, p_holder, permits) %>%
  summarize(earn = sum(g_earn), npermit = length(unique(p_fshy))) %>%
  group_by(year, permits) %>%
  summarize(median = median(earn), low = quantile(earn, 0.25),
    upper = quantile(earn, 0.75)) %>%
  ggplot(aes(year, log(median), group=permits, color = permits)) + geom_line() + ylab("Ln median revenue (across people)") + xlab("Year") + ggtitle("Median revenue")

# panel 1 = something like proportion of single permit holders, broken out by salmon
# this calculates number of salmon and non-salmon permits held per person-year
permits_person_year = group_by(cfec, p_holder, year) %>%
  summarize(n.otherpermit = length(unique(p_fshy[g_earn>0 & substr(p_fshy,1,1) != "S"])),
    n.salpermit = length(unique(p_fshy[g_earn>0 & substr(p_fshy,1,1) == "S"])))
permits_person = group_by(permits_person_year, year) %>%
  summarize(n.other = length(which(n.otherpermit==1 & n.salpermit==0)),
    n.sal = length(which(n.salpermit==1 & n.otherpermit==0)), nTot = n())

p1 = ggplot(permits_person, aes(x = year, y = (n.sal)/nTot, colour = "salmon")) +
  geom_line() +
  geom_line(aes(x = year, y = (n.sal+n.other)/nTot, colour = "total")) +
    xlab("Year") + ylab("Proportion of total active") + ggtitle("Single permit fishers")

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
p2 = group_by(effSpec, year) %>%
  summarize(mean = mean(effSpec,na.rm=T),
    mean.other = mean(effSpec.nosal[is.finite(effSpec.nosal)],na.rm=T),
    sd = sd(effSpec,na.rm=T),
    sd.other = sd(effSpec.nosal[is.finite(effSpec.nosal)],na.rm=T),
    lower = mean - sd/mean, upper = mean + sd/mean,
    lower.other = mean.other - sd.other/mean.other,
    upper.other = mean.other + sd.other/mean.other) %>%
  ggplot(aes(x = year, y = mean)) + geom_line() + geom_line(aes(x = year, y = mean, colour = "all")) +
  geom_line(aes(x = year, y = mean.other, colour = "no salmon specialists")) + xlab("Year") +
  ylab("Effective species diversity ($ weighted)") + ggtitle("Species diversity")

pdf("portfolio/figs/Fig1b.pdf")
gridExtra::grid.arrange(p0a, p0b, p1, p2, ncol=2)
dev.off()
