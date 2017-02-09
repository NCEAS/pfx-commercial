library(dplyr)
d <- readr::read_csv("data/strategies-labels.csv")

load("portfolio/data-generated/diff-dat-stan.rda")
d <- d %>% mutate(modeled = ifelse(strategy %in% md$strategy, "Yes", "No"))

# spp <- group_by(dat, strategy) %>%
  # summarise(
    # spp_l = round(quantile(specDiv, prob = 0.25), 1),
    # spp_h = round(quantile(specDiv, prob = 0.75), 1))

# d <- inner_join(d, spp)
d <- inner_join(d, select(md, strategy, strategy_mean_div))

#######
dat <- readRDS(file="portfolio/data-generated/cfec-annual-for-modeling.rds")
dat$permit_group = NA
dat$permit_group[dat$permit%in%c("S05B","S15B")] = "S - troll"
dat$permit_group[which(dat$permit%in%c("S03H","S03M","S03T"))] = "S - drift - CI, AP, BB (sock)"
dat$permit_group[which(dat$permit%in%c("S01H","S01K"))] = "S - seine - CI, K"
dat$permit_group[which(dat$permit%in%c("S04E","S04H","S04K","S04M","S04T"))] = "S - set - PWS, CI, K, AP, BB (sock)"
dat$permit_group[which(dat$permit%in%c("S04W","S04Z"))] = "S - set - NS/KU"
dat$permit_group[which(dat$permit%in%c("S01L"))] = "S - seine - CH"
dat$permit_group[which(dat$permit%in%c("S01A"))] = "S - seine - SE"
dat$permit_group[which(dat$permit%in%c("S01E"))] = "S - seine - PWS"
dat$permit_group[which(dat$permit%in%c("S03E"))] = "S - drift - PWS"
dat$permit_group[which(dat$permit%in%c("S01M"))] = "S - seine - AP"
dat$permit_group[which(dat$permit%in%c("S03A"))] = "S - drift - SE"
dat$permit_group[which(dat$permit%in%c("S04D"))] = "S - set - YAK"

salmon_permit_table = group_by(dat[which(!is.na(dat$permit_group)),], permit_group) %>%
  summarize(tot = sum(chnk) + sum(coho) + sum(pink) + sum(sock) + sum(chum),
    chnk = round(sum(chnk)/tot,2), coho = round(sum(coho)/tot,2), pink = round(sum(pink)/tot,2),
    sock = round(sum(sock)/tot,2), chum = round(sum(chum)/tot,2)) %>%
  select(-tot)

concatenate_species = function(x) {
  tmp = x[c("chnk","coho","pink","sock","chum")] > 0.1
  paste(names(tmp)[tmp], collapse=", ")
}

salmon_permit_table$species_groups = apply(salmon_permit_table, 1, concatenate_species)

salmon_permit_table = select(salmon_permit_table, permit_group, species_groups)

cfec =feather::read_feather("portfolio/data-generated/cfec.feather")

# otter trawl = cod, pollock
g1 = group_by(cfec[grep("M 7",cfec$p_fshy),], spec) %>%
  summarize(s = sum(g_earn)) %>%
  mutate(tot = s/sum(s)) %>% filter(tot > 0.1)

# otter trawl - M09 and M 9X are pcod
g2 = group_by(cfec[grep("M 09",cfec$p_fshy),], spec) %>%
  summarize(s = sum(g_earn)) %>%
  mutate(tot = s/sum(s)) %>% filter(tot > 0.1)

# M 06 and M 6X are all pcod
g3 = group_by(cfec[grep("M 6",cfec$p_fshy),], spec) %>%
  summarize(s = sum(g_earn)) %>%
  mutate(tot = s/sum(s)) %>% arrange(-tot) %>% filter(tot > 0.1)

permit_table = rbind(as.data.frame(salmon_permit_table),
  tibble::tribble(
    ~permit_group, ~species_groups,
    "Fin - otter", "cod",
    "Fin - ll", "pollock",
    "Fin - pot", "cod",
    "Dun C", "crab",
    "King C", "crab",
    "Herr - pound", "herring",
    "Sea cuc", "sea cucumber",
    "Tan C", "crab",
    "Hal - ll", "halibut",
    "Herr roe - gill", "herring",
    "Sab", "sablefish")
  )


# alternatively do it by permit
#salmon_permit_table2 = group_by(dat[which(dat$permit %in% c("S05B","S15B","S03H",
#  "S03M","S03T","S01H","S01K","S04E","S04H","S04K","S04M","S04T","S04W","S04Z")),], permit, year) %>%
#  summarize(tot = sum(chnk,na.rm=T) + sum(coho,na.rm=T) + sum(pink,na.rm=T) + sum(sock,na.rm=T) + sum(chum,na.rm=T),
#    chnk = sum(chnk)/tot, coho = sum(coho)/tot, pink = sum(pink)/tot,
#    sock = sum(sock)/tot, chum = sum(chum)/tot,
#    permit_group = permit_group[1]) %>%
#  select(-tot) %>%
#  group_by(permit) %>%
#  summarize(chnk = round(mean(chnk,na.rm=T),2), pink = round(mean(pink,na.rm=T),2), coho = round(mean(coho,na.rm=T),2),
#    chum = round(mean(chum,na.rm=T),2), sock = round(mean(sock,na.rm=T),2), permit_group = permit_group[1])
#######

permit_table <- permit_table %>% dplyr::rename(`Label` = permit_group,
  `Main species` = species_groups)

d <- select(d, str_label, description, strategy_mean_div, modeled) %>%
  rename(Label = str_label, Description = description,
    `Over 100 permit holders` = modeled,
    `Mean spp. diversity` = strategy_mean_div) %>%
  arrange(`Mean spp. diversity`) %>%
  mutate(`Mean spp. diversity` = round(`Mean spp. diversity`, 1))

d <- d %>% filter(`Over 100 permit holders` == "Yes") %>%
  select(-`Over 100 permit holders`)

d <- d %>% left_join(permit_table)

xtable::print.xtable(
  xtable::xtable(d,
    caption = "", digits = 1),
  include.rownames = FALSE,
  file = "portfolio/figs/strategy-table.tex",
  booktabs = TRUE,
  caption.placement = "top",
  size = "small",
  sanitize.text.function = identity,
  only.contents = TRUE,
  timestamp = NULL
)

n_grouped_permits <- nrow(d)
saveRDS(n_grouped_permits, "portfolio/data-generated/n_grouped_permits.rds")
