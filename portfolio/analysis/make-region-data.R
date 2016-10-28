cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
# convert 6-dig p_fshy codes to 5 digits
# removing blank space - now they're all length(5)
idx = which(nchar(cfec$p_fshy)==6)
cfec$p_fshy[idx] = paste(substr(cfec$p_fshy[idx], 1, 1),
  substr(cfec$p_fshy[idx], 3, 6), sep="")
# remove white space in 2nd position
idx = which(substr(cfec$p_fshy, 2, 2) == " ")
cfec$p_fshy[idx] = paste(substr(cfec$p_fshy[idx], 1, 1),
  substr(cfec$p_fshy[idx], 3, length(cfec$p_fshy[idx])), sep="")

cfec$p_spec = substr(cfec$p_fshy,1,1)
cfec$p_spec[cfec$p_spec == "B"] = "Halibut"
cfec$p_spec[cfec$p_spec == "C"] = "Sablefish"
cfec$p_spec[cfec$p_spec %in% c("D","E","K","T")] = "Crab"
cfec$p_spec[cfec$p_spec %in% c("F")] = "Freshwater"
cfec$p_spec[cfec$p_spec %in% c("G","H","L")] = "Herring"
cfec$p_spec[cfec$p_spec %in% c("I")] = "Lingcod"
cfec$p_spec[cfec$p_spec %in% c("A","J","N","R","W")] = "Shellfish"
cfec$p_spec[cfec$p_spec %in% c("O","P","Q","U","Z")] = "Invert"
cfec$p_spec[cfec$p_spec %in% c("M")] = "Misc"
cfec$p_spec[cfec$p_spec %in% c("S")] = "Salmon"
cfec$p_spec[cfec$p_spec %in% c("Y")] = "Rockfish"
cfec <- filter(cfec, p_spec != "Freshwater") # only 1400 rows

library(tidyverse)

# look_up <- read_csv("data/cfec-codes.csv")
# look_up <- look_up %>% mutate(p_fshy = gsub(" ", "", p_fshy))

d <- select(cfec, p_holder, p_fshy, g_earn, region, p_spec, specn, year, g_pounds)
rm(cfec)

d$taxa = NA
d$taxa[d$p_spec %in%c("Halibut", "Sablefish", "Lingcod", "Rockfish", "Misc")] = "Groundfish"
d$taxa[d$p_spec %in%c("Crab", "Shellfish", "Invert")] = "Invertebrate"
d$taxa[d$p_spec %in%c("Salmon")] = "Salmon"
d$taxa[d$p_spec %in%c("Herring")] = "Herring"

d$region_combined <- NA
d$region_combined[d$region %in% c("Cook Inlet", "PWS")] <- "PWS/Cook Inlet"
d$region_combined[d$region %in% c("Kodiak")] <- "Kodiak"
d$region_combined[d$region %in% c("Alaska Peninsula")] <- "Alaska Peninsula"
d$region_combined[d$region %in% c("SEAK Inside", "SEAK Outside", "Yakutat")] <- "Southeast"
d$region_combined[d$region %in% c("Bristol Bay")] <- "Bristol Bay"

revenue <- d %>% group_by(year, taxa, region_combined) %>%
  summarize(revenue = sum(g_earn)) %>%
  ungroup() %>%
  mutate(revenue = revenue/1e6)

simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

no_salmon <- filter(d, taxa != "Salmon") %>% mutate(jackknife = "Salmon")
no_invertebrates <- filter(d, taxa != "Invertebrate") %>% mutate(jackknife = "Invertebrate")
no_herring <- filter(d, taxa != "Herring") %>% mutate(jackknife = "Herring")
no_groundfish <- filter(d, taxa != "Groundfish") %>% mutate(jackknife = "Groundfish")
all <- mutate(d, jackknife = "None")

diversity <- bind_rows(all, no_salmon) %>%
  bind_rows(no_invertebrates) %>%
  bind_rows(no_herring)  %>%
  bind_rows(no_groundfish) %>%
  group_by(jackknife, p_holder, region_combined, year, specn) %>%
  summarize(revenue = sum(g_earn))%>%
  ungroup() %>% group_by(jackknife, p_holder, year, region_combined) %>%
  summarize(species_diversity = simp.div(revenue)) %>%
  ungroup() %>% group_by(jackknife, year, region_combined) %>%
  summarize(mean_species_diversity = mean(species_diversity)) %>%
  ungroup()

readr::write_csv(revenue, "portfolio/data-generated/commercial-region-revenue.csv")
readr::write_csv(diversity, "portfolio/data-generated/commercial-region-diversity.csv")
