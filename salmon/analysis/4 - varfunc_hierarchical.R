library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

for(model_name in c("individual","fishery","region")) {
#model_name = "individual" # fishery, individual
#weight_by_species = readRDS("portfolio/data-generated/weight_data_by_species_fishery.rds")
#weight_by_species = readRDS("salmon/data-generated/weight_data_by_species_people2.rds")
weight_by_species = readRDS("portfolio/data-generated/weight_data_by_species_people.rds")

names(weight_by_species)[which(names(weight_by_species)=="spec")]="species"
names(weight_by_species)[which(names(weight_by_species)=="p_fshy")]="fishery"
weight_by_species$species[which(weight_by_species$species=="COHO")] = "Coho"
weight_by_species$species[which(weight_by_species$species=="SOCK")] = "Sockeye"
weight_by_species$species[which(weight_by_species$species=="CHNK")] = "Chinook"
weight_by_species$species[which(weight_by_species$species=="PINK")] = "Pink"
weight_by_species$species[which(weight_by_species$species=="CHUM")] = "Chum"
weight_by_species$region = NA
weight_by_species$region[which(weight_by_species$fishery == "S 01A")] = "Southeast"
weight_by_species$region[which(weight_by_species$fishery == "S 03A")] = "Southeast"
weight_by_species$region[which(weight_by_species$fishery == "S 03E")] = "Prince William Sound"
weight_by_species$region[which(weight_by_species$fishery == "S 01E")] = "Prince William Sound"
weight_by_species$region[which(weight_by_species$fishery == "S 01K")] = "Kodiak"
weight_by_species$region[which(weight_by_species$fishery == "S 01M")] = "Alaska Peninsula"

#weight_by_species$w = weight_by_species$w / weight_by_species$n

if(model_name == "fishery") {
  weight_by_species$w = log(weight_by_species$w)
}
if(model_name == "individual") {
  weight_by_species$w = log(weight_by_species$w/weight_by_species$n)
}
if(model_name == "region") {
  weight_by_species = group_by(weight_by_species, species, year, region) %>%
    summarize(w = sum(w, na.rm=T))
  names(weight_by_species)[which(names(weight_by_species)=="region")]="fishery"
}

hatch = readRDS(file="salmon/data-generated/hatchery_harvest_millions.rds")
hatch$total_harvest[which(hatch$percent_hatchery==0)] = hatch$hatchery_harvest_millions[which(hatch$percent_hatchery==0)]
if(model_name == "region") {
  names(hatch)[which(names(hatch)=="region")]="fishery"
}
hatch = hatch[,c("year","species","region","percent_hatchery")]
hatch_ap = expand.grid("year"=unique(hatch$year), "species"=unique(hatch$species),
  "percent_hatchery"=0, "region" = "Alaska Peninsula")
hatch = rbind(hatch, hatch_ap)
hatch = left_join(weight_by_species, hatch)

hatch$fishery_species = paste0(hatch$fishery,"_",hatch$species)
hatch$prev_catch = hatch$w[match(paste0(hatch$species, "_", hatch$fishery,"_",hatch$year-1), paste0(hatch$species, "_", hatch$fishery,"_",hatch$year))]

hatch$prev_catch2 = hatch$w[match(paste0(hatch$species, "_", hatch$fishery,"_",hatch$year-2), paste0(hatch$species, "_", hatch$fishery,"_",hatch$year))]
hatch$percent_hatchery[which(is.na(hatch$percent_hatchery))] = 0
hatch$prev_pcthatch = hatch$percent_hatchery[match(paste0(hatch$species, "_", hatch$fishery,"_",hatch$year-1), paste0(hatch$species, "_", hatch$fishery,"_",hatch$year))]

hatch = hatch[-which(is.na(hatch$prev_catch)),]
hatch = hatch[-which(is.na(hatch$prev_catch2)),]
if(model_name!="region") {
  hatch = hatch[-which(hatch$fishery=="S 01K" & hatch$year%in%c(1989,1990)),]
}

fishery_species = as.numeric(as.factor(paste0(hatch$fishery, hatch$species)))
fishery = as.numeric(as.factor(hatch$fishery))
y = hatch$w
yprev = hatch$prev_catch
species = as.numeric(as.factor(hatch$species))
percent_hatchery = hatch$percent_hatchery/100
N = length(fishery)
M = max(fishery_species)

mod = stan(file="salmon/analysis/hatchery_var_hier.stan", data = list("N"=N,"M"=M, "y"=y,
    "yprev"=yprev, "species"=species, "percent_hatchery"=percent_hatchery,
    "fishery"=fishery, "fishery_species"=fishery_species,
  "yprev2"=hatch$prev_catch2, "prev_pct_hatchery"=hatch$prev_pcthatch),
    pars = c("sigma0","sigma0","sigma1","musigma1","sigsigma1","b1","mub1","sigb1","pred", "log_lik","effect_sig","phi"),
    iter=1000, chains=3, thin=1)

saveRDS(mod, file=paste0("salmon/data-generated/",model_name,"_agg.rds"))

#library(loo)
#loo(extract_log_lik(mod))$looic

df = data.frame("fishery_species"=levels(as.factor(paste0(hatch$fishery, hatch$species))))
df$fishery = substr(df$fishery_species, 1, 5)
df$species = substr(df$fishery_species, 6, length(df$fishery_species))
if(model_name=="region") {
  df$fishery = c(rep("Kodiak",5), rep("Prince William Sound",5), rep("Southeast",5))
  df$species = rep(c("Chinook","Chum","Coho","Pink","Sockeye"), 3)
}
df$estimate = apply(extract(mod, "effect_sig")$effect_sig, 2, mean)
df$sd = apply(extract(mod, "effect_sig")$effect_sig, 2, sd)
df$low90 = apply(extract(mod, "effect_sig")$effect_sig, 2, quantile, 0.05)
df$high90 = apply(extract(mod, "effect_sig")$effect_sig, 2, quantile, 0.95)
df$low95 = apply(extract(mod, "effect_sig")$effect_sig, 2, quantile, 0.025)
df$high95 = apply(extract(mod, "effect_sig")$effect_sig, 2, quantile, 0.975)

saveRDS(df, file=paste0("salmon/data-generated/",model_name,"_df.rds"))

}



# diagnostics, etc
df = data.frame("y"=y,"percent_hatchery"=percent_hatchery, "fishery"=levels(as.factor(hatch$fishery))[fishery],"species"=species,
"fishery_species"=levels(as.factor(paste0(hatch$fishery, hatch$species)))[fishery_species], "pred" = apply(extract(mod, "pred")$pred, 2, mean),
  "resid" = y - apply(extract(mod, "pred")$pred, 2, mean), "year"=hatch$year)

df = arrange(df,fishery_species,year)
ac = 0
for(i in 1:30) {
  indx = which(df$fishery_species==levels(df$fishery_species)[i])
  ac[i] = acf(df$resid[indx])$acf[,,1][2]
}

ggplot(df, aes(year, resid)) + geom_point() + geom_line() + facet_wrap(~fishery_species)

ggplot(df, aes(y, pred)) + geom_point() + geom_smooth() + xlab("Obs") + ylab("Pred")

ggplot(df, aes(y, pred)) + geom_point() + geom_smooth() + xlab("Obs") + ylab("Pred") + facet_wrap(~fishery)

ggplot(df, aes(y, pred)) + geom_point() + geom_smooth() + xlab("Obs") + ylab("Pred") + facet_wrap(~fishery)

ggplot(df, aes(y, pred)) + geom_point() + geom_smooth() + xlab("Obs") + ylab("Pred") + facet_wrap(~species)

ggplot(df, aes(percent_hatchery, resid)) + geom_point() + geom_smooth() + xlab("Obs") + ylab("Pred") + facet_wrap(~species)
