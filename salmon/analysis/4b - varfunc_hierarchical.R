library(reshape2)
library(dplyr)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dat = read.csv("salmon/data-generated/commonPropertyCommercialCatch.csv")
dat = dplyr::rename(dat, year=Year, region=Region)

harvest <- melt(dat[,c("year","region","Chinook_harvest","Sockeye_harvest",
  "Coho_harvest","Pink_harvest","Chum_harvest")], id.vars = c("year","region"))
harvest$variable = unlist(lapply(strsplit(as.character(harvest$variable),"_"), getElement, 1))
harvest = dplyr::rename(harvest, species=variable, w = value)

enhance <- melt(dat[,c("year","region","Chinook_enhanced","Sockeye_enhanced",
  "Coho_enhanced","Pink_enhanced","Chum_enhanced")], id.vars = c("year","region"))
enhance$variable = unlist(lapply(strsplit(as.character(enhance$variable),"_"), getElement, 1))
enhance = dplyr::rename(enhance, species=variable, enhanced = value)

dat = dplyr::left_join(harvest, enhance)
# combine ap and kodiak - they were combined in reports up to 1995
kodap_1 = dat[which(dat$region=="Kodiak" & dat$year <= 1995),]
kodap_1$region = as.character(kodap_1$region)
kodap_1$region = "KodAP"
kodap_2 = group_by(dat[which(dat$year > 1995 & dat$region %in% c("Kodiak","AP")),], species, year) %>%
  summarize("region"="KodAP", w = sum(w), enhanced = sum(enhanced)) %>%
  as.data.frame
kodap = rbind(kodap_1[,c("year","region","species","w","enhanced")],
  kodap_2[,c("year","region","species","w","enhanced")])

dat = dat[-which(dat$region%in% c("Kodiak","AP")),]
dat = rbind(dat, kodap)
dat$region = as.character(dat$region)
dat$species = as.character(dat$species)

dat$percent_hatchery = dat$enhanced/dat$w
dat = select(dat, -enhanced)

dat$w = log(dat$w)

#hatch$fishery_species = paste0(hatch$fishery,"_",hatch$species)
dat$prev_catch = dat$w[match(paste0(dat$species, "_", dat$region,"_",dat$year-1), paste0(dat$species, "_", dat$region,"_",dat$year))]

dat$prev_catch2 = dat$w[match(paste0(dat$species, "_", dat$fishery,"_",dat$year-2), paste0(dat$species, "_", dat$fishery,"_",dat$year))]
dat$percent_hatchery[which(is.na(dat$percent_hatchery))] = 0
dat$prev_pcthatch = dat$percent_hatchery[match(paste0(dat$species, "_", dat$fishery,"_",dat$year-1), paste0(dat$species, "_", dat$fishery,"_",dat$year))]

dat$diff_hatch = dat$percent_hatchery - dat$prev_pcthatch

dat = dat[-which(is.na(dat$prev_catch)),]
#dat = dat[-which(is.na(dat$diff_hatch)),]

fishery_species = as.numeric(as.factor(paste0(dat$region, dat$species)))
fishery = as.numeric(as.factor(dat$region))
y = dat$w
yprev = dat$prev_catch
species = as.numeric(as.factor(dat$species))
percent_hatchery = dat$diff_hatch#dat$percent_hatchery/100
N = length(fishery)
M = max(fishery_species)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mod = stan(file="salmon/analysis/hatchery_var_hier.stan", data = list("N"=N,"M"=M, "y"=y,
    "yprev"=yprev, "species"=species, "x"=percent_hatchery, "x2" = dat$percent_hatchery,
    "fishery"=fishery, "fishery_species"=fishery_species),
    pars = c("sigma0","sigma1","musigma1","sigsigma1","b1","mub1","sigb1","pred", "log_lik","effect_sig"),
    iter=5000, chains=5, thin=1)

saveRDS(mod, file=paste0("salmon/data-generated/",model_name,"_agg.rds"))

#library(loo)
#loo(extract_log_lik(mod))$looic

df = data.frame("fishery_species"=levels(as.factor(paste0(hatch$region, hatch$species))))
df$fishery = c(rep("Kodiak",5), rep("Prince William Sound",5), rep("Southeast",5))
df$species = rep(c("Chinook","Chum","Coho","Pink","Sockeye"), 3)

df$estimate = apply(rstan::extract(mod, "effect_sig")$effect_sig, 2, mean)
df$sd = apply(rstan::extract(mod, "effect_sig")$effect_sig, 2, sd)
df$low90 = apply(rstan::extract(mod, "effect_sig")$effect_sig, 2, quantile, 0.05)
df$high90 = apply(rstan::extract(mod, "effect_sig")$effect_sig, 2, quantile, 0.95)
df$low95 = apply(rstan::extract(mod, "effect_sig")$effect_sig, 2, quantile, 0.025)
df$high95 = apply(rstan::extract(mod, "effect_sig")$effect_sig, 2, quantile, 0.975)

df$bestimate = apply(rstan::extract(mod, "b1")$b1*0.2, 2, mean)
df$bsd = apply(rstan::extract(mod, "b1")$b1*0.2, 2, sd)

saveRDS(df, file=paste0("salmon/data-generated/",model_name,"_df.rds"))




# diagnostics, etc
hatch=dat
df = data.frame("y"=y,"percent_hatchery"=percent_hatchery, "fishery"=levels(as.factor(hatch$fishery))[fishery],"species"=species,
"fishery_species"=levels(as.factor(paste0(hatch$fishery, hatch$species)))[fishery_species], "pred" = apply(rstan::extract(mod, "pred")$pred, 2, mean),
  "resid" = y - apply(rstan::extract(mod, "pred")$pred, 2, mean), "year"=hatch$year)

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
