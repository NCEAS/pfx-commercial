rm(list = ls())
load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/fig1-dat.rda")

library(dplyr)

nums <- c("one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5, "six" = 6,
  "seven" = 7, "eight" = 8, "nine" = 9, "ten" = 10, "11" = 11, "12" = 12, "13" = 13,
  "14" = 14, "15" = 15, "16" = 16)

nrows <- nrow(mm)
npholders <- length(unique(dat$p_holder))
range_yrs <- range(dat$year)
nstr <- length(unique(dat$strategy))
rvers <- paste(R.version$major, R.version$minor, sep = ".")
nstr_yrs <- length(unique(dat$strategy_year))

prop_single_all <- filter(out_permits, jk == "x")$prop_single
min_prop_single <- min(prop_single_all)
yr_min_prop_single <- filter(out_permits, jk == "x")$year[prop_single_all == min_prop_single]
min_prop_single <- min_prop_single %>% round(2) * 100
max_prop_single <- filter(out_permits, jk == "x")$prop_single %>%
  max() %>% round(2) * 100
curr_prop_single_nohal <- filter(out_permits,
  jk == "B")$prop_single[length(unique(out_permits$year))] %>% round(2) * 100
curr_prop_single_nosalm <- filter(out_permits,
  jk == "S")$prop_single[length(unique(out_permits$year))] %>% round(2) * 100

spdiv_all <- filter(out_div, jk == "x")$mean_spdiv
max_spdiv_all <- max(spdiv_all)
yr_max_spdiv_all <- filter(out_div, jk == "x")$year[spdiv_all == max_spdiv_all]
max_spdiv_all <- max_spdiv_all %>% round(2) * 100

min_spdiv_all <- min(spdiv_all)
yr_min_spdiv_all <- filter(out_div, jk == "x")$year[spdiv_all == min_spdiv_all]
min_spdiv_all <- min_spdiv_all %>% round(2) * 100

spdiv_salm <- filter(out_div, jk == "S")$mean_spdiv
max_spdiv_salm <- max(spdiv_salm)
yr_max_spdiv_salm <- filter(out_div, jk == "S")$year[spdiv_salm == max_spdiv_salm]
max_spdiv_salm <- max_spdiv_salm %>% round(2) * 100

n_jacknifed <- length(unique(out_div$jk)) - 1
n_jacknifed <- names(nums[n_jacknifed])

mean_permit_days <- round(mean(md$strategy_mean_days)/10) * 10

e1 <- -1 * rstan::extract(m)$coef_g1_strategy
e2 <- rstan::extract(m)$coef_g2_strategy

e <- rbind(e1, e2)
prob_change_inc_variance <- round(sum(e > 0) / (nrow(e)*ncol(e)), 2) * 100

ndat <- readRDS("portfolio/data-generated/ndat.rds")
ndat_cull <- readRDS("portfolio/data-generated/ndat-cull.rds")
ndat <- c(ndat, ndat_cull)
nstrat <- readRDS("portfolio/data-generated/nstrat-cull.rds")
rm(mm, mm2, dat)
save.image(file = "portfolio/data-generated/output-values.rda")
