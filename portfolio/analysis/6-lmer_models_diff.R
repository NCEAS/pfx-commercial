# Experiment with models
# by fitting them quickly with lme4

# 2 separate but more complicated models. More interactions:
# - lmer() model to full dataset: interactions allowed to vary?
# - (1) model for mean, (2) model for variance (log sd)
# random effects in strategy?
# days as a covariate?
# 1. What's benefit for individual person
# 2. Benefit across people within a strategy [does this change with div]
# 3. benefit across strategies? [does this change with div]

rm(list = ls())
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)

dat = readRDS(file="portfolio/data-generated/cfec-diff-for-modeling.rds")

### Eric's culling:
#1. We restricted our analysis to p_holders who fished for 5 or more years
#dat <- group_by(dat, p_holder) %>%
#  mutate(nyr = length(unique(year))) %>%
#  mutate(range_div = diff(range(specDiv))) %>%
#  as_data_frame() %>%
#  filter(nyr >= 5)

# Filters: remove people-year combinations making < $5000
dat = dat[which(dat$revenue >= 5000), ]

# note: grouping here is based on strategies defined by permits
dat$strategy = dat$strategy_permit
dat <- group_by(dat, strategy) %>%
  mutate(npeople = length(unique(p_holder)))

#2. For each person-year combination, we created 'strategies' by concatenating
# all permits fished, and only retaining 'strategies' with >= 200 data points. Previously,
# this was done by people-year, but Eric changed this to be including > 200 people / strategy
top_strategies = group_by(dat, strategy) %>%
  summarize(n = length(unique(p_holder)),
    earn=sum(revenue)) %>%
  filter(n>=100) # note -- the code should work fine with this >= 100 too

#3. We then tabulated the permits that make up these strategies, and there are only
# 56.
top_permits = data.frame("orig"=names(table(unlist(lapply(top_strategies$strategy, strsplit, " ")))))

# 4. of these top permits, we proceeded to group permits that were targeting a single species
# and other than area, were otherwise the same. In other words, someone fishing herring roe
# in kodiak (G34K) and alaska peninsula (G34M) have a herring roe strategy. This combining
# reduces permits to 59
top_permits$new = as.character(top_permits$orig)
top_permits$new[which(substr(top_permits$new,1,3)=="D09")] = "D09" # dungeness, not limited by pot#
top_permits$new[which(substr(top_permits$new,1,3)=="G01")] = "G01" # herring roe, purse seine
top_permits$new[which(substr(top_permits$new,1,3)=="G34")] = "G34" # herring roe, gillnet
top_permits$new[which(substr(top_permits$new,1,3)=="K91")] = "K91" # king crab
top_permits$new[which(substr(top_permits$new,1,3)=="L12")] = "L12" # herring, hand pick
top_permits$new[which(substr(top_permits$new,1,3)=="L21")] = "L21" # herring, pound
top_permits$new[which(substr(top_permits$new,1,3)=="P09")] = "P09" # shrimp pot gear
top_permits$new[which(substr(top_permits$new,1,3)=="Q11")] = "Q11" # sea cucumber
top_permits$new[which(substr(top_permits$new,1,3)=="T09")] = "T09" # tanner crab

# 5. Combining multispecies finfish permits (M). There are 8 miscellaneous finfish permits (M)
# remaining, and only 2 can be combined: M26B/G (mechanical jig, GOA/statewide)
top_permits$new[which(substr(top_permits$new,1,3)=="M26")] = "M26" # mechanical jig

# 6. Combine some of the longline categories based on vessel size.
top_permits$new[top_permits$new=="C06B"] = "C61B"
top_permits$new[top_permits$new=="B06B"] = "B61B"
top_permits$new[top_permits$new=="S05B"] = "S15B"

# 7. Combining salmon permits is a little more difficult, because the species composition
# varies widely based on geography. For the drift gillnet (S03) permits, we
# combined S03 permits from Cook Inlet, Bristol Bay, and Alaska Peninsula because sockeye
# represents > 92% of species revenue
top_permits$new[which(top_permits$new%in%c("S03H","S03M","S03T"))] = "S03" # drift gillnet
# For the purse seine permits (S01), again there's wide variety in species landings by
# area (85% pink in PWS to 85% sockeye on alaska peninsula). Of the 5 permits, only 2 had
# similar species compositions, so we grouped Kodiak and Cook Inlet
top_permits$new[which(top_permits$new%in%c("S01H","S01K"))] = "S01" # purse seine
# For the set gillnet, S04 permits, there's some variety but also clear sockeye specialists. We
# grouped permits that had > 80% sockeye
top_permits$new[which(top_permits$new%in%c("S04E","S04H","S04K","S04M","S04T"))] = "S04a" # purse seine
# Finally, we grouped permits from norton sound and kuskokwim because of similar species %
top_permits$new[which(top_permits$new%in%c("S04W","S04Z"))] = "S04b"
# There's a few remaining permits (S04D, S04X, S04P, S04Y) - but they're so different they can't
# be grouped
#8. combine the new permit groupings into new strategies
top_permits$orig = as.character(top_permits$orig)
top_strategies$new.strategy = NA
for(i in 1:nrow(top_strategies)) {
  top_strategies$new.strategy[i] = paste(top_permits$new[match(lapply(lapply(top_strategies$strategy, strsplit, " "), unlist)[[i]],
  top_permits$orig)], collapse=" ")
}
# join this into the data frame -- this is leaving ~ 70 strategies
dat = left_join(dat, top_strategies) %>% select(-c(n, earn))
dat$strategy=dat$new.strategy
dat = dat[is.na(dat$strategy)==FALSE,]

# Derived variables
dat$log_spec_div <- scale(log(dat$specDiv))
dat$scaled_spec_div <- scale(dat$specDiv)
dat$log_length <- scale(log(dat$length + 1))
dat$log_weight <- scale(log(dat$weight + 1))
dat$log_days <- scale(log(dat$days + 1))
dat$log_npermit <- scale(log(dat$npermit))
dat$log_days_permit <- scale(log(dat$days_permit+1))

group_by(dat, strategy) %>%
  summarize(meanD = mean(log(days_permit + 1)), meanDiv = mean(specDiv), meanEarn = mean(revenue)) %>%
  as.data.frame %>%
  ggplot(aes(meanD, meanDiv, label = strategy)) +
  geom_text(size=3, aes(colour=meanEarn)) + xlab("Mean log_days") + ylab("Mean effective diversity")

# Pull in species compositions by strategy to help w/interpretation
specComp = read.csv("portfolio/data-generated/species_by_strategy.csv")
specComp = specComp[specComp$strategy.permit %in% top_strategies$strategy,-1]
# missing a few intentionally -- urchin/gduck/etc
specComp$new_strategy = top_strategies$new.strategy[match(specComp$strategy.permit , top_strategies$strategy)]
specComp[is.na(specComp)]=0

###############

#dat$strategy <- NULL
#dat$strategy <- dat$strategy_gear
#nrow(dat)
#dat <- group_by(dat, p_holder) %>%
#  mutate(nyr = length(unique(year))) %>%
#  mutate(range_div = diff(range(specDiv))) %>%
#  as_data_frame() %>%
#  filter(nyr >= 10) #%>%
#  # filter(range_div > 0)
#nrow(dat)

#dat <- group_by(dat, strategy) %>%
#  mutate(range_div = diff(range(specDiv))) %>%
#  as_data_frame() %>%
#  filter(range_div > 0) # eliminates "pound"
#nrow(dat)


# Downsample for speed of testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.5)
set.seed(1)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

# many different strategies, need to model only most common,
#top.strategies = names(rev(sort(table(dat$strategy)))[1:30])
#dat = dat[dat$strategy%in%top.strategies, ]
#nrow(dat)

library(glmmTMB)

# try to fit a model with people-strategy random effects, in addition to strategy ones.
# we can also compare how the calculation of days affects fit. calculating effort as
# the sum of individual permit seasons improves fit by ~ 2000 (2nd model here)
dat$logdiff = log(dat$revenue/dat$revenue.prev)
dat$days.change = log((dat$days_permit+1)/(dat$days_permit.prev + 1))
mod <- glmmTMB(logdiff ~ specDiv.change * days.change +
    (-1+specDiv.change + days.change + log(revenue.prev)|strategy) + log(revenue.prev), data = dat) # 426526.1

# diagnostics of mean model
strategy.summary = group_by(dat, strategy) %>%
  summarize(sdlogrev = sd(log(revenue)),
    meanrev = mean(log(revenue)),
    specDiv = mean(log(specDiv)))
strategy.summary$randomInt = ranef(mod)[[1]]$strategy$`(Intercept)` + fixef(mod)[[1]][["(Intercept)"]]
strategy.summary$randomSpec = ranef(mod)[[1]]$strategy$`specDiv.change` + fixef(mod)[[1]][["specDiv.change"]]
strategy.summary$randomDays = ranef(mod)[[1]]$strategy$`days.change` + fixef(mod)[[1]][["days.change"]]
strategy.summary$randomRevenue = ranef(mod)[[1]]$strategy$`log(revenue.prev)` + fixef(mod)[[1]][["log(revenue.prev)"]]

p <- tidyr::gather(strategy.summary, model, intercept, contains("random")) %>%
  ggplot(aes(specDiv, intercept, color = meanrev)) + geom_point() +
  facet_wrap(~model, scales = "free_y") +
  geom_smooth(se = FALSE, color = "red", method = "lm")
print(p)

# plot residuals v fitted
pdf("portfolio/figs/residuals_diff_model.pdf")
dat$fitted_mod = fitted.values(mod)
dat$residuals = residuals(mod)
ggplot(dat, aes(x=fitted_mod, y=residuals, col = log(revenue))) + facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggplot(dat, aes(x=specDiv.change, y=residuals, col = log(revenue))) + facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggplot(dat, aes(x=days.change, y=residuals, col = log(revenue))) + facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggplot(dat, aes(x=log(revenue.prev), y=residuals, col = log(revenue))) + facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
dev.off()

mod <- glmmTMB(log(revenue) ~ scaled_spec_div*log_days_permit +
    I(log_days_permit^2) + I(scaled_spec_div^2) +
    (1 + scaled_spec_div + log_days_permit|strategy) + (1|people_strategy),
  data = dat)
summary(mod)
AIC(mod)

# 2. model the residuals / variance model
dat$residuals = residuals(mod)
dat$absResid = log(abs(dat$residuals))
mod.cv <- glmmTMB(absResid ~ scaled_spec_div * log_days_permit +
  # I(scaled_spec_div^2) + I(log_days^2) + I(log_days^2) +
    (1 + scaled_spec_div + log_days_permit|strategy) + (1|people_strategy),
    # (1 + log_spec_div + log_days|strategy) +
    # (1|p_holder),
  data = dat)
summary(mod.cv)
AIC(mod.cv)

# create data frame to help interpret output
df_output = data.frame(strategy.permit = unique(dat$strategy),
  "Return_int" = ranef(mod)$cond$strategy$'(Intercept)',
  "Return_slope" = ranef(mod)$cond$strategy$scaled_spec_div,
  "Risk_int" = ranef(mod.cv)$cond$strategy$'(Intercept)',
  "Risk_slope" = ranef(mod.cv)$cond$strategy$scaled_spec_div,
  specComp[match(df_output$strategy.permit, specComp$new_strategy),])
df_output = df_output[,-which(names(df_output)%in%c("new_strategy","strategy.permit.1"))]
write.table(df_output, "portfolio/data-generated/df_output.csv", sep=",",col.names=T, row.names=F)

ggplot(df_output, aes(Return_int, Risk_int, label = strategy.permit)) +
  geom_text(size=3) + xlab("Return (revenue)") + ylab("Risk (CV)")

ggplot(df_output, aes(Return_slope, Risk_slope, label = strategy.permit)) +
  geom_text(size=3) + xlab("Effect of diversification on return (revenue)") +
  ylab("Effect of diversification on risk (CV)")

# 3. model downside risk /
# we can't get situations where people lost money from this
# maybe this is where log-diff model could help too?

# plot random effect intercept vs
strategy.summary = group_by(dat, strategy) %>%
  summarize(sdlogrev = sd(log(revenue)),
    meanrev = mean(log(revenue)),
    specDiv = mean(log(specDiv)))

# plot random effect intercept vs
person.summary = group_by(dat, p_holder) %>%
  summarize(sdlogrev = sd(log(revenue)),
    meanrev = mean(log(revenue)),
    specDiv = mean(log(specDiv)))

strategy.summary$randomInt = ranef(mod)[[1]]$strategy$`(Intercept)` + fixef(mod)[[1]][["(Intercept)"]]
strategy.summary$randomSpec = ranef(mod)[[1]]$strategy$`scaled_spec_div` + fixef(mod)[[1]][["scaled_spec_div"]]
strategy.summary$cv_randomInt = ranef(mod.cv)[[1]]$strategy$`(Intercept)` + fixef(mod.cv)[[1]][["(Intercept)"]]
strategy.summary$cv_randomSpec = ranef(mod.cv)[[1]]$strategy$`scaled_spec_div` + fixef(mod.cv)[[1]][["scaled_spec_div"]]

p <- tidyr::gather(strategy.summary, model, intercept, contains("random")) %>%
  ggplot(aes(specDiv, intercept, color = meanrev)) + geom_point() +
  facet_wrap(~model, scales = "free_y") +
  geom_smooth(se = FALSE, color = "red", method = "lm")
print(p)

# strategy.summary$randomSpecSq = ranef(mod)[[1]]$strategy$`I(scaled_spec_div^2)` + fixef(mod)[[1]][["I(scaled_spec_div^2)"]]
# strategy.summary$cv_randomSpecSq = ranef(mod)[[1]]$strategy$`I(scaled_spec_div^2)` + fixef(mod)[[1]][["I(scaled_spec_div^2)"]]

person.summary$randomInt = ranef(mod)[[1]]$p_holder$`(Intercept)` + fixef(mod)[[1]][["(Intercept)"]]
# person.summary$cv_randomInt = ranef(mod.cv)[[1]]$p_holder$`(Intercept)` + fixef(mod.cv)[[1]][["(Intercept)"]]

###################
pdf("../figs/tmb-separate-exploration-nopholder-re.pdf", width = 10, height = 8)

plot_coefficients_tmb <- function(model, pholder_re = TRUE) {
  ci <- confint(model)
  term <- row.names(ci)
  ci <- as_data_frame(ci)  %>% mutate(term = term)
  ci$estimate <- fixef(model)[[1]]
  ci <- filter(ci, !grepl("Intercept", term))

  re_s <- attr(summary(model)$varcor$cond$strategy, "stddev")
  if (pholder_re)
    re_p <- attr(summary(model)$varcor$cond$p_holder, "stddev")
  if (pholder_re) {
    re <- data.frame(term = c(paste0("re.sd.strategy.", names(re_s)),
        paste0("re.sd.p_holder.", names(re_p))))
    re$estimate <- c(re_s, re_p)
  } else {
    re <- data.frame(term = c(paste0("re.sd.strategy.", names(re_s))))
    re$estimate <- c(re_s)
  }
  re$`2.5 %` <- re$estimate
  re$`97.5 %` <- re$estimate

  ci <- bind_rows(ci, re)
  ggplot(ci, aes(y = estimate, ymax = `2.5 %`, ymin = `97.5 %`, x = term)) +
    geom_pointrange() + coord_flip() + geom_hline(yintercept = 0, lty = 2)
}
plot_coefficients_tmb(mod) %>% print
plot_coefficients_tmb(mod.cv, FALSE) %>% print

# p <- tidyr::gather(person.summary, model, intercept, randomInt, cv_randomInt) %>%
#   ggplot(aes(specDiv, intercept, color = meanrev)) + geom_point(alpha = 0.2) +
#     facet_wrap(~model, scales = "free_y") +
#     geom_smooth(se = FALSE, color = "red")
# print(p)
#
# p <- tidyr::gather(person.summary, model, intercept, randomInt, cv_randomInt) %>%
#   ggplot(aes(meanrev, intercept, color = specDiv)) + geom_point(alpha = 0.2) +
#     facet_wrap(~model, scales = "free_y") +
#     geom_smooth(se = FALSE, color = "red")
# print(p)

p <- tidyr::gather(strategy.summary, model, intercept, contains("random")) %>%
  ggplot(aes(specDiv, intercept, color = meanrev)) + geom_point() +
    facet_wrap(~model, scales = "free_y") +
    geom_smooth(se = FALSE, color = "red", method = "lm")
print(p)

p <- tidyr::gather(strategy.summary, model, intercept, contains("random")) %>%
  ggplot(aes(meanrev, intercept, color = specDiv)) + geom_point() +
    facet_wrap(~model, scales = "free_y") +
    geom_smooth(se = FALSE, color = "red", method = "lm")
print(p)

dev.off()

# Look at the strategy identity of the random effects:
strategy.summary <- arrange(strategy.summary, cv_randomSpec) %>%
  mutate(order = 1:n()) %>%
  mutate(strategy_ordered = reorder(strategy, order))

# p <- person.summary %>%
#   left_join(select(dat, p_holder, strategy) %>% distinct()) %>%
#   left_join(select(strategy.summary, strategy, strategy_ordered)) %>%
#   ggplot(aes(specDiv, cv_randomInt, color = meanrev)) + geom_point(alpha = 0.2) +
#     facet_wrap(~strategy_ordered) +
#     theme(strip.text.x = element_text(size = 4)) +
#     geom_smooth(se = FALSE, color = "red")
# ggsave("../figs/pholder-re-strategies.pdf", width = 26, height = 20, units = "cm")

p <- tidyr::gather(strategy.summary, model, intercept, contains("random")) %>%
  ggplot(aes(intercept, strategy_ordered, color = meanrev)) + geom_point() +
    facet_wrap(~model, scales = "free_x", ncol = 2)
ggsave("../figs/re-strategies.pdf", width = 10, height = 10)

p <- tidyr::gather(strategy.summary, model, intercept, randomSpec, cv_randomSpec) %>%
  ggplot(aes(intercept, strategy_ordered, color = meanrev)) + geom_point() +
    facet_wrap(~model, scales = "free_x", ncol = 1) +
    geom_vline(xintercept = 0, lty = 2)
ggsave("../figs/re-strategies-slim-nopholder-re.pdf", width = 8, height = 10)


# plot fitted vs observed
fitted_mod <- fitted.values(mod)
fitted_mod_cv <- fitted.values(mod.cv)
residuals_cv <- residuals(mod.cv)

# ggplot(dat, aes(x=fitted_mod, y=log(revenue), col = log(revenue))) +
#   facet_wrap(~ strategy, scale="free") +
#   geom_point(alpha = 0.3) +
#   geom_abline(slope=1, intercept=0, color = "grey50")
# ggsave("residuals_rev/fitted_v_observed.pdf", width = 40, height = 40, units = "cm")

# plot fitted vs residuals
ggplot(dat, aes(x=fitted_mod, y=residuals, col = log(revenue))) + facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/fitted_v_residuals.png", width = 40, height = 40, units = "cm")

ggplot(dat, aes(x=fitted_mod_cv, y=residuals_cv, col = log(revenue))) + facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/fitted_v_residuals_cv.png", width = 40, height = 40, units = "cm")

# plot specDiv vs residuals
dat$strategy_ordered <- NULL
dat <- left_join(dat, select(strategy.summary, strategy, strategy_ordered))
p <- ggplot(dat, aes(x=scaled_spec_div, y=residuals, col = log(revenue))) +
  facet_wrap(~strategy_ordered) +
  geom_point(alpha = 0.1) + geom_hline(yintercept=0) +
  ylim(-2,2) +
  theme(strip.text.x = element_text(size = 4)) +
  geom_smooth(se = FALSE, color = "red")
ggsave("residuals_rev/specdiv_v_residuals.pdf", width = 28, height = 22, units = "cm")

p <- ggplot(dat, aes(x=scaled_spec_div, y=residuals)) +
  geom_point(alpha = 0.1) + geom_hline(yintercept=0) +
  ylim(-2,2) +
  geom_smooth(se = FALSE, color = "red")

p <- ggplot(dat, aes(x=specDiv, y=residuals_cv, col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.1) + geom_hline(yintercept=0) +
  geom_smooth(se = FALSE, color = "red")
ggsave("residuals_rev/specdiv_v_residuals_cv.png", width = 40, height = 40, units = "cm")

# plot vessel length vs residuals
# everything looks okay here except for miscellaneous groundfish which curve
# up
ggplot(dat, aes(x=log_length, y=residuals, col = log(revenue))) +
  facet_wrap(~strategy, scale="free_x") +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/length_v_residuals.png", width = 40, height = 40, units = "cm")

ggplot(dat, aes(x=log_length, y=residuals_cv, col = log(revenue))) +
  facet_wrap(~strategy, scale="free_x") +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/length_v_residuals_cv.png", width = 40, height = 40, units = "cm")

# plot days vs residuals
ggplot(dat, aes(x=log_days, y=residuals, col = log(revenue))) +
  facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/days_v_residuals.png", width = 40, height = 40, units = "cm")

ggplot(dat, aes(x=log_days, y=residuals_cv, col = log(revenue))) +
  facet_wrap(~strategy) +
  geom_point(alpha = 0.3) + geom_hline(yintercept=0)
ggsave("residuals_rev/days_v_residuals_cv.png", width = 40, height = 40, units = "cm")

## ## THINGS I'M SEEING HERE
## # 1. Many of the strategies have no variance w/respect to the predictors. Specifically, permits that are
## #   targeting 1 spp (herring, halibut, sablefish) are essentially ~ 0. We can't do a good job of estimating
## #   random effects on slopes for these strategies, and probably need to just set them to 0.
## #
## # 2. For the differenced model, inclusion of previous revenue may be important as a covariate -- some strategies
## #   have negative relationships, contrary to the "rich get richer" idea.
## #
## # 3. The distribution of the log differenced revenue isn't close to normal -- it's a t distribution with
## #   df ~ 2.4, so changing to an alternate transformation / distribution would help some of the residual patterns
## #   fit.t = fitdistr(testData$rev.pctChange, "t")
## #
## # 4. I found that many of the extreme events occurred for people making very little money. When we
## #   restrict the analysis to people making> 1000 or more (or any floor) the distributions are much more normal
##
## # Can alternate distribution help w/residuals? What this shows is that reducing the extreme tails worsens
## # the model R^2 a bit, but improves the QQ plot and the scale - location plot (eliminating the U or V shape)
## # As an example of the residuals w/permit S15B
## lm.norm = lm(rev.pctChange ~ -1 + specDiv.pctChange+days.pctChange + log(revenue.prev),
##   data = testData[which(testData$strategy=="S15B"),])
## # change to standard normal based on cdf / percentiles.
## fit.t = fitdistr(testData$rev.pctChange, "t")
## testData$rev.pctChange.transform = qnorm(pt(testData$rev.pctChange, df = fit.t$estimate[3]), 0, 1)
## lm.norm2 = lm(rev.pctChange.transform ~ as.factor(year) + specDiv.pctChange+days.pctChange + log(revenue.prev),
##   data = testData[which(testData$strategy=="S15B"),])
