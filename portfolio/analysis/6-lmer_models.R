# Experiment with models 
# by fitting them quickly with TMB

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

#####################################################################################
# UNTRANSFORMED DATA. With threshold of 5 yrs / person, about the same size as
# differenced dataset. Most pasted from Sean's "7-Separate-models.Rmd" files
#####################################################################################
dat = readRDS(file="../data-generated/cfec-annual-for-modeling.rds")
dat <- group_by(dat, p_holder) %>%
  mutate(nyr = length(unique(year))) %>%
  mutate(range_div = diff(range(specDiv))) %>%
  as_data_frame() %>%
  filter(nyr >= 5) #%>%
  # filter(range_div > 0)
nrow(dat)

dat$log_spec_div <- scale(log(dat$specDiv))
dat$log_length <- scale(log(dat$length + 1))
dat$log_weight <- scale(log(dat$weight + 1))
dat$log_days <- scale(log(dat$days + 1))
dat$log_npermit <- scale(log(dat$npermit))

# Filters: remove people-year combinations making < $5000
dat = dat[which(dat$revenue >= 5000), ]

# Downsample for speed of testing 
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)/2)
set.seed(1)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

# many different strategies, need to model only most common, 
top.strategies = names(rev(sort(table(dat$strategy)))[1:60])
dat = dat[dat$strategy%in%top.strategies, ]
nrow(dat)

# LMER models here -- conditional R^2 ~ 0.8
# I also played with including length, but some of that is already in strategy intercepts
# mod <- lmer(log(revenue) ~ log_spec_div * log_days + log_npermit + 
#     (1 + log_spec_div + log_days|strategy) +
#     (1|p_holder),
#     data = dat)

library(glmmTMB)
mod <- glmmTMB(log(revenue) ~ log_spec_div * log_days + log_npermit + 
    (1 + log_spec_div + log_days|strategy) +
    (1|p_holder),
    data = dat)
# r.squaredGLMM(mod)

dat$residuals = residuals(mod)

# 2. model the residuals / variance model 
dat$absResid = log(abs(dat$residuals))
mod.cv <- glmmTMB(absResid ~ log_spec_div * log_days + log_npermit +
    (1 + log_spec_div + log_days|strategy)+
    (1|p_holder) , data = dat)
# r.squaredGLMM(mod.cv)

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
strategy.summary$randomSpec = ranef(mod)[[1]]$strategy$`log_spec_div` + fixef(mod)[[1]][["log_spec_div"]]
strategy.summary$cv_randomInt = ranef(mod.cv)[[1]]$strategy$`(Intercept)` + fixef(mod.cv)[[1]][["(Intercept)"]]
strategy.summary$cv_randomSpec = ranef(mod.cv)[[1]]$strategy$`log_spec_div` + fixef(mod.cv)[[1]][["log_spec_div"]]

person.summary$randomInt = ranef(mod)[[1]]$p_holder$`(Intercept)` + fixef(mod)[[1]][["(Intercept)"]]     
person.summary$cv_randomInt = ranef(mod.cv)[[1]]$p_holder$`(Intercept)` + fixef(mod.cv)[[1]][["(Intercept)"]]     

# strategy.summary$randomInt = coef(mod)$strategy$`(Intercept)`
# strategy.summary$randomSpec = coef(mod)$strategy$`log_spec_div`
# strategy.summary$randomSpecDay = coef(mod)$strategy$`log_spec_div:log_days`
# strategy.summary$cv_randomInt = coef(mod.cv)$strategy$`(Intercept)`
# strategy.summary$cv_randomSpec = coef(mod.cv)$strategy$`log_spec_div`
# strategy.summary$cv_randomSpecDay = coef(mod.cv)$strategy$`log_spec_div:log_days`
# 
# person.summary$randomInt = coef(mod)$p_holder$`(Intercept)`
# person.summary$cv_randomInt = coef(mod.cv)$p_holder$`(Intercept)`

###################
pdf("../figs/tmb-separate-exploration.pdf", width = 10, height = 8)
# plot_coefficients_lmer <- function(model) {
#   broom::tidy(model, conf.int = TRUE) %>%
#   filter(term != "(Intercept)") %>%
#   filter(!grepl("cor_", term)) %>%
#   # filter(!grepl("npermit", term)) %>%
#   mutate(conf.low = ifelse(is.na(std.error), estimate, conf.low)) %>%
#   mutate(conf.high = ifelse(is.na(std.error), estimate, conf.high)) %>%
#   ggplot(aes(y = estimate, ymax = conf.low, ymin = conf.high, x = term)) +
#   geom_pointrange() +
#   coord_flip() +
#   geom_hline(yintercept = 0, lty = 2)
# }
# plot_coefficients_lmer(mod) %>% print
# plot_coefficients_lmer(mod.cv) %>% print

plot_coefficients_tmb <- function(model) {
  ci <- confint(model)
  term <- row.names(ci)
  ci <- as_data_frame(ci)  %>% mutate(term = term)
  ci$estimate <- fixef(model)[[1]]
  ci <- filter(ci, !grepl("Intercept", term))
  ggplot(ci, aes(y = estimate, ymax = `2.5 %`, ymin = `97.5 %`, x = term)) +
    geom_pointrange() + coord_flip() + geom_hline(yintercept = 0, lty = 2)
}
plot_coefficients_tmb(mod) %>% print
plot_coefficients_tmb(mod.cv) %>% print

p <- tidyr::gather(strategy.summary, model, intercept, randomInt:cv_randomSpec) %>% 
  ggplot(aes(specDiv, intercept, color = meanrev)) + geom_point() +
    facet_wrap(~model, scales = "free_y") +
    geom_smooth(se = FALSE, color = "red", method = "lm")
print(p)

p <- tidyr::gather(strategy.summary, model, intercept, randomInt:cv_randomSpec) %>% 
  ggplot(aes(meanrev, intercept, color = specDiv)) + geom_point() +
    facet_wrap(~model, scales = "free_y") +
    geom_smooth(se = FALSE, color = "red", method = "lm")
print(p)

p <- tidyr::gather(person.summary, model, intercept, randomInt, cv_randomInt) %>% 
  ggplot(aes(specDiv, intercept, color = meanrev)) + geom_point(alpha = 0.2) +
    facet_wrap(~model, scales = "free_y") +
    geom_smooth(se = FALSE, color = "red")
print(p)

p <- tidyr::gather(person.summary, model, intercept, randomInt, cv_randomInt) %>% 
  ggplot(aes(meanrev, intercept, color = specDiv)) + geom_point(alpha = 0.2) +
    facet_wrap(~model, scales = "free_y") +
    geom_smooth(se = FALSE, color = "red")
print(p)
dev.off()


# plot fitted vs observed
ggplot(dat, aes(x=fitted.values(mod), y=log(revenue), col = log(revenue))) + 
  facet_wrap(~ strategy, scale="free") +
  geom_point(alpha = 0.7) + 
  geom_abline(slope=1, intercept=0, color = "grey50")
ggsave("residuals_rev/fitted_v_observed.pdf", width = 40, height = 40, units = "cm")

# plot fitted vs residuals
ggplot(dm, aes(x=fitted.values(mod), y=residuals, col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals_rev/fitted_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot specDiv vs residuals
ggplot(dat, aes(x=specDiv, y=residuals, col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals_rev/specdiv_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot days.pctChange vs residuals
ggplot(dat, aes(x=days, y=residuals, col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals_rev/days_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot previous year vs residuals
ggplot(dat, aes(x=year, y=residuals, col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals_rev/year_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot fitted vs log abs residuals
ggplot(dat, aes(x=fitted.values(mod), y=sqrt(abs(residuals), col = log(revenue)))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals_rev/fitted_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")

# plot specDiv vs residuals
ggplot(dm, aes(x=specDiv, y=sqrt(abs(residuals)), col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals_rev/specdiv_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")

# plot days.pctChange vs residuals
ggplot(dat, aes(x=days, y=sqrt(abs(residuals)), col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals_rev/days_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")

# plot previous year vs residuals
ggplot(dat, aes(x=year, y=sqrt(abs(residuals)), col = log(revenue))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals_rev/year_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")




###############################################################################################
# DIFFERENCED DATA
###############################################################################################
# LOAD DATA
#cfecAnnual.diff = readRDS(file="diff_linearModeling_complete.rds")
cfecAnnual.diff = readRDS(file="diff_linearModeling_complete_all.rds")

# Calculate some differenced/derived variables
cfecAnnual.diff$specDiv.pctChange = log(cfecAnnual.diff$specDiv / cfecAnnual.diff$specdiv.prev)
cfecAnnual.diff$days.pctChange = log((cfecAnnual.diff$days+1) / (cfecAnnual.diff$days.prev+1))
cfecAnnual.diff$rev.pctChange = log(cfecAnnual.diff$revenue / cfecAnnual.diff$revenue.prev)

#ggplot(cfecAnnual.diff, aes(x=log(revenue.prev), y = rev.pctChange)) + geom_point()
# restrict analysis to people who don't change strategies (keeps ~ 90%)
cfecAnnual.diff = cfecAnnual.diff[which(cfecAnnual.diff$strategy == cfecAnnual.diff$strategy.prev), ]

# also filter out people making < 1000 / year
cfecAnnual.diff = cfecAnnual.diff[which(cfecAnnual.diff$revenue >= 5000), ]

# 4200 different strategies, need to model only most common, 
top.strategies = names(rev(sort(table(cfecAnnual.diff$strategy)))[1:100])
cfecAnnual.diff = cfecAnnual.diff[cfecAnnual.diff$strategy%in%top.strategies, ]
testData = cfecAnnual.diff

# calcualte variance versus mean across strategies 
strategy.levels = lapply(lapply(strsplit(as.character(testData$strategy), " "), substr, 1, 1), paste, collapse=" ")
testData$strategy.levels = unlist(strategy.levels)

# crab = D/K/T
group_by(testData[grep("S",testData$strategy.levels),], strategy) %>% 
  summarize(mean = mean(revenue+revenue.prev)/2, days = mean(days+days.prev)/2,
    sd = sd(rev.pctChange), div = mean(specDiv + specdiv.prev)/2, 
    permits = mean(npermit)) %>% 
  ggplot(aes(x = permits, y = log(mean), col = permits)) + geom_point()


# Convert skewed data to something more like normal
fit.t = MASS::fitdistr(testData$rev.pctChange, "t")
testData$rev.pctChange.raw = testData$rev.pctChange
testData$rev.pctChange = qnorm(pt(testData$rev.pctChange, df = fit.t$estimate[3]), 0, 1)

# FIT SOME QUICK MODELS WITH LMER
# random slope, fixed intercept at 0 because log-diff data has mean 0
# boxplot(testData$rev.pctChange ~ testData$strategy, outline=F)
#lm1 = lm(rev.pctChange ~ -1 + specDiv.pctChange + (specDiv.pctChange:strategy+days.pctChange:strategy), data = testData)
lm2 = lmer(rev.pctChange ~ -1 + (-1+specDiv.pctChange+days.pctChange|strategy) + 
    specDiv.pctChange + days.pctChange, data = testData) 
r.squaredGLMM(lm2)

testData$residuals <- residuals(lm2)

# plot fitted vs observed -- non salmon specialists
non.sal= which(substr(testData$strategy,1,1)!="S")
ggplot(testData[non.sal,], aes(x=fitted.values(lm2)[non.sal], y=rev.pctChange, color = strategy)) + 
  geom_point(alpha = 0.7) + 
  theme(aspect.ratio=1) + 
  geom_abline(slope=1, intercept=0,color = "grey50")
ggsave("residuals/fitted_v_observed_nonSalmon.pdf", width = 40, height = 40, units = "cm")

# plot fitted vs observed
ggplot(testData, aes(x=fitted.values(lm2), y=rev.pctChange, color = log(revenue.prev))) + 
  facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + 
  geom_abline(slope=1, intercept=0, color = "grey50")
ggsave("residuals/fitted_v_observed.pdf", width = 40, height = 40, units = "cm")

# plot fitted vs residuals
ggplot(testData, aes(x=fitted.values(lm2), y=residuals, color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals/fitted_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot specDiv vs residuals
ggplot(testData, aes(x=specDiv.pctChange, y=residuals, color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals/specdiv_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot days.pctChange vs residuals
ggplot(testData, aes(x=days.pctChange, y=residuals, color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals/changeDays_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot previous revenue vs residuals
ggplot(testData, aes(x=log(revenue.prev), y=residuals, color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals/prevRev_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot previous year vs residuals
ggplot(testData, aes(x=year, y=residuals, color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_hline(yintercept=0)
ggsave("residuals/year_v_residuals.pdf", width = 40, height = 40, units = "cm")

# plot fitted vs log abs residuals
ggplot(testData, aes(x=fitted.values(lm2), y=log(abs(residuals)), color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals/fitted_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")

# plot specDiv vs residuals
ggplot(testData, aes(x=specDiv.pctChange, y=sqrt(abs(residuals)), color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals/specdiv_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")

# plot days.pctChange vs residuals
ggplot(testData, aes(x=days.pctChange, y=sqrt(abs(residuals)), color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals/changeDays_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")

# plot previous revenue vs residuals
ggplot(testData, aes(x=log(revenue.prev), y=sqrt(abs(residuals)), color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals/prevRev_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")

# plot previous year vs residuals
ggplot(testData, aes(x=year, y=sqrt(abs(residuals)), color = log(revenue.prev))) + facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, color = "grey50")
ggsave("residuals/year_v_sqrtabsresid.pdf", width = 40, height = 40, units = "cm")

# Another model validation = prediction
# We can also do some diagnostics and hold out ~ 3 people per strategy, then predict their response
# 
testData$row = seq(1, dim(testData)[1]) # id 
#testData.test = group_by(testData, strategy) %>% 
#  sample_n(3)
test_set = sample(seq(1, dim(testData)[1]), size=20000, replace=F)
testData.test = testData[test_set,]
testData.train = testData[-test_set,] # training set
lm.train = lmer(rev.pctChange ~ -1 + (-1+specDiv.pctChange+days.pctChange|strategy) + 
    specDiv.pctChange + days.pctChange, data = testData.train) 
testData.test$pred = predict(lm.train, testData.test)

ggplot(testData.test, aes(x=pred, y=rev.pctChange, color = log(revenue.prev))) + 
  facet_wrap(~strategy, scale="free") +
  geom_point(alpha = 0.7) + 
  geom_abline(slope=1, intercept=0, color = "grey50")
ggsave("residuals/fitted_v_observed_holdout.pdf", width = 40, height = 40, units = "cm")


## THINGS I'M SEEING HERE
# 1. Many of the strategies have no variance w/respect to the predictors. Specifically, permits that are
#   targeting 1 spp (herring, halibut, sablefish) are essentially ~ 0. We can't do a good job of estimating
#   random effects on slopes for these strategies, and probably need to just set them to 0. 
#
# 2. For the differenced model, inclusion of previous revenue may be important as a covariate -- some strategies
#   have negative relationships, contrary to the "rich get richer" idea. 
#
# 3. The distribution of the log differenced revenue isn't close to normal -- it's a t distribution with 
#   df ~ 2.4, so changing to an alternate transformation / distribution would help some of the residual patterns
#   fit.t = fitdistr(testData$rev.pctChange, "t")
# 
# 4. I found that many of the extreme events occurred for people making very little money. When we 
#   restrict the analysis to people making> 1000 or more (or any floor) the distributions are much more normal

# Can alternate distribution help w/residuals? What this shows is that reducing the extreme tails worsens
# the model R^2 a bit, but improves the QQ plot and the scale - location plot (eliminating the U or V shape) 
# As an example of the residuals w/permit S15B
lm.norm = lm(rev.pctChange ~ -1 + specDiv.pctChange+days.pctChange + log(revenue.prev), 
  data = testData[which(testData$strategy=="S15B"),]) 
# change to standard normal based on cdf / percentiles. 
fit.t = fitdistr(testData$rev.pctChange, "t")
testData$rev.pctChange.transform = qnorm(pt(testData$rev.pctChange, df = fit.t$estimate[3]), 0, 1)
lm.norm2 = lm(rev.pctChange.transform ~ as.factor(year) + specDiv.pctChange+days.pctChange + log(revenue.prev), 
  data = testData[which(testData$strategy=="S15B"),]) 