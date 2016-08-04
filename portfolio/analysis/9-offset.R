library(dplyr)
library(ggplot2)
source("portfolio/analysis/cull-dat.R")
dat <- cullDat(diff = TRUE)

# downsample for speed of testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.33)
set.seed(123)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

# dat <- dat %>% group_by(strategy) %>% mutate(n=n()) %>% filter(n > 30)
# nrow(dat)

dat <- mutate(dat, 
  revenue_change = log(revenue/revenue.prev),
  days_change = log((days_permit+1)/(days_permit.prev+1)),
  spec_change = log(specDiv/specdiv.prev))

library(lme4)
m <- lmer(log(revenue)~days_change*spec_change +
  I(days_change^2) + I(spec_change^2) +
  (1 + spec_change + days_change|strategy), 
  data = dat, offset = log(revenue.prev))
summary(m)
library(broom)
broom::tidy(m, conf.int = T) %>% ggplot(aes(estimate, term)) + geom_point() + 
  geom_segment(aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
    geom_vline(xintercept=0,lty=2)
head(augment(m))
head(tidy(m, conf.int = T))
glance(m)
re <- coef(m)$strategy
re$strategy <- row.names(re)
row.names(re) <- NULL
library(tidyr)
gather(re, term, estimate, -strategy) %>%
  ggplot(aes(estimate, strategy)) + geom_point() +
  facet_wrap(~term)
fixef(m)
m.aug <- augment(m)

m2 <- lmer(log(abs(.resid))~days_change*spec_change + 
  (1 + spec_change + days_change|strategy), 
  data = m.aug)
summary(m2)
broom::tidy(m2, conf.int = T) %>% ggplot(aes(estimate, term)) + geom_point() + 
  geom_segment(aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
    geom_vline(xintercept=0,lty=2)
re2 <- coef(m2)$strategy
re2$strategy <- row.names(re2)
row.names(re2) <- NULL
gather(re2, term, estimate, -strategy) %>%
  ggplot(aes(estimate, strategy)) + geom_point() +
  facet_wrap(~term)

gg <- dat %>% group_by(strategy) %>% summarise(mean_div = mean(specDiv))
re2$mean_div <- NULL
re2 <- inner_join(re2, gg)
re <- inner_join(re, gg)
p1 <- ggplot(re2, aes(mean_div, `(Intercept)`)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = F)
p2 <- ggplot(re2, aes(mean_div, spec_change)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = F)
p3 <- ggplot(re, aes(mean_div, `(Intercept)`)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = F)
p4 <- ggplot(re, aes(mean_div, spec_change)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = F)
gridExtra::grid.arrange(p1, p3, p2, p4)

re2_sigma <- re2
names(re2_sigma)[1:4] <- paste0("sigma_", names(re2_sigma)[1:4])
res <- inner_join(re, select(re2_sigma, -mean_div))

ggplot(res, aes(spec_change, sigma_spec_change)) + geom_text(aes(label = strategy)) +
  geom_vline(xintercept=0) + geom_hline(yintercept=0)

