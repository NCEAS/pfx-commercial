library(dplyr)
library(ggplot2)
source("portfolio/analysis/cull-dat.R")
dat <- cullDat(diff = TRUE)

# downsample for speed of testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.30)
set.seed(12)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

dat$strategy_year <- paste(dat$strategy, dat$year, sep = ":")
library(lme4)

dat <- mutate(dat,
  revenue_change = log(revenue/revenue.prev),
  days_change = log((days_permit+1)/(days_permit.prev+1)),
  spec_change = log(specDiv/specdiv.prev),
  specdiv.prev.zero = specdiv.prev - 1)
range(dat$specdiv.prev.zero)

dat <- mutate(dat, inc = ifelse(spec_change > 0, "inc", ifelse(spec_change == 0, "zero", "dec")))

m.c <- lmer(log(revenue) ~
  # b1(spec_change, bp)*days_change +
  # b2(spec_change, bp)*days_change +
  b1(spec_change, bp)*specdiv.prev.zero +
  b2(spec_change, bp)*specdiv.prev.zero +
  (-1 + b1(spec_change, bp) + b2(spec_change, bp) | strategy) +
  (1|strategy_year),
  data = dat, offset = log(revenue.prev), REML = T)
summary(m.c)

m.d <- lmer(log(revenue) ~
  spec_change*specdiv.prev.zero +
  (-1 + spec_change | strategy) +
  (1|strategy_year),
  data = dat, offset = log(revenue.prev), REML = T)
summary(m.d)
AIC(m.d)

m.e <- lmer(log(revenue) ~
  spec_change*days_change +
  spec_change*specdiv.prev.zero +
  (-1 + spec_change + days_change | strategy) +
  (1|strategy_year),
  data = dat, offset = log(revenue.prev), REML = T)
summary(m.3)
AIC(m.d)

library(broom)
tidy(m.c, conf.int = T) %>% ggplot(aes(estimate, term)) + geom_point() +
  geom_segment(aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
    geom_vline(xintercept=0,lty=2)

tidy(m.d, conf.int = T) %>% ggplot(aes(estimate, term)) + geom_point() +
  geom_segment(aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
    geom_vline(xintercept=0,lty=2)

tidy(m.e, conf.int = T) %>% ggplot(aes(estimate, term)) + geom_point() +
  geom_segment(aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
    geom_vline(xintercept=0,lty=2)

# m.aug <- augment(m.c)
# m.aug <- augment(m.d)
m.aug <- augment(m.e)
m.aug$spec_change <- dat$spec_change
m.aug <- mutate(m.aug, inc = ifelse(spec_change > 0, "inc", ifelse(spec_change == 0, "zero", "dec")))
p <- ggplot(m.aug, aes(spec_change, log(abs(.resid)), colour = inc, group = inc)) +
  geom_point(alpha = 0.05) +
  facet_wrap(~strategy) + geom_smooth(se=F, colour = "black", method = "lm")
p

m.aug$days_change <- dat$days_change
m2 <- lmer(log(abs(.resid)) ~ days_change +
  b1(spec_change, bp)*specdiv.prev.zero +
  b2(spec_change, bp)*specdiv.prev.zero +
  (b1(spec_change, bp) + b2(spec_change, bp) | strategy),
  data = m.aug, REML = T)
summary(m2)

tidy(m2, conf.int = T) %>% ggplot(aes(estimate, term)) + geom_point() +
  geom_segment(aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
    geom_vline(xintercept=0,lty=2)

# gg <- dat %>% group_by(strategy) %>% summarise(mean_div = mean(specDiv),
  # median_div = median(specDiv))
gg <- dat %>% group_by(strategy) %>%
  summarise(mean_div = mean(specDiv), nn = length(unique(p_holder)))
library(scales)
library(ggrepel)
library(viridis)

res2 <- data.frame(strategy = row.names(coef(m2)$strategy))
res2$eff_rev <- coef(m.e)$strategy[,"spec_change"]
res2$dec_rev <- coef(m.c)$strategy[,"b1(spec_change, bp)"]
res2$inc_rev <- coef(m.c)$strategy[,"b2(spec_change, bp)"]
res2$dec <- coef(m2)$strategy[,"b1(spec_change, bp)"]
res2$inc <- coef(m2)$strategy[,"b2(spec_change, bp)"]

p1 <- res2 %>%
  mutate(strategy = as.character(strategy)) %>%
  inner_join(gg) %>%
  ggplot(aes(x = eff_rev, y = inc)) +
  # ggplot(aes(x = inc_rev, y = inc)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_text_repel(aes(label = strategy, x = eff_rev, y = inc),
  # geom_text_repel(aes(label = strategy, x = inc_rev, y = inc),
    size = 3, colour = "grey50") +
   scale_color_viridis() +
  geom_point(aes(color = mean_div, size = nn)) +
  theme_light() +
  xlab("Effect on revenue") +
  ylab("Effect of generalizing on variability") +
  labs(colour = "Mean sp.\ndiversity", size = "Number of\npermits")

p2 <- res2 %>%
  mutate(strategy = as.character(strategy)) %>%
  inner_join(gg) %>%
  ggplot(aes(x = eff_rev, y = -dec)) +
  # ggplot(aes(x = inc_rev, y = dec)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_text_repel(aes(label = strategy, x = eff_rev, y = -dec),
  # geom_text_repel(aes(label = strategy, x = inc_rev, y = dec),
    size = 3, colour = "grey50") +
   scale_color_viridis() +
  geom_point(aes(color = mean_div, size = nn)) +
  theme_light() +
  xlab("Effect on revenue") +
  ylab("Effect of specializing on variability") +
  labs(colour = "Mean sp.\ndiversity", size = "Number of\npermits")

grid_arrange_shared_legend(p1, p2, ncol = 2, position = "right")
