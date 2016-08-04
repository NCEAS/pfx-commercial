library(dplyr)
library(ggplot2)
source("portfolio/analysis/cull-dat.R")
dat <- cullDat(diff = TRUE)

# downsample for speed of testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.25)
set.seed(123)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

# dat <- dat %>% group_by(strategy) %>% mutate(n=n()) %>% filter(n > 30)
# nrow(dat)

dat <- mutate(dat,
  revenue_change = log(revenue/revenue.prev),
  days_change = log((days_permit+1)/(days_permit.prev+1)),
  spec_change = log(specDiv/specdiv.prev))

dat$strategy_year <- paste(dat$strategy, dat$year, sep = ":")
library(lme4)
m <- lmer(log(revenue)~
  days_change*spec_change +
  # I(days_change^2) + I(spec_change^2) +
  # poly(days_change, 2) + poly(spec_change) +
  (1 + spec_change + days_change|strategy) + (1|strategy_year), 
  data = dat, offset = log(revenue.prev))

ranef(m)
summary(m)
library(broom)
broom::tidy(m, conf.int = T) %>% ggplot(aes(estimate, term)) + geom_point() +
  geom_segment(aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
    geom_vline(xintercept=0,lty=2)
ggsave("portfolio/figs/offset-mean-coefs.pdf", width = 8, height = 8)
re <- coef(m)$strategy
re$strategy <- row.names(re)
row.names(re) <- NULL
library(tidyr)
gather(re, term, estimate, -strategy) %>%
  ggplot(aes(estimate, strategy)) + geom_point() +
  facet_wrap(~term)
ggsave("portfolio/figs/offset-mean-ranefs.pdf", width = 8, height = 10)
m.aug <- augment(m)

ggplot(m.aug, aes(spec_change, log(abs(.resid)), colour = days_change)) + 
  geom_point(alpha = 0.1) +
  facet_wrap(~strategy) + stat_smooth(se=F)
ggsave("portfolio/figs/offset-panel.pdf", width = 12, height = 12)

m.aug$year <- dat$year
ggplot(m.aug, aes(year, .resid, colour = days_change)) + 
  geom_point(alpha = 0.1) +
  facet_wrap(~strategy)
ggsave("portfolio/figs/offset-panel-time.pdf", width = 18, height = 18)

m2 <- lmer(log(abs(.resid))~days_change*spec_change + 
  # I(days_change^2) + I(spec_change^2) +
  (1 + spec_change + days_change|strategy), 
  data = m.aug)
# m3 <- lmer(log(abs(.resid))~days_change*spec_change + 
#   (1 + spec_change + days_change|strategy), 
#   data = filter(m.aug, .resid < 0))
# m2 <- m3
summary(m2)
broom::tidy(m2, conf.int = T) %>% ggplot(aes(estimate, term)) + geom_point() +
  geom_segment(aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
    geom_vline(xintercept=0,lty=2)
ggsave("portfolio/figs/offset-sigma-coefs.pdf", width = 8, height = 8)
re2 <- coef(m2)$strategy
re2$strategy <- row.names(re2)
row.names(re2) <- NULL
gather(re2, term, estimate, -strategy) %>%
  ggplot(aes(estimate, strategy)) + geom_point() +
  facet_wrap(~term)
ggsave("portfolio/figs/offset-sigma-ranefs.pdf", width = 8, height = 10)

gg <- dat %>% group_by(strategy) %>% summarise(mean_div = mean(specDiv))
re2$mean_div <- NULL
re$mean_div <- NULL
re2 <- inner_join(re2, gg)
re <- inner_join(re, gg)
p1 <- ggplot(re2, aes(mean_div, `(Intercept)`)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = F) +
    ggtitle("g0")
p2 <- ggplot(re2, aes(mean_div, spec_change)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = F) +
    ggtitle("g1")
p3 <- ggplot(re, aes(mean_div, `(Intercept)`)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = F) +
    ggtitle("b0")
p4 <- ggplot(re, aes(mean_div, spec_change)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = F) +
    ggtitle("b1")
pdf("portfolio/figs/offset-group-effects-downside.pdf", width=9,height=9)
gridExtra::grid.arrange(p1, p3, p2, p4)
dev.off()

summary(lm(`(Intercept)`~poly(mean_div, 2), data = re2))
summary(lm(spec_change~poly(mean_div, 2), data = re2))
summary(lm(`(Intercept)`~poly(mean_div, 2), data = re))
summary(lm(spec_change~poly(mean_div, 2), data = re))

re2_sigma <- re2
names(re2_sigma)[1:4] <- paste0("sigma_", names(re2_sigma)[1:4])
res <- inner_join(re, select(re2_sigma, -mean_div))

ggplot(res, aes(spec_change, sigma_spec_change)) + geom_text(aes(label = strategy)) +
  geom_vline(xintercept=0) + geom_hline(yintercept=0)
ggsave("portfolio/figs/offset-bivariate-slopes.pdf", width = 6, height = 6)

# Example of including strategy-year random effects
dat$strategyYear <- paste(dat$strategy, dat$year, sep = ":")
m <- lmer(log(revenue)~ -1+days_change*spec_change +
    I(days_change^2) + I(spec_change^2) +
    (-1+spec_change + days_change|strategy) + (1|strategyYear),
  data = dat, offset = log(revenue.prev))

coef.names = rownames(ranef(m)$strategyYear)
coef.year = substr( coef.names, nchar(coef.names) - 3, nchar(coef.names))
coef.strat = substr( coef.names, 1, nchar(coef.names) - 5)
df = data.frame("strategy"=coef.strat, "year" = coef.year, "est" =ranef(m)$strategyYear$'(Intercept)')

ggplot(df, aes(year, est, group = strategy)) + geom_line() + facet_wrap(~ strategy)

