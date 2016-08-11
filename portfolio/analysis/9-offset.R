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

# dat <- dat %>% group_by(strategy) %>% mutate(n=n()) %>% filter(n > 30)
# nrow(dat)

dat <- mutate(dat,
  revenue_change = log(revenue/revenue.prev),
  days_change = log((days_permit+1)/(days_permit.prev+1)),
  spec_change = log(specDiv/specdiv.prev))

dat <- mutate(dat, inc = ifelse(spec_change > 0, "inc", ifelse(spec_change == 0, "zero", "dec")))
p <- ggplot(dat, aes(spec_change, revenue_change, colour = inc, group = inc)) + 
  geom_point(alpha = 0.05) +
  facet_wrap(~strategy) + geom_smooth(se=F, colour = "black", method = "lm")
ggsave("portfolio/figs/offset-panel-mean.pdf", width = 12, height = 12)

dat$strategy_year <- paste(dat$strategy, dat$year, sep = ":")
library(lme4)
m <- lmer(log(revenue)~
  days_change*spec_change +
  # I(days_change^2) + I(spec_change^2) +
  # poly(days_change, 2) + poly(spec_change) +
  (-1 + spec_change |strategy) + (1|strategy_year), 
  data = dat, offset = log(revenue.prev))

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

m.aug <- mutate(m.aug, inc = ifelse(spec_change > 0, "inc", ifelse(spec_change == 0, "zero", "dec")))
p <- ggplot(filter(m.aug, .resid < 0), aes(spec_change, log(abs(.resid)), colour = inc, group = inc)) + 
  geom_point(alpha = 0.05) +
  facet_wrap(~strategy) + geom_smooth(se=F, colour = "black", method = "lm")
ggsave("portfolio/figs/offset-panel-down-only.pdf", width = 12, height = 12)
p <- ggplot(m.aug, aes(spec_change, log(abs(.resid)), colour = inc, group = inc)) + 
  geom_point(alpha = 0.05) +
  facet_wrap(~strategy) + geom_smooth(se=F, colour = "black", method = "lm")
ggsave("portfolio/figs/offset-panel.pdf", width = 12, height = 12)
p <- ggplot(m.aug, aes(spec_change, .resid, colour = inc, group = inc)) + 
  geom_point(alpha = 0.05) +
  facet_wrap(~strategy) + geom_smooth(se=F, colour = "black", method = "lm")
ggsave("portfolio/figs/offset-panel-mean-resid.pdf", width = 12, height = 12)

my_lm <- function(y, x) {
 coef(lm(y~x))[[2]]
}
effs <- group_by(m.aug, strategy, inc) %>% 
  summarise(eff = my_lm(log(abs(.resid)), spec_change),
    n = n())

# x <- filter(m.aug, strategy == "B61B", inc == "inc")
# filter(effs, strategy == "B61B")
# plot(x$spec_change, x$.resid)

filter(effs, inc != "zero", n > 30) %>% select(-n) %>% 
  tidyr::spread(inc, eff) %>%
  mutate(x1 = 0, x2 = 1) %>%
  ggplot(aes(x = x1, xend = x2, y = dec, yend = inc)) + 
  geom_segment(alpha = 0.5) +
  geom_text(aes(label = strategy, x = x1, y = dec)) +
  geom_text(aes(label = strategy, x = x2, y = inc)) +
  geom_hline(yintercept = 0, lty = 2) +
  xlim(-0.08, 1.08) + theme_light() + ylim(-4, 4)
ggsave("portfolio/figs/offset-bump.pdf", height = 10, width = 6)

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


# -----------------------------------------------
# breakpoint lmer()

bp <- 0
b1 <- function(x, bp) ifelse(x < bp, x, 0)
b2 <- function(x, bp) ifelse(x < bp, 0, x)
m <- lmer(log(revenue) ~ #days_change + 
  b1(spec_change, bp)*days_change + 
  b2(spec_change, bp)*days_change + 
  (-1 + b1(spec_change, bp) + b2(spec_change, bp) | strategy) +
  (1|strategy_year), 
  data = dat, offset = log(revenue.prev), REML = F)
summary(m)
m.b <- lmer(log(revenue) ~ 
  spec_change*days_change + 
  (-1 + b1(spec_change, bp) + b2(spec_change, bp) | strategy) +
  (1|strategy_year), 
  data = dat, offset = log(revenue.prev), REML = F)
MuMIn::r.squaredGLMM(m)
MuMIn::r.squaredGLMM(m.b)
AIC(m) - AIC(m.b)

m.c <- lmer(log(revenue) ~ #days_change +
  b1(spec_change, bp)*log_days +
  b2(spec_change, bp)*log_days +
  b1(spec_change, bp)*specdiv.prev +
  b2(spec_change, bp)*specdiv.prev +
  (-1 + b1(spec_change, bp) + b2(spec_change, bp) | strategy) +
  (1|strategy_year),
  data = dat, offset = log(revenue.prev), REML = F)
summary(m.c)

m.b <- lmer(log(revenue) ~
  spec_change*days_change +
  spec_change*specdiv.prev +
  (-1 + spec_change + spec_change:specdiv.prev | strategy) +
  (1|strategy_year),
  data = dat, offset = log(revenue.prev), REML = F)
summary(m.b)

m <- lmer(log(revenue) ~
  b1(spec_change, bp)*days_change + 
  b2(spec_change, bp)*days_change + 
  (-1 + b1(spec_change, bp) + b2(spec_change, bp) | strategy) +
  (1|strategy_year), 
  data = dat, offset = log(revenue.prev), REML = F)
m.b <- lmer(log(revenue) ~ days_change +
  b1(spec_change, bp) + 
  b2(spec_change, bp) + 
  (-1 + b1(spec_change, bp) + b2(spec_change, bp) | strategy) +
  (1|strategy_year), 
  data = dat, offset = log(revenue.prev), REML = F)
AIC(m) - AIC(m.b)

m <- lmer(log(revenue) ~
  b1(spec_change, bp)*days_change + 
  b2(spec_change, bp)*days_change + 
  (-1 + b1(spec_change, bp) + b2(spec_change, bp) | strategy) +
  (1|strategy_year), 
  data = dat, offset = log(revenue.prev), REML = T)

res <- data.frame(strategy = row.names(coef(m)$strategy))
res$dec <- coef(m)$strategy[,"b1(spec_change, bp)"]
res$inc <- coef(m)$strategy[,"b2(spec_change, bp)"]
res %>% 
  mutate(x1 = 0, x2 = 1) %>%
  ggplot(aes(x = x1, xend = x2, y = dec, yend = inc)) + 
  geom_segment(alpha = 0.5) +
  geom_text(aes(label = strategy, x = x1, y = dec)) +
  geom_text(aes(label = strategy, x = x2, y = inc)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  xlim(-0.08, 1.08) + theme_light() +
  scale_x_continuous(breaks = c(0, 1), labels = c("Decrease", "Increase"), limits = c(-0.25, 1.25)) +
  xlab("Species diversity") + ylab("Slope")
ggsave("portfolio/figs/offset-break-lmer-revenue-bump.pdf", width = 5, height = 10)
m.aug <- augment(m)

m.aug$spec_change <- dat$spec_change
m.aug <- mutate(m.aug, inc = ifelse(spec_change > 0, "inc", ifelse(spec_change == 0, "zero", "dec")))
p <- ggplot(filter(m.aug, .resid < 0), aes(spec_change, log(abs(.resid)), colour = inc, group = inc)) + 
  geom_point(alpha = 0.05) +
  facet_wrap(~strategy) + geom_smooth(se=F, colour = "black", method = "lm")
ggsave("portfolio/figs/offset-panel-down-only.pdf", width = 12, height = 12)
p <- ggplot(m.aug, aes(spec_change, log(abs(.resid)), colour = inc, group = inc)) + 
  geom_point(alpha = 0.05) +
  facet_wrap(~strategy) + geom_smooth(se=F, colour = "black", method = "lm")
ggsave("portfolio/figs/offset-panel.pdf", width = 12, height = 12)
p <- ggplot(m.aug, aes(spec_change, .resid, colour = inc, group = inc)) + 
  geom_point(alpha = 0.05) +
  facet_wrap(~strategy) + geom_smooth(se=F, colour = "black", method = "lm")
ggsave("portfolio/figs/offset-panel-mean-resid.pdf", width = 12, height = 12)

m2 <- lmer(log(abs(.resid)) ~ #days_change + 
  b1(spec_change, bp)*days_change + 
  b2(spec_change, bp)*days_change + 
  (b1(spec_change, bp) + b2(spec_change, bp) | strategy),
  data = m.aug, REML = F)
m2.b <- lmer(log(abs(.resid)) ~ days_change + 
  b1(spec_change, bp) + 
  b2(spec_change, bp) + 
  (b1(spec_change, bp) + b2(spec_change, bp) | strategy),
  data = m.aug, REML = F)
AIC(m2) - AIC(m2.b)
summary(m2)
MuMIn::r.squaredGLMM(m2)
m2 <- lmer(log(abs(.resid)) ~
  b1(spec_change, bp)*days_change + 
  b2(spec_change, bp)*days_change + 
  (b1(spec_change, bp) + b2(spec_change, bp) | strategy),
  data = m.aug, REML = T)

res2 <- data.frame(strategy = row.names(coef(m2)$strategy))
res2$dec <- coef(m2)$strategy[,"b1(spec_change, bp)"]
res2$inc <- coef(m2)$strategy[,"b2(spec_change, bp)"]
res2 %>% 
  mutate(x1 = 0, x2 = 1) %>%
  ggplot(aes(x = x1, xend = x2, y = dec, yend = inc)) + 
  geom_segment(alpha = 0.5) +
  geom_text(aes(label = strategy, x = x1, y = dec)) +
  geom_text(aes(label = strategy, x = x2, y = inc)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  xlim(-0.08, 1.08) + theme_light() +
  scale_x_continuous(breaks = c(0, 1), labels = c("Decrease", "Increase"), limits = c(-0.25, 1.25)) +
  xlab("Species diversity") + ylab("Slope")
ggsave("portfolio/figs/offset-break-lmer-variance-bump.pdf", width = 5, height = 10)

gg <- dat %>% group_by(strategy) %>% summarise(mean_div = mean(specDiv),
  median_div = median(specDiv))
res99 <- res2
res99$int <- coef(m2)$strategy[,1]
res99 <- inner_join(res99, gg)
ggplot(res99, aes(mean_div, int, color = grepl("S", strategy))) + geom_point() + geom_smooth(colour = "black", method = "lm")
ggplot(res99, aes(median_div, int, color = grepl("S", strategy))) + geom_point() + geom_smooth(colour = "black", method = "lm")
ggplot(dat, aes(specDiv)) + geom_histogram() + facet_wrap(~strategy, scales="free_y")

library(scales)
library(ggrepel)
res <- mutate(res, mean_rev_re = (dec + inc)/2)
res2$mean_rev_re <- NULL
res2$dec_rev <- NULL
res2$inc_rev <- NULL
res3 <- rename(res, inc_rev = inc, dec_rev = dec)
res2 <- inner_join(res2, res3)
j <- jitter(rep(0, nrow(res2)), amount = 0.05)
res2 %>% 
  mutate(x1 = j, x2 = j + 1, x1_clean = 0-0.1, x2_clean = 1+0.1) %>%
  mutate(strategy = as.character(strategy)) %>%
  mutate(strategy1 = ifelse(inc>0.9 | inc<0.2, strategy, NA)) %>%
  mutate(strategy0 = ifelse(dec> -0.3 | dec< -0.8, strategy, NA)) %>%
  ggplot(aes(x = x1, xend = x2, y = dec, yend = inc)) + 
  scale_color_gradient2(low = muted("red"), mid = "grey90", 
    high = muted("blue"), space="Lab") +
  geom_segment(alpha = 0.5, aes(colour = mean_rev_re)) +
  geom_text_repel(aes(label = strategy0, x = x1, y = dec), size = 2.5, colour = "grey30") +
  geom_text_repel(aes(label = strategy1, x = x2, y = inc), size = 2.5, colour = "grey30") +
  geom_point(aes(x = x1, y = dec, colour = dec_rev), size = 2) +
  geom_point(aes(x = x2, y = inc, colour = inc_rev), size = 2) +
  geom_hline(yintercept = 0, lty = 2, col = "grey50") +
  theme_light() +
  scale_x_continuous(breaks = c(0, 1), labels = c("Decrease", "Increase"), 
    limits = c(-0.4, 1.4)) +
  xlab("Species diversity") + ylab("Slope") +
  labs(colour = "Spp. diversity\nrevenue slope") +
  theme(legend.position = c(0.8, 0.2))
ggsave("portfolio/figs/offset-break-bump.pdf", width = 5, height = 9)

ggplot(res2, aes(inc, inc_rev)) + geom_point()
ggplot(res2, aes(dec, dec_rev)) + geom_point()
ggplot(res2, aes(inc_rev - dec_rev, y = 1)) + geom_point()

gg <- dat %>% group_by(strategy) %>% 
  summarise(mean_div = mean(specDiv), nn = length(unique(p_holder)))

library(viridis)

res2 %>% 
  mutate(strategy = as.character(strategy)) %>%
  inner_join(gg) %>%
  # ggplot(aes(mean_div, inc_rev)) + geom_point()
  mutate(strategy1 = 
    ifelse(inc>0.9 | inc< -2 | inc_rev > 0.4 | inc_rev < -0.6, strategy, NA)) %>%
  mutate(strategy0 = 
    ifelse(dec> 2 | dec< -0.8 | dec_rev > 0.4 | dec_rev < -0.6, strategy, NA)) %>%
  ggplot(aes(x = inc_rev, xend = dec_rev, y = inc, yend = dec)) + 
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_segment(alpha = 0.5, aes(colour = mean_div)) +
  geom_point(aes(x = inc_rev, y = inc, size = nn), pch = 19, colour = "grey40") +
  geom_point(aes(x = dec_rev, y = dec, size = nn), pch = 21, bg = "white",
    colour = "grey40") +
  geom_text(aes(label = strategy0, x = dec_rev, y = dec), nudge_y = -0.1, 
    size = 3, colour = "grey50") +
  geom_text(aes(label = strategy1, x = inc_rev, y = inc), nudge_y = 0.1, 
    size = 3, colour = "grey50") +
   scale_color_viridis() +
  theme_light() +
  xlab("Species diversity effect on revenue") + ylab("Species diversity effect on variability") +
  labs(colour = "Mean species\ndiversity", size = "Permit holders") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  xlim(-1.3, 0.8) +
  annotate("text", x = 0.1, y = 1.9, label = "Generalize, inc. $, inc. var.", 
    hjust = 0, size = 3) +
  annotate("text", x = 0.1, y = -1.3, label = "Specialize, dec. $, inc. var.", 
    hjust = 0, size = 3) +
  annotate("text", x = -0.1, y = 1.9, label = "Generalize, dec. $, inc. var.", 
    hjust = 1, size = 3) +
  annotate("text", x = -0.1, y = -1.3, label = "Specialize, inc. $, inc. var.", 
    hjust = 1, size = 3)
ggsave("portfolio/figs/offset-break-spaghetti.pdf", width = 7.5, height = 5)

gg <- dat %>% group_by(strategy) %>% 
  summarise(mean_div = mean(specDiv), nn = length(unique(p_holder)))
labels <- readr::read_csv("data/strategies-labels.csv")
res2$str_label <- NULL
res2 <- left_join(res2, select(labels, strategy, str_label))

look_str <- function(str) {
  x <- strsplit(str, " ")[[1]]
  x <- sapply(x, function(xx) labels$str_label[labels$strategy == xx])
  paste(x, collapse = ", ")
}
res2$str_label <- sapply(res2$strategy, function(x) look_str(x))
res2$str_label <- gsub(" - ", " ", res2$str_label)

library(gridExtra)
library(grid)
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

	plots <- list(...)
	position <- match.arg(position)
	g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
	legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
	lheight <- sum(legend$height)
	lwidth <- sum(legend$width)
	gl <- lapply(plots, function(x) x + theme(legend.position="none"))
	gl <- c(gl, ncol = ncol, nrow = nrow)

	combined <- switch(position,
		"bottom" = arrangeGrob(do.call(arrangeGrob, gl),
			legend,
			ncol = 1,
			heights = unit.c(unit(1, "npc") - lheight, lheight)),
		"right" = arrangeGrob(do.call(arrangeGrob, gl),
			legend,
			ncol = 2,
			widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
	# grid.newpage()
	grid.draw(combined)

}

p1 <- res2 %>% 
  mutate(strategy = as.character(strategy)) %>%
  inner_join(gg) %>%
  # mutate(strategy1 = ifelse(inc>0.9 | inc< -2, strategy, NA)) %>%
  # mutate(strategy0 = ifelse(dec> 2 | dec< -0.8, strategy, NA)) %>%
  mutate(strategy_label = ifelse(nn > 250, str_label, NA)) %>%
  mutate(inc = -inc) %>%
  ggplot(aes(x = inc_rev, y = inc)) + 
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_text_repel(aes(label = strategy_label, x = inc_rev, y = inc), nudge_y = 0.05, 
    size = 3, colour = "grey50") +
   scale_color_viridis() +
  geom_point(aes(color = mean_div, size = nn)) +
  theme_light() +
  xlab("Effect of generalizing on revenue") + 
  ylab("Effect of generalizing on variability") +
  labs(colour = "Mean sp.\ndiversity", size = "Number of\npermits") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

p2 <- res2 %>% 
  mutate(strategy = as.character(strategy)) %>%
  inner_join(gg) %>%
  mutate(dec = dec, dec_rev = -dec_rev) %>%
  mutate(strategy_label = ifelse(nn > 250, str_label, NA)) %>%
  mutate(strategy1 = ifelse(inc>0.9 | inc< -2, strategy, NA)) %>%
  mutate(strategy0 = ifelse(dec> 2 | dec< -0.8, strategy, NA)) %>%
  ggplot(aes(x = dec_rev, y = dec)) + 
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_text_repel(aes(label = strategy_label, x = dec_rev, y = dec), nudge_y = 0.05, 
    size = 3, colour = "grey50") +
   scale_color_viridis() +
  geom_point(aes(color = mean_div, size = nn)) +
  theme_light() +
  xlab("Effect of specializing on revenue") + 
  ylab("Effect of specializing on variability") +
  labs(colour = "Mean sp.\ndiversity", size = "Number of\npermits") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

pdf("portfolio/figs/offset-break-anti-spaghetti.pdf", width = 10, height = 4)
grid_arrange_shared_legend(p1, p2, ncol = 2, nrow = 1, position = "right")
dev.off()


p <- res2 %>% 
  mutate(strategy = as.character(strategy)) %>%
  inner_join(gg) %>%
  # ggplot(aes(mean_div, inc_rev)) + geom_point()
  mutate(strategy1 = 
    ifelse(inc>0.9 | inc< -2 | inc_rev > 0.4 | inc_rev < -0.6, strategy, NA)) %>%
  mutate(strategy0 = 
    ifelse(dec> 2 | dec< -0.8 | dec_rev > 0.4 | dec_rev < -0.6, strategy, NA)) %>%
  ggplot(aes(x = inc_rev, xend = dec_rev, y = inc, yend = dec)) + 
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_segment(alpha = 0.5, aes(colour = mean_div)) +
  geom_point(aes(x = inc_rev, y = inc, colour = nn), pch = 19, colour = "grey40") +
  geom_point(aes(x = dec_rev, y = dec, colour = nn), pch = 21, bg = "white",
    colour = "grey40") +
  geom_text(aes(label = strategy0, x = dec_rev, y = dec), nudge_y = -0.1, 
    size = 3, colour = "grey50") +
  geom_text(aes(label = strategy1, x = inc_rev, y = inc), nudge_y = 0.1, 
    size = 3, colour = "grey50") +
   scale_color_viridis() +
  theme_light() +
  xlab("Species diversity effect on revenue") + ylab("Species diversity effect on variability") +
  labs(colour = "Mean species\ndiversity", size = "Permit holders") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  xlim(-1.3, 0.8) +
  annotate("text", x = 0.1, y = 1.9, label = "Generalize, inc. $, inc. var.", 
    hjust = 0, size = 3) +
  annotate("text", x = 0.1, y = -1.3, label = "Specialize, dec. $, inc. var.", 
    hjust = 0, size = 3) +
  annotate("text", x = -0.1, y = 1.9, label = "Generalize, dec. $, inc. var.", 
    hjust = 1, size = 3) +
  annotate("text", x = -0.1, y = -1.3, label = "Specialize, inc. $, inc. var.", 
    hjust = 1, size = 3)
print(p)

ggsave("portfolio/figs/offset-break-spaghetti-no-n.pdf", width = 7.5, height = 5)

p <- res2 %>% 
  mutate(strategy = as.character(strategy)) %>%
  inner_join(gg) %>%
  mutate(strategy1 = 
    ifelse(inc>0.9 | inc< -2 | inc_rev > 0.4 | inc_rev < -0.6, strategy, NA)) %>%
  mutate(strategy0 = 
    ifelse(dec> 2 | dec< -0.8 | dec_rev > 0.4 | dec_rev < -0.6, strategy, NA)) %>%
  mutate(rev_avg = (inc_rev + dec_rev)/2) %>%
  mutate(dec = dec) %>%
  ggplot(aes(x = rev_avg, xend = rev_avg, y = inc, yend = dec)) + 
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_segment(alpha = 0.2) +
  geom_point(aes(x = rev_avg, y = inc), pch = 19, colour = "#FF6848") +
  geom_point(aes(x = rev_avg, y = dec), pch = 19, colour = "#4E94FF") +
  geom_text(aes(label = strategy0, x = rev_avg, y = dec), nudge_y = -0.1, 
    size = 3, colour = "grey50") +
  geom_text(aes(label = strategy1, x = rev_avg, y = inc), nudge_y = 0.1, 
    size = 3, colour = "grey50") +
   scale_color_viridis() +
  theme_light() +
  xlab("Species diversity effect on revenue") + 
  ylab("Species diversity effect on variability") +
  labs(colour = "Mean species\ndiversity", size = "Permit holders") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())# +
  # xlim(-1.3, 0.8) +
  # annotate("text", x = 0.1, y = 1.9, label = "Generalize, inc. $, inc. var.", 
  #   hjust = 0, size = 3) +
  # annotate("text", x = 0.1, y = -1.3, label = "Specialize, dec. $, inc. var.", 
  #   hjust = 0, size = 3) +
  # annotate("text", x = -0.1, y = 1.9, label = "Generalize, dec. $, inc. var.", 
  #   hjust = 1, size = 3) +
  # annotate("text", x = -0.1, y = -1.3, label = "Specialize, inc. $, inc. var.", 
  #   hjust = 1, size = 3)
print(p)

ggsave("portfolio/figs/offset-break-spaghetti-simplified.pdf", width = 6.5, height = 4.5)

p <- res2 %>% 
  mutate(strategy = as.character(strategy)) %>%
  inner_join(gg) %>%
  mutate(strategy1 = 
    ifelse(inc>0.9 | inc< -2 | inc_rev > 0.4 | inc_rev < -0.6, strategy, NA)) %>%
  mutate(strategy0 = 
    ifelse(dec> 2 | dec< -0.8 | dec_rev > 0.4 | dec_rev < -0.6, strategy, NA)) %>%
  mutate(rev_avg = (inc_rev + dec_rev)/2) %>%
  mutate(dec = dec) %>%
  ggplot(aes(x = inc_rev, xend = dec_rev, y = inc, yend = dec)) + 
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_segment(alpha = 0.2) +
  geom_point(aes(x = inc_rev, y = inc), pch = 19, colour = "#FF6848") +
  geom_point(aes(x = dec_rev, y = dec), pch = 19, colour = "#4E94FF") +
  geom_text(aes(label = strategy0, x = dec_rev, y = dec), nudge_y = -0.1, 
    size = 3, colour = "grey50") +
  geom_text(aes(label = strategy1, x = inc_rev, y = inc), nudge_y = 0.1, 
    size = 3, colour = "grey50") +
   scale_color_viridis() +
  theme_light() +
  xlab("Species diversity effect on revenue") + 
  ylab("Species diversity effect on variability") +
  labs(colour = "Mean species\ndiversity", size = "Permit holders") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())# +
  # xlim(-1.3, 0.8) +
  # annotate("text", x = 0.1, y = 1.9, label = "Generalize, inc. $, inc. var.", 
  #   hjust = 0, size = 3) +
  # annotate("text", x = 0.1, y = -1.3, label = "Specialize, dec. $, inc. var.", 
  #   hjust = 0, size = 3) +
  # annotate("text", x = -0.1, y = 1.9, label = "Generalize, dec. $, inc. var.", 
  #   hjust = 1, size = 3) +
  # annotate("text", x = -0.1, y = -1.3, label = "Specialize, inc. $, inc. var.", 
  #   hjust = 1, size = 3)
print(p)

ggsave("portfolio/figs/offset-break-spaghetti-simplified2.pdf", width = 6.5, height = 4.5)


