library(dplyr)

source("portfolio/analysis/cull-dat.R")
dat <- cullDat(rev_threshold = 10000, npholders_thres1 = 50,
  npholders_thres2 = 100)

# #downsample for fast testing
# unique_holders <- unique(dat$p_holder)
# n_sample <- round(length(unique_holders)*0.4)
# set.seed(1)
# dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
# nrow(dat)

# dat <- dat %>% group_by(strategy) %>% mutate(n=n()) %>% filter(n > 0)
# nrow(dat)
# length(unique(dat$strategy))

dat <- mutate(dat,
  revenue_change = log(revenue/revenue.prev),
  days_change = log((days_permit+1)/(days_permit.prev+1)),
  spec_change = log(specDiv/specdiv.prev))
dat$strategy_year <- paste(dat$strategy, dat$year, sep = ":")
dat$strategy_id <- as.numeric(as.factor(as.character(dat$strategy)))
dat$str_yr_id <- as.numeric(as.factor(as.character(dat$strategy_year)))
dat <- dat %>% group_by(strategy) %>%
  mutate(strategy_mean_div = mean(specDiv),
    strategy_mean_days = mean(days_permit)) %>%
  as_data_frame

b1 <- function(x, bp = 0) ifelse(x < bp, x, 0)
b2 <- function(x, bp = 0) ifelse(x < bp, 0, x)

# NOTE: first and second coefficients must be species diversity slopes:
mm <- model.matrix(log(revenue) ~ -1 + b1(spec_change) + b2(spec_change) +
    days_change + b1(spec_change):days_change + b2(spec_change):days_change,
  data = dat)
mm2 <- mm

md <- select(dat,
  strategy_id, strategy_mean_div, strategy_mean_days, strategy) %>%
  unique %>%
  arrange(strategy_id)

labels <- readr::read_csv("data/strategies-labels.csv")
md$str_label <- NULL
md <- left_join(md, select(labels, strategy, str_label))

look_str <- function(str) {
  x <- strsplit(str, " ")[[1]]
  x <- sapply(x, function(xx) labels$str_label[labels$strategy == xx])
  paste(x, collapse = " + ")
}

md$str_label <- sapply(md$strategy, function(x) look_str(x))
md$str_label <- gsub(" - ", " ", md$str_label)

scale_2d = function(x) {
  x = x - mean(x)
  x / (2 * sd(x))
}
md <- mutate(md, scaled_strategy_mean_div = scale_2d(strategy_mean_div),
  scaled_strategy_mean_days = scale_2d(strategy_mean_days))

save(dat, mm, mm2, md, file = "portfolio/data-generated/diff-dat-stan.rda")

# library(ggplot2)
# ggplot(dat, aes(year, log(revenue))) + geom_point(alpha = 0.1) + facet_wrap(~strategy)

#ggplot(dat, aes(year, specDiv)) + geom_point(alpha = 0.1) + facet_wrap(~strategy)
#ggplot(dat, aes(year, spec_change)) + geom_point(alpha = 0.1) + facet_wrap(~strategy)

# filter(dat, strategy == "T09", spec_change != 0)
# cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
# filter(cfec, p_holder == 17410, year ==1993) %>% select(p_fshy, spec, g_earn, landdate) %>% arrange(landdate) %>% mutate(g_earn = round(g_earn/1e3,1))

# filter(dat, strategy == "B05B", spec_change != 0)
# filter(cfec, p_holder == 17410, year ==1993) %>% select(p_fshy, spec, g_earn, landdate) %>% arrange(landdate) %>% mutate(g_earn = round(g_earn/1e3,1))
#
# group_by(dat, strategy) %>% summarize(n0 = round(sum(spec_change == 0)/n()*100, 2)) %>% arrange(-n0) %>% as.data.frame()

