library(dplyr)
m <- readRDS("portfolio/data-generated/stan-july31.rds")
dat <- readRDS("portfolio/data-generated/modeled-dat.rds")
mm <- model.matrix(revenue ~ 
  I(scaled_spec_div^2) + I(log_days_permit^2) + I(log_length^2) +
  (scaled_spec_div + log_days_permit + log_length)^2, data = dat)[,-1]
# mm <- mm/3
mm2 <- model.matrix(revenue ~ (scaled_spec_div + log_days_permit + log_length)^2, 
  data = dat)[,-1]

# -----------------------------------------------
# posterior beanplots

# devtools::install_github("seananderson/stanhelpers")
p <- stanhelpers::extract_df(m)

names(p$b_j) <- colnames(mm)
names(p$g_k) <- colnames(mm2)

md <- select(as.data.frame(dat), strategy, strategy_mean_div) %>% unique()
dd <- read.table("portfolio/figs/stan-strategy-ids.txt",
  stringsAsFactors = F)
names(dd) <- c("strategy_id", "strategy")
md <- inner_join(md, dd) %>% arrange(strategy_id)

p$b0_strategy <- mutate_all(p$b0_strategy, function(x) p$b0[[1]] + x)
p$b1_strategy <- mutate_all(p$b1_strategy, function(x) p$b_j$scaled_spec_div + x)
p$g1_strategy <- mutate_all(p$g1_strategy, function(x) p$g_k$scaled_spec_div + x)
for (i in seq_len(nrow(md))) {
  p$g0_strategy[,i] <- 
    p$g0_strategy[,i] + p$g0[[1]] + p$h1[[1]] * md[i,"strategy_mean_div"]
}

d1 <- summarise_all(p$b0_strategy, median) %>% t %>% as.data.frame() %>%
  rename(b0 = V1) 
d2 <- summarise_all(p$b1_strategy, median) %>% t %>% as.data.frame() %>%
  rename(b1 = V1)
d3 <- summarise_all(p$g0_strategy, median) %>% t %>% as.data.frame() %>%
  rename(g0 = V1)
d4 <- summarise_all(p$g1_strategy, median) %>% t %>% as.data.frame() %>%
  rename(g1 = V1)
d5 <- cbind(d1, d2, d3, d4)

names(p$b0_strategy) <- dd$strategy
names(p$b1_strategy) <- dd$strategy
names(p$g1_strategy) <- dd$strategy
names(p$g0_strategy) <- dd$strategy

p1 <- tidyr::gather(p$b0_strategy, term, posterior) %>%
  ggplot(aes(term, posterior)) + geom_violin() +
    coord_flip() + ggtitle("b0")
p2 <- tidyr::gather(p$g0_strategy, term, posterior) %>%
  ggplot(aes(term, posterior)) + geom_violin() +
    coord_flip() + ggtitle("g0")
p3 <- tidyr::gather(p$b1_strategy, term, posterior) %>%
  ggplot(aes(term, posterior)) + geom_violin() +
    coord_flip() + geom_hline(yintercept = 0, lty = 2) + ggtitle("b1")
p4 <- tidyr::gather(p$g1_strategy, term, posterior) %>%
  ggplot(aes(term, posterior)) + geom_violin() +
    coord_flip() + geom_hline(yintercept = 0, lty = 2) + ggtitle("g1")
pdf("portfolio/figs/stan-violins-re.pdf", width = 12, height = 9)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 4)
dev.off()

# -----------------------------------------------
# spoke plots:
b <- broom::tidy(m, conf.int = F, estimate.method = "median", rhat = F, ess = F)
md2 <- filter(b, grepl("g0_strategy", term)) %>% mutate(strategy_id = 1:n()) %>% inner_join(md)
h1 <- filter(b, term == "h1")
md2$estimate <- md2$estimate + md2$strategy_mean_div * h1$estimate

md3 <- filter(b, grepl("b0_strategy", term)) %>% 
  mutate(strategy_id = 1:n()) %>%
  inner_join(md) %>% rename(b0 = estimate) %>% 
  select(-term, -std.error, -strategy_mean_div) %>%
  inner_join(md2) %>% select(-std.error, -term) %>%
  rename(g0 = estimate)

ggplot(md3, aes(g0, b0, label = strategy, colour = strategy_mean_div)) + 
  geom_text()

permit_plot <- function(permit) {
  gre1 <- paste0(permit, " ")
  gre2 <- paste0(permit, "$")
  d_permit <- filter(md3, grepl(gre1, strategy) | grepl(gre2, strategy))
  single <- filter(md3, strategy == permit) %>% 
    rename(b0_single = b0, g0_single = g0) %>% 
      select(-strategy_id, -strategy_mean_div, -strategy)
  d_permit$b0_single <- single$b0_single
  d_permit$g0_single <- single$g0_single
  ggplot(d_permit, 
    aes(exp(b0), exp(g0), yend = exp(g0_single), xend = exp(b0_single), 
    label = strategy, colour = strategy_mean_div)) + 
    geom_text() +
    geom_segment() + theme(legend.position="none")
}
p1 <- permit_plot("G01")
p2 <- permit_plot("G34")
p3 <- permit_plot("K91")
p4 <- permit_plot("T91Q")
p5 <- permit_plot("B61B")
p6 <- permit_plot("C61B")
p7 <- permit_plot("S01")
p8 <- permit_plot("S03")
pdf("portfolio/figs/stan-gg-spoke.pdf", width = 12, height = 12)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8)
dev.off()

# -----------------------------------------------
#
## b <- broom::tidy(m, conf.int = T, estimate.method = "median", conf.level = 0.95)
## b2 <- broom::tidy(m, conf.int = T, estimate.method = "median", conf.level = 0.5) %>%
##   rename(conf.low.50 = conf.low, con.high.50 = conf.high) %>%
##   select(-estimate, -std.error)
## b <- inner_join(b, b2) %>%
##   filter(grepl("b1_strategy"))
## ggplot(b, aes(est))
b1 <- tidyr::gather(p$b1_strategy, strategy, posterior) %>%
  group_by(strategy) %>%
  summarise(
    b1.l = quantile(posterior, probs = 0.025),
    b1.l.5 = quantile(posterior, probs = 0.25),
    b1.m = quantile(posterior, probs = 0.5),
    b1.u.5 = quantile(posterior, probs = 0.75),
    b1.u = quantile(posterior, probs = 0.975))

g1 <- tidyr::gather(p$g1_strategy, strategy, posterior) %>%
  group_by(strategy) %>%
  summarise(
    g1.l = quantile(posterior, probs = 0.025),
    g1.l.5 = quantile(posterior, probs = 0.25),
    g1.m = quantile(posterior, probs = 0.5),
    g1.u.5 = quantile(posterior, probs = 0.75),
    g1.u = quantile(posterior, probs = 0.975))

b0 <- tidyr::gather(p$b0_strategy, strategy, posterior) %>%
  group_by(strategy) %>%
  summarise(b0.m = quantile(posterior, probs = 0.5))

slopes <- inner_join(b1, g1) %>% inner_join(b0)
slopes$highlight <- FALSE
slopes$highlight[slopes$strategy == "S01A"] <- TRUE

ggplot(slopes, aes(b1.m, g1.m)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  # geom_segment(aes(x = b1.l, y = g1.m, xend = b1.u, yend = g1.m), alpha = 0.1) +
  # geom_segment(aes(x = b1.m, y = g1.l, xend = b1.m, yend = g1.u), alpha = 0.1) +
  geom_segment(aes(x = b1.l.5, y = g1.m, xend = b1.u.5, yend = g1.m), alpha = 0.2) +
  geom_segment(aes(x = b1.m, y = g1.l.5, xend = b1.m, yend = g1.u.5), alpha = 0.2) +
  geom_point(aes(size = b0.m, colour = highlight), alpha = 0.2) +
  scale_colour_manual(values = c("black","red")) +
  geom_text(aes(label = strategy), check_overlap = F) +
  xlab("Within-strategy effect of diversity on revenue") +
  ylab("Within-strategy effect of diversity on variability of revenue")

unique(dat$strategy)
# q <- filter(dat, strategy == "S01A") %>%
q <- filter(dat, strategy == "M7IB") %>%
  group_by(p_holder) %>%
  summarise(m = mean(log(revenue)), v = sd(revenue)/mean(revenue),
    m_div = mean(scaled_spec_div),
    m_days = mean(log_days_permit))
# ggplot(q, aes(m, v, colour = m_div)) + geom_point()
ggplot(q, aes(m_div, v, colour = m_days)) + geom_point() + stat_smooth(method = "lm")
# ggplot(q, aes(m_div, m, colour = v)) + geom_point()
