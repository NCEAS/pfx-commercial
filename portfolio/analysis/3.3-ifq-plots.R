## The following code implements the posterior density ratio plots for all the parameters
## I'm not currently using it

## library(tidyverse)
##
## load("portfolio/data-generated/diff-dat-stan.rda")
## load("portfolio/data-generated/m_ifq.rda")
## load("portfolio/data-generated/m.rda")
## devtools::load_all("pfxr")
##
## p <- stanhelpers::extract_df(m_ifq)
##
## get_ratio_posteriors <- function(effect_name, breaks = NULL, lab = "") {
##   effects_dat <- p[[effect_name]] %>%
##     tidyr::gather(strategy, posterior) %>%
##     mutate(strategy_ifq_id =
##              as.numeric(sub(paste0(effect_name, "_([0-9]+)"), "\\1", strategy))) %>%
##     select(-strategy) %>%
##     inner_join(md_ifq)
##
##   get_ifq_ratio <- function(strategy) {
##     before <- filter(effects_dat, strategy_ifq == strategy)
##     after <- filter(effects_dat, strategy_ifq == paste(strategy, "IFQ"))
##     ratio <- exp(after$posterior) / exp(before$posterior)
##     data.frame(strategy = strategy, ratio = ratio)
##   }
##
##   s <- unique(effects_dat$strategy_ifq)
##   strategies <- gsub(" IFQ", "", s[grepl("IFQ", s)])
##
##   out <- plyr::ldply(strategies, get_ifq_ratio)
##   out <- out %>% filter(ratio < 200) %>%
##     group_by(strategy) %>%
##     mutate(median_ratio = median(ratio)) %>%
##     ungroup() %>%
##     left_join(select(md, strategy, str_label))
##
##   labs <- unique(select(out, median_ratio, str_label)) %>%
##     mutate(str_label_ordered = factor(str_label,
##                                       levels = str_label[order(-median_ratio)])) %>%
##     select(-median_ratio)
##
##   out$str_label_ordered <- NULL
##   out <- inner_join(out, labs)
##
##   g <- ggplot(out, aes(str_label_ordered, ratio)) +
##     geom_hline(yintercept = 1, col = "grey65") +
##     geom_violin() +
##     scale_y_log10(breaks = breaks) +
##     xlab(lab) +
##     coord_flip() +
##     theme_light()
##
##     g
## }
##
## g0 <- get_ratio_posteriors("g0_strategy", breaks = c(0.6, 0.8, 1, 1.2, 1.4, 1.6),
##   lab = "After IFQ/Before IFQ variability")
## g2 <- get_ratio_posteriors("g2_strategy", breaks = sort(
##   c(2, 20, 5 * 10^(seq(-2, 2)), 10^(seq(-2, 2)))),
##   lab = "After IFQ/Before IFQ variability")
##
## gridExtra::grid.arrange(g0, g2)
##
## ggsave("portfolio/figs/itq-ratios.pdf", width = 4.5, height = 4)
##
