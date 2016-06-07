library(dplyr)
# library(ggplot2)
library(mgcv)
if (!exists("cfec")) {
  cfec <- feather::read_feather("data/cfec.feather")
}

standardize_smooth <- function(year, response) {
  response <- response + 1
  tryCatch({
  # m <- gam(response ~ s(year), family = Gamma(link = "log"))
  m <- glm(response ~ year, family = Gamma(link = "log"))
  p <- predict(m, type = "response")
  p
  }, error = function(e) rep(NA, length(response)))
}

cfec_holder <- cfec %>% 
  group_by(p_holder, year) %>% 
    summarize(total_revenue = sum(g_earn, na.rm = TRUE),
     species_permit = substr(p_fshy[1],1,1),
     species_permit_n = length(unique(substr(p_fshy,1,1))),
     area_permit = unique(region)[1],
     area_n = length(unique(region)))

cfec_holder <- filter(cfec_holder, species_permit_n == 1, area_n == 1) %>% 
  as.data.frame

cfec_holder <- mutate(cfec_holder, group = paste(area_permit, species_permit)) %>% 
  mutate(year_factor = as.factor(year))

cfec_holder <- 
  cfec_holder %>% group_by(species_permit, area_permit) %>% 
  mutate(smooth = standardize_smooth(year_factor, total_revenue )) %>% 
  as.data.frame

cfec_holder <- cfec_holder %>% arrange(p_holder, year)

p <- ggplot(filter(cfec_holder, group %in% unique(cfec_holder$group)[1:12]),
  aes(x = year, y = log(total_revenue +1), group = p_holder)) + 
  geom_line(alpha = 0.05) +
    geom_point(aes(y = log(smooth)), colour = "red", lwd = 1) +
    facet_wrap(area_permit~species_permit)
ggsave("figs/check-smooths.png", width = 8, height = 8)

