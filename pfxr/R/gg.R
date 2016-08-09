theme_gg <- function(base_size = 11, base_family = "") {
  theme_light() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text.x = element_text(colour = "grey30"),
    axis.text = element_text(colour = "grey30"),
    axis.title = element_text(colour = "grey30"),
    legend.title = element_text(colour = "grey30"),
    panel.border = element_rect(fill = NA, colour = "grey70", size = 1),
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_text(size = rel(0.7))
  )
}

# from ggplot wiki
grid_arrange_shared_legend <- function(..., ncol = length(list(...)),
  nrow = 1, position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
    "bottom" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
      legend,
      ncol = 1,
      heights =  grid::unit.c(grid::unit(1, "npc") - lheight, lheight)),
    "right" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
      legend,
      ncol = 2,
      widths =  grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)))
  grid::grid.draw(combined)
}
