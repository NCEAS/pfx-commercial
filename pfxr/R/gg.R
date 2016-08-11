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
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = rel(0.7), colour = "grey30"),
    legend.title = element_text(size = rel(0.6)),
    legend.key = element_rect(colour = NA)
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

#' ggplot2-like colour scale in HCL space
#'
#' @param n Number of colours to return.
#' @param hue_min Minimum hue value in the range [0,360]
#' @param hue_max Maximum hue value in the range [0,360]
#' @param l Luminance in the range [0,100]
#' @param c Chroma of the colour.
#' @details See the \code{hcl} function for details.
#' @examples
#' gg_color_hue(10)
#'
gg_color_hue <- function(n, hue_min = 10, hue_max = 280, l = 62, c = 100) {
  hues = seq(hue_min, hue_max, length=n+1)
  hcl(h=hues, l=l, c=c)[1:n]
}
