#' @import ggplot2
#' @import viridis
#' @import dplyr

get_contour <- function(df, x_variable, y_variable, prob = 0.8, n = 200, ...) {
  x <- dplyr::select_(df, x_variable, y_variable) %>%
    data.matrix()  %>%
      coda::mcmc() %>%
        emdbook::HPDregionplot(prob = prob, n = n, ...)

  for (j in length(x)) x[[j]]$contour_group <- j

  x <- x %>% lapply(as.data.frame) %>%
    bind_rows()

  x <- mutate(x, contour_group =
    ifelse(!is.na(contour_group), contour_group, 1))

  x[,c("x", "y", "contour_group")]
}

plot_polygons <- function(polygon_data, x_column, y_column,
  xlab = "Variance", ylab = "Mean", prob = 0.8,
  grouping = c("diversity_group"), ...) {
  contours <- plyr::ddply(polygon_data, grouping,
    function(x) get_contour(x, x_column, y_column, prob = prob))
# We need unique IDs in case there are multiple polygons for one group
  contours$id <- paste(contours$contour_group, contours[,grouping[1]])
  polygon_data <- mutate(polygon_data, id = 1)

  p <- contours %>%
    ggplot(aes_string("x", "y", group = "id", color = grouping[1],
      fill = grouping[1]))

  p <- p + geom_point(data = polygon_data, aes_string(x_column, y_column),
    alpha = 0.1)
  p <- p + geom_polygon(alpha = 0.3) +
    scale_fill_viridis(discrete = TRUE) + scale_color_viridis(discrete = TRUE) +
    theme_bw() +
    xlab(xlab) + ylab(ylab)

  if (length(grouping) > 1)
    p <- p + facet_wrap(grouping[2])

  print(p)
  invisible(contours)
}
