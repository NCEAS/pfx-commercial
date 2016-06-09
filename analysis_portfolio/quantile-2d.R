# Extracted and adapted from the metafolio package
#' Get quantile contour
#'
#' @param x Output from \code{\link[MASS]{kde2d}}.
#' @param alpha The quantile level.
get_quantile_contour <- function(x, alpha = 0.8) {
  zdens <- rev(sort(x$z))
  Czdens <- cumsum(zdens)
  Czdens <- (Czdens/Czdens[length(zdens)])
  crit.val <- zdens[max(which(Czdens<=alpha))]
  b.full=contourLines(x,levels=crit.val)
  list(x = b.full[[1]]$x, y = b.full[[1]]$y)
}

#' Custom bandwidth
#'
#' Based on \code{bandwidth.nrd} from \pkg{MASS}. This version takes the
#' absolute value of \code{var} to avoid errors.
#'
#' @param x A numeric vector
custom_bw <- function(x) {
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2] - r[1])/1.34
    4 * 1.06 * min(sqrt(abs(var(x))), h) * length(x)^(-1/5)
}

#' Add a kernel density polygon
#'
#' @param x x values
#' @param y y values
#' @param col Colour to add polygon with. Will be made into two levels of
#'   opacity.
#' @param lwd lwd Line width
#' @param alpha A numeric vector of length 2 that gives the confidence levels
#'   for the two kernel density polygons.
#' @param add_poly Add polygons?
#' @param add_pts Logical: should points be added?
add_dens_polygon <- function(x, y, col = "#000000", lwd = 1.7,
  alpha = c(0.25, 0.75), add_pts = FALSE, add_poly = TRUE,
  plot = TRUE) {
  if(add_poly) {
    x_bw <- custom_bw(na.omit(x))
    y_bw <- custom_bw(na.omit(y))
    k <- get_quantile_contour(MASS::kde2d(x,y, h = c(x_bw, y_bw)), alpha = alpha[1])
    if (plot)
      polygon(k$x, k$y, border = col, col = paste(col, "40", sep = ""), lwd = lwd)
    k <- get_quantile_contour(MASS::kde2d(x,y, h = c(x_bw, y_bw)), alpha = alpha[2])
    if (plot)
      polygon(k$x, k$y, border = col, col = paste(col, "80", sep = ""), lwd = lwd)
  }
  if(add_pts) points(x, y, pch = 21, col = paste(col, "60", sep = ""), cex = 0.7)
  invisible(data.frame(x = k$x, y = k$y))
}
