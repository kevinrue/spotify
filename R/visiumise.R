#' @importFrom Biobase matchpt
make_visium <- function(xy, resolution) {
  x_gap <- round((max(xy$x) - min(xy$x)) / resolution)
  y_gap <- round((max(xy$y) - min(xy$y)) / resolution)
  xy_gap <- min(x_gap, y_gap)
  x_grid <- round(seq(min(xy$x), max(xy$x), by = xy_gap))
  y_grid <- round(seq(min(xy$y), max(xy$y), by = xy_gap))
  visium_xy <- expand.grid(x_grid, y_grid)
  colnames(visium_xy) <- c("x", "y")
  visium_xy$x <- visium_xy$x + ifelse(visium_xy$y %in% y_grid[c(TRUE, FALSE)], xy_gap / 2, 0)
  visium_xy$nearest_index <- matchpt(as.matrix(visium_xy), as.matrix(xy[, c("x", "y")]))$index
  visium_xy$cluster <- xy$cluster[visium_xy$nearest_index]
  visium_xy$distance_to_nearest <- distance(visium_xy, xy[visium_xy$nearest_index, ])
  visium_xy <- subset(visium_xy, distance_to_nearest < xy_gap)
  ggplot(visium_xy, aes(x, y, colour = cluster)) +
    geom_point() +
    coord_fixed()
}

distance <- function(X, Y) {
  tmp <- data.frame(
    visium.x = X$x,
    visium.y = X$y,
    img.x = Y$x,
    img.y = Y$y
  )
  with(tmp, sqrt((visium.x - img.x)^2 + (visium.y - img.y)^2))
}
