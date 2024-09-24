#' Scatterplot
#'
#' @param xy_coord `data.frame` of XY coordinates.
#'
#' @return [`ggplot`] object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point coord_fixed
#'
#' @examples
#' make_point(data.frame(x = 1, y = 1))
make_point <- function(xy_coord) {
  ggplot(xy_coord, aes(x, y)) +
    geom_point() +
    coord_fixed()
}

#' Jitterplot
#'
#' @param xy_coord `data.frame` of XY coordinates.
#' @param jitter Amount of jitter.
#'
#' @return [`ggplot`] object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_jitter coord_fixed
#'
#' @examples
#' make_jitter(data.frame(x = 1, y = 1))
make_jitter <- function(xy_coord, jitter) {
  ggplot(xy_coord, aes(x, y)) +
    geom_jitter(width = jitter, height = jitter) +
    coord_fixed()
}

#' Spatial Plot
#'
#' @param xy_coord `data.frame` of XY coordinates.
#' @param point.size Point size
#'
#' @return [`ggplot`] object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point coord_fixed
#'
#' @examples
#' make_jitter(data.frame(x = 1, y = 1))
make_spatial <- function(xy_coord, point.size) {
  ggplot(xy_coord, aes(x, y, colour = cluster)) +
    geom_point(size = point.size) +
    coord_fixed()
}
