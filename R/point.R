#' Scatterplot
#'
#' @param xy_coord `data.frame` of XY coordinates.
#'
#' @return [`ggplot`] object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point coord_fixed
#'
#' @examples
#' make_point(data.frame(row = 1, column = 1))
make_point <- function(xy_coord) {
  ggplot(xy_coord, aes(x, y)) +
    geom_point() +
    coord_fixed()
}