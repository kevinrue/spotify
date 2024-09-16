#' Title
#'
#' @param xy Data frame.
#' @param extras List of additional parameters. See Details.
#'
#' @return Factor.
#' @export
#' @importFrom dbscan kNNdist
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(1000),
#'   y = rnorm(1000)
#' )
#' cluster(df)
cluster <- function(xy, extras = list()) {
  k.nn <- ifelse(is.null(extras$k.nn), 10, extras$k.nn)
  k.cluster <- ifelse(is.null(extras$k.cluster), 10, extras$k.cluster)
  knn.dist <- kNNdist(as.matrix(xy[, c("x", "y")]), k = k.nn)
  dist.breaks <- unique(quantile(knn.dist, probs = seq(0, 1, length.out = k.cluster+1)))
  dist.breaks[1] <- dist.breaks[1]-1
  dist.breaks[length(dist.breaks)] <- dist.breaks[length(dist.breaks)]+1
  dist.cluster <- cut(knn.dist, dist.breaks)
  levels(dist.cluster) <- seq_along(levels(dist.cluster))
  dist.cluster
}
