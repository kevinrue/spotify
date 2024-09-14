#' Display Heatmap of Binary Matrix
#'
#' @param img_matrix Matrix
#'
#' @return [`Heatmap`]
#' @export
#' @importFrom ComplexHeatmap Heatmap draw
#'
#' @examples
#' make_heatmap(matrix(sample(0:1, size = 100, replace = TRUE), nrow = 10))
make_heatmap <- function(img_matrix) {
  if (is.null(img_matrix)) {
    return(plot.new())
  }
  hm <- Heatmap(
    matrix = t(img_matrix),
    col = c("1" = "black", "0" = "white"),
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_heatmap_legend = FALSE
  )
  draw(hm)
}
