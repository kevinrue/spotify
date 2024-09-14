#' Convert Image to Matrix
#'
#' @param img_data magick image object.
#'
#' @return Integer matrix.
#' @export
#'
#' @examples
#' library(magick)
#' 
#' img <- system.file(package = "spatialist", "Kevin.jpg")
#' img_raw <- image_read(path = img)
#' img_data <- image_data(img_raw)
#' is.matrix(firstLayerNotWhite(img_data))
firstLayerNotWhite <- function(img_data) {
  img_matrix <- img_data[1, ,] != "ff"
  storage.mode(img_matrix) <- "integer"
  img_matrix
}
