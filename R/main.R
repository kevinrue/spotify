#' Spatialise Images
#' 
#' Main function.
#'
#' @param path Path to the input file.
#'
#' @return A `ggplot` object.
#' @export
#' @importFrom magick image_read
#'
#' @examples
#' spatialise(system.file(package = "spatialist", "Kevin.jpg"))
spatialise <- function(path) {
  img_raw <- image_read(path = path)
  img_raw
}
