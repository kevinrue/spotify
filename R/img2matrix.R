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
#' img <- system.file(package = "spatialist", "Kevin.jpeg")
#' img_raw <- image_read(path = img)
#' img_data <- image_data(img_raw)
#' is.matrix(ebimage_thresh(img_data))
ebimage_thresh <- function(img_raw) {
  # ebimage_data <- readImage("inst/Kevin.jpeg")
  ebimage_data <- as_EBImage(img_raw)
  # ebimage_data <- Image(aperm(img_data, c(2,3,1)))
  # ebimage_data <- as.Image(img_data)
  # ebimage_data <- rgbImage(red, green, blue)
  gray <- channel(ebimage_data, "gray")
  bw <- thresh(gray, 10, 10, 0.05)  # adaptive threshold
  seg <- bwlabel(bw)
  # display(colorLabels(seg), method = "raster")
  img_matrix <- seg@.Data > 0
  storage.mode(img_matrix) <- "integer"
  img_matrix
}
