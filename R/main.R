#' Spatialise Images
#' 
#' Main function.
#'
#' @param path Path to the input file.
#' @param return.type Type of image to return.
#' @param extras List of options for sub-functions.
#'
#' @return A `ggplot` object.
#' @export
#' @importFrom magick image_read image_flatten image_data
#'
#' @examples
#' kevin <- spatialise(
#'   path = system.file(package = "spatialist", "Kevin.jpg")
#' )
#' print(kevin)
#' 
#' kevin <- spatialise(
#'   path = system.file(package = "spatialist", "Kevin.jpg"),
#'   return.type = "flatten",
#'   extras = list(
#'     image_flatten = list(operator = "Threshold")
#'   )
#' )
#' print(kevin)
spatialise <- function(path, return.type = c("raw", "flatten"), extras = list()) {
  return.type <- match.arg(return.type)
  
  if (! "image_flatten" %in% names(extras)) {
    extras$image_flatten <- list(operator = "Modulate")
  }
  
  img_raw <- image_read(path = path)
  
  if (return.type == "raw") {
    return(img_raw)
  }
  
  img_flat <- image_flatten(img_raw, extras$image_flatten$operator)
  
  if (return.type == "flatten") {
    return(img_flat)
  }
  
  img_data <- image_data(img_flat)
  return(img_data)
  
  # data
  non_white <- img_data[1, ,] != "ff"
  storage.mode(non_white) <- "integer"
  img_raw
}
