#' Spatialise Images
#' 
#' Main function.
#'
#' @param path Path to the input file.
#' @param return.type Type of image to return.
#' @param downsample Integer resolution or `FALSE`.
#' @param jitter Amount of jitter.
#' @param point.size Point size
#' @param img2matrix.FUN Function converting image data to a matrix.
#' @param invert Use negative of the image.
#' @param extras List of options for sub-functions.
#'
#' @return A `ggplot` object.
#' @export
#' @importFrom magick image_read image_flatten image_data
#' @importFrom iSEE subsetPointsByGrid
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
spatialise <- function(
  path,
  return.type = c("raw", "flatten", "data", "matrix", "heatmap", "xy", "point", "jitter", "spatial", "visium"),
  downsample = 150,
  jitter = 1,
  point.size = 1,
  img2matrix.FUN = firstLayerNotWhite,
  invert = FALSE,
  extras = list()
) {
  return.type <- match.arg(return.type)
  
  if (! "image_flatten" %in% names(extras)) {
    extras$image_flatten <- list(operator = "Modulate")
  }
  
  img_raw <- image_read(path = path)
  
  if (return.type == "raw") {
    return(img_raw)
  }
  
  if (identical(img2matrix.FUN, ebimage_thresh)) {
    img_matrix <- ebimage_thresh(img_raw)
  } else {
    img_flat <- image_flatten(img_raw, extras$image_flatten$operator)
    
    if (return.type == "flatten") {
      return(img_flat)
    }
    
    img_data <- image_data(img_flat)
    
    if (return.type == "data") {
      return(img_data)
    }
    
    img_matrix <- img2matrix.FUN(img_data)
  }
  
  if (invert) {
    img_matrix <- 1 - img_matrix
  }
  
  if (return.type == "matrix") {
    return(img_matrix)
  }
  
  if (return.type == "heatmap") {
    return(make_heatmap(img_matrix))
  }
  
  xy_coord <- as.data.frame(which(img_matrix == 1, arr.ind = TRUE))
  xy_coord <- data.frame(
    x = xy_coord$row,
    y = -xy_coord$col
  )
  
  if (!isFALSE(downsample)) {
    keep <- subsetPointsByGrid(
      X = xy_coord$x,
      Y = xy_coord$y,
      resolution = downsample
    )
    xy_coord <- xy_coord[keep, ]
  }
  
  if (return.type == "point") {
    return(make_point(xy_coord))
  }
  
  if (return.type == "xy") {
    return(xy_coord)
  }
  
  xy_coord$x <- xy_coord$x + runif(nrow(xy_coord), min = -jitter, max = jitter)
  xy_coord$y <- xy_coord$y + runif(nrow(xy_coord), min = -jitter, max = jitter)
  
  if (return.type == "jitter") {
    return(make_point(xy_coord))
  }
  
  xy_coord$cluster <- cluster(xy_coord, extras$cluster)
  
  if (return.type == "spatial") {
    return(make_spatial(xy_coord, point.size))
  }
  
  if (return.type == "visium") {
    return(make_visium(xy_coord, downsample, point.size))
  }
  
}
