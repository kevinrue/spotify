% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{spotify}
\alias{spotify}
\title{Spotify Images}
\usage{
spotify(
  path,
  return.type = c("raw", "flatten", "data", "matrix", "heatmap", "xy", "point", "jitter",
    "spatial", "visium"),
  downsample = 150,
  jitter = 1,
  point.size = 1,
  img2matrix.FUN = firstLayerNotWhite,
  invert = FALSE,
  extras = list()
)
}
\arguments{
\item{path}{Path to the input file.}

\item{return.type}{Type of image to return.}

\item{downsample}{Integer resolution or `FALSE`.}

\item{jitter}{Amount of jitter.}

\item{point.size}{Point size}

\item{img2matrix.FUN}{Function converting image data to a matrix.}

\item{invert}{Use negative of the image.}

\item{extras}{List of options for sub-functions.}
}
\value{
A `ggplot` object.
}
\description{
Main function.
}
\examples{
kevin <- spotify(
  path = system.file(package = "spotify", "Kevin.jpg")
)
print(kevin)

kevin <- spotify(
  path = system.file(package = "spotify", "Kevin.jpg"),
  return.type = "flatten",
  extras = list(
    image_flatten = list(operator = "Threshold")
  )
)
print(kevin)
}
