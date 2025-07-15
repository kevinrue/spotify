library(spotify)
library(ggplot2)

# find nearest neighbours in a dataframe of x and y coordinates
library(dbscan)

ggjitter <- spotify(
  path = "inst/spo",
  return.type = "jitter",
  jitter = 5,
  invert = FALSE,
  downsample = 75
)$data

ggplot(ggjitter) +
  geom_point(aes(x = x, y = y)) +
  theme_void()

# ggmatrix <- spotify(
#   path = "inst/spotify.jpg",
#   return.type = "matrix",
#   jitter = 7,
#   invert = TRUE,
#   downsample = 50
# )

library(spatstat)

# owin(mask = ggmatrix == TRUE)

chull_coords <- ggjitter[chull(ggjitter), ]

ggppp <- ppp(
  x = ggjitter$x,
  y = ggjitter$y,
  # windows = owin(xrange = c(1, ncol(ggmatrix)), yrange = c(-nrow(ggmatrix), 0), mask = ggmatrix == TRUE)
  window = owin(
    poly = list(
      x = rev(c(chull_coords$x, chull_coords$x[1])),
      y = rev(c(chull_coords$y, chull_coords$y[1]))
    )
  )
)

ggdirichlet <- dirichlet(ggppp)

plot(ggdirichlet)

area_tiles <- vapply(ggdirichlet$tiles, area.owin, numeric(1))
perimeter_tiles <- vapply(ggdirichlet$tiles, perimeter, numeric(1))
nvertices_tiles <- vapply(ggdirichlet$tiles, nvertices, numeric(1))

area_cutoff_max <- median(area_tiles) * 1.5
perimeter_cutoff_max <- median(perimeter_tiles) * 1.25
perimeter_cutoff_min <- quantile(perimeter_tiles, 0.01)
n_vertices_min <- 4
n_cell_types <- 4

ggplot() +
  geom_hline(yintercept = area_cutoff_max) +
  geom_point(aes(rank, area), data.frame(area = sort(area_tiles, decreasing = TRUE), rank = seq_along(area_tiles)))

ggplot() +
  geom_hline(yintercept = c(perimeter_cutoff_min, perimeter_cutoff_max)) +
  geom_point(aes(rank, perimeter), data.frame(perimeter = sort(perimeter_tiles, decreasing = TRUE), rank = seq_along(perimeter_tiles)))

ggplot() +
  geom_point(aes(area, perimeter), data.frame(area = area_tiles, perimeter = perimeter_tiles))

ggdirichlet <- ggdirichlet[area_tiles < area_cutoff_max & perimeter_tiles < perimeter_cutoff_max & perimeter_tiles > perimeter_cutoff_min & nvertices_tiles > n_vertices_min]

area_tiles <- vapply(ggdirichlet$tiles, area.owin, numeric(1))
perimeter_tiles <- vapply(ggdirichlet$tiles, perimeter, numeric(1))
nvertices_tiles <- vapply(ggdirichlet$tiles, nvertices, numeric(1))

type_tiles <- cut(
  area_tiles,
  breaks = seq(
    min(area_tiles) - 1,
    max(area_tiles) + 1,
    length.out = n_cell_types + 1
  )
)
# type_tiles <- cut(
#   perimeter_tiles,
#   breaks = seq(
#     min(perimeter_tiles) - 1,
#     max(perimeter_tiles) + 1,
#     length.out = n_cell_types + 1
#   )
# )
# type_tiles <- factor(nvertices_tiles)
levels(type_tiles) <- seq_along(levels(type_tiles))

table(type_tiles, useNA = "ifany")

library(RColorBrewer)

ggplot(ggjitter) +
  geom_point(aes(x = x, y = y)) +
  theme_void()

plot(ggdirichlet, do.col = TRUE, values = type_tiles, col = colourmap(brewer.pal(nlevels(type_tiles), "Pastel1"), inputs = levels(type_tiles)), show.all = FALSE)

center_tiles <- do.call("rbind", lapply(ggdirichlet$tiles, function(tile) {
  data.frame(x = mean(tile$x), y = mean(tile$y))
}))
center_tiles$tile <- 1:nrow(center_tiles)
center_tiles$type <- factor(NA, 1:n_cell_types)
center_tiles$type[sample(1:nrow(center_tiles), size = round(0.1 * nrow(center_tiles)))] <- 1
center_tiles$dist_to_1 <- NA
# center_tiles$nn <- nnwhich(X = center_tiles$x, Y = center_tiles$Y)
idx_to_process <- which(is.na(center_tiles$type))
distance_to_nearest_1 <- function(i) {
  message(i)
  tmp_data <- rbind(
    center_tiles[i, c("x", "y")],
    subset(center_tiles, type == 1, c("x", "y"))
  )
  nndist(tmp_data)[1]
}
center_tiles$dist_to_1[idx_to_process] <- vapply(idx_to_process, distance_to_nearest_1, numeric(1))
# center_tiles$nn_is_1 <- center_tiles$type[center_tiles$nn] == 1
assign_cell_type <- function(i) {
  message(i)
  sample(2:n_cell_types, prob = sqrt(center_tiles$dist_to_1[i]*(2:n_cell_types)), size = 1)
}
center_tiles$type[idx_to_process] <- vapply(idx_to_process, assign_cell_type, factor(1))
head(center_tiles)
table(center_tiles$type, useNA = "ifany")

plot(ggdirichlet, do.col = TRUE, values = center_tiles$type, col = colourmap(brewer.pal(nlevels(center_tiles$type), "Pastel1"), inputs = levels(center_tiles$type)), show.all = FALSE)
