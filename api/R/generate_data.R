library(sf)
library(dplyr)

generate_voronoi <- function(
  x,
  npoly
) {
  dat <- st_sample(x, npoly) |>
    st_union() |>
    st_voronoi() |>
    st_collection_extract("POLYGON") |>
    st_as_sf() |>
    st_intersection(x) |>
    st_as_sf() |>
    mutate(id = row_number())

  dat
}

generate_points <- function(
  x,
  npts
) {
  dat <- st_sample(x, npts) |>
    st_as_sf() |>
    mutate(
      category = sample(LETTERS[1:5], npts, replace = TRUE),
      value = round(runif(npts) * 10, 2)
    )

  dat
}

generate_data <- function(username, nquestion, x, npoly, npts) {
  seed_value <- digest::digest2int(
    paste0(username, "_", nquestion)
  )

  withr::with_seed(seed_value, {
    points <- generate_points(x, npts)
    voronoi <- generate_voronoi(x, npoly)

    list(
      points = points,
      voronoi = voronoi
    )
  })
}
