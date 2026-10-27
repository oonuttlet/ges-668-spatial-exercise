library(plumber)
library(sf)
library(dplyr)
library(tigris)

# Load your data-generation functions
source("R/generate_data.R")

# Load the study area once when the API starts
study_area <- sf::st_read(
  "data/baltimore_city.gpkg",
  quiet = TRUE
)

#* Download generated points as CSV
#* @get /dataset/points.csv
#* @serializer contentType list(type="text/csv")
function(res, username, question = 1) {
  dat <- generate_data(
    seed = username,
    nquestion = as.integer(question),
    x = study_area,
    npoly = 25,
    npts = 25
  )

  # Convert point geometry to longitude/latitude columns
  coords <- sf::st_coordinates(
    sf::st_transform(dat$points, 4326)
  )

  points_csv <- sf::st_drop_geometry(dat$points) |>
    dplyr::mutate(
      longitude = coords[, 1],
      latitude = coords[, 2]
    )

  res$headers[["Content-Disposition"]] <-
    sprintf('attachment; filename="points_q%s.csv"', question)

  paste(
    capture.output(
      utils::write.csv(points_csv, row.names = FALSE)
    ),
    collapse = "\n"
  )
}

#* Download Voronoi polygons as GeoJSON
#* @get /dataset/voronoi.geojson
#* @serializer contentType list(type="application/geo+json")
function(res, username, question = 1) {
  dat <- generate_data(
    seed = username,
    nquestion = as.integer(question),
    x = study_area,
    npoly = 25,
    npts = 25
  )

  voronoi <- sf::st_transform(dat$voronoi, 4326)

  res$headers[["Content-Disposition"]] <-
    sprintf('attachment; filename="voronoi_q%s.geojson"', question)

  geojson <- sf::st_write(
    voronoi,
    dsn = tempfile(fileext = ".geojson"),
    driver = "GeoJSON",
    quiet = TRUE
  )

  # Read the serialized GeoJSON text
  readLines(geojson, warn = FALSE) |>
    paste(collapse = "\n")
}
