library(plumber)
library(sf)
library(dplyr)

cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 204
    return("")
  }

  plumber::forward()
}

#* @filter cors
function(req, res) {
  cors(req, res)
}

# Load your data-generation functions
source("R/generate_data.R")
source("R/questions.R")
source("R/check_answer.R")

# Load the study area once when the API starts
study_area <- sf::st_read(
  "data/baltimore_bounds.gpkg",
  quiet = TRUE
)

#* Download generated points as CSV
#* @serializer contentType list(type="text/csv")
#* @get /dataset/points
function(res, username = "", question = 1) {
  if (!nzchar(trimws(username))) {
    res$status <- 400
    return("Missing required query parameter: username")
  }

  question_num <- suppressWarnings(as.integer(question))

  if (is.na(question_num) || question_num < 1 || length(question_num) != 1) {
    res$status <- 400
    return("Invalid parameter: question must be a positive integer")
  }

  dat <- generate_data(
    username = username,
    nquestion = as.integer(question),
    x = study_area,
    npoly = 5,
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

  res$setHeader("Content-Type", "text/csv; charset=utf-8")
  res$setHeader(
    "Content-Disposition",
    sprintf('attachment; filename="%s_points_q%s.csv"', username, question_num)
  )

  paste(
    capture.output(write.csv(points_csv, row.names = FALSE)),
    collapse = "\n"
  )
}

#* Download Voronoi polygons as GeoJSON
#* @serializer contentType list(type="application/geo+json")
#* @get /dataset/voronoi
function(res, username = "", question = 1) {
  if (!nzchar(trimws(username))) {
    res$status <- 400
    return("Missing required query parameter: username")
  }

  question_num <- suppressWarnings(as.integer(question))

  if (is.na(question_num) || question_num < 1 || length(question_num) != 1) {
    res$status <- 400
    return("Invalid parameter: question must be a positive integer")
  }

  dat <- generate_data(
    username = username,
    nquestion = question_num,
    x = study_area,
    npoly = 5,
    npts = 25
  )

  voronoi <- sf::st_transform(dat$voronoi, 4326)

  res$setHeader(
    "Content-Disposition",
    sprintf('attachment; filename="%s_voronoi_q%s.geojson"', username, question_num)
  )

  geojson_path <- tempfile(fileext = ".geojson")

  on.exit(unlink(geojson_path), add = TRUE)

  sf::st_write(
    voronoi,
    dsn = geojson_path,
    driver = "GeoJSON",
    quiet = TRUE
  )

  paste(readLines(geojson_path, warn = FALSE), collapse = "\n")
}

#* Get a question
#* @param username Student username
#* @get /questions/<id>
function(id, username, res) {
  question_id <- suppressWarnings(as.integer(id))

  if (
    length(question_id) != 1 ||
      is.na(question_id) ||
      question_id < 1
  ) {
    res$status <- 400
    return(list(error = "Invalid question ID."))
  }

  if (missing(username) || !nzchar(trimws(username))) {
    res$status <- 400
    return(list(error = "Username is required."))
  }

  question <- get_question(trimws(username), question_id)

  if (is.null(question)) {
    res$status <- 404
    return(list(error = "Question not found."))
  }

  question
}

#* Check a student's answer
#* @param username Student username
#* @param answer Submitted answer
#* @post /check/<id>
function(id, username, answer, res) {
  question_id <- suppressWarnings(as.integer(id))

  if (
    length(question_id) != 1 ||
      is.na(question_id) ||
      is.null(question_id) ||
      question_id < 1
  ) {
    res$status <- 400
    return(list(error = "Invalid question ID."))
  }

  if (missing(username) || !nzchar(trimws(username))) {
    res$status <- 400
    return(list(error = "Username is required."))
  }

  if (missing(answer) || length(answer) != 1 || !nzchar(trimws(answer))) {
    res$status <- 400
    return(list(error = "Answer is required."))
  }

  username <- trimws(username)

  if (is.null(question_templates[[as.character(question_id)]])) {
    res$status <- 404
    return(list(error = "Question not found."))
  }

  # Regenerate the student's deterministic dataset
  dat <- generate_data(
    username = username,
    nquestion = id,
    x = study_area,
    npoly = 5,
    npts = 25
  )

  # Calculate and check the answer
  result <- check_answer(
    data = dat,
    answer = answer,
    username = username,
    question_id = question_id
  )

  result
}
