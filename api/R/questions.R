question_templates <- list(
  `1` = list(
    id = 1,
    title = "Calculate a Weighted Spatial Mean"
  ),
  `2` = list(
    id = 2,
    title = "Calculate a Weighted Spatial Mean (again)"
  ),
  `3` = list(
    id = 3,
    title = "Calculate a Weighted Spatial Mean (again!!)"
  )
)

generate_weights <- function(username, question) {
  seed <- digest::digest2int(
    paste(username, question, "weights", sep = ":")
  )

  withr::with_seed(seed, {
    setNames(
      round(runif(5, min = 0.5, max = 10), 1),
      LETTERS[1:5]
    )
  })
}

generate_distance <- function(username, question) {
  seed <- digest::digest2int(
    paste(username, question, "distance", sep = ":")
  )

  withr::with_seed(seed, {
    round(runif(1, min = 50, max = 1250), -1)
  })
}

generate_polygon_id <- function(username, question) {
  seed <- digest::digest2int(
    paste(username, question, "polygon", sep = ":")
  )

  withr::with_seed(seed, {
    sample(1:5, 1)
  })
}

get_question <- function(username, question_id) {
  question <- question_templates[[as.character(question_id)]]

  if (is.null(question)) {
    return(NULL)
  }

  weights <- generate_weights(username, question_id)
  question$weights <- as.list(weights)

  distance <- generate_distance(username, question_id)
  question$distance <- distance

  polygon_id <- generate_polygon_id(username, question_id)
  question$polygon_id <- polygon_id

  question$text <- sprintf(
    paste0(
      "<p>Multiply each point value by the weighting factor ",
      "associated with its category.</p>",
      "<p>For each polygon, calculate the mean weighted value ",
      "of all points located within %s feet of that polygon.</p>",
      "<p>What is the mean weighted value for Polygon ID %s?</p>"
    ),
    question$distance,
    question$polygon_id
  )

  question
}
