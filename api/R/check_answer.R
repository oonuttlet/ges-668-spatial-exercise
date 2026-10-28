library(sf)
library(dplyr)

check_answer <- function(data, answer, username, question_id) {
  question_key <- as.character(question_id)
  question <- question_templates[[question_key]]

  if (is.null(question)) {
    stop("Unknown question.")
  }

  weights <- generate_weights(username, question_id)
  distance <- generate_distance(username, question_id)
  polygon_id <- generate_polygon_id(username, question_id)

  pts <- data$points |>
    st_transform(2893)

  poly_buffer <- data$voronoi |>
    st_transform(2893) |>
    rename(polygon_id = id) |>
    st_buffer(distance)

  pts$weighted_value <- pts$value *
    weights[as.character(pts$category)]

  pts_summ <- pts |>
    st_join(poly_buffer, left = FALSE) |>
    st_drop_geometry() |>
    group_by(polygon_id) |>
    summarize(
      mean_wgt_val = mean(weighted_value, na.rm = TRUE),
      .groups = "drop"
    )

  print(paste("Target polygon ID:", polygon_id))
  print(paste("Available polygon IDs:", paste(pts_summ$polygon_id, collapse = ", ")))

  answer_val <- pts_summ |>
    filter(polygon_id == !!polygon_id) |>
    pull(mean_wgt_val)

  print(answer_val)

  if (length(answer_val) != 1 || !is.finite(answer_val)) {
    return(list(correct = FALSE))
  }

  submitted_answer <- suppressWarnings(as.numeric(answer))

  if (length(submitted_answer) != 1 ||
    is.na(submitted_answer) ||
    !is.finite(submitted_answer)) {
    return(list(correct = FALSE))
  }

  print(
    abs(round(submitted_answer, 2) - round(answer_val, 2))
  )
  print(isTRUE(
    abs(round(submitted_answer, 2) - round(answer_val, 2)) <= 0.1
  ))

  list(
    correct = isTRUE(
      abs(round(submitted_answer, 2) - round(answer_val, 2)) <= 0.1
    )
  )
}
