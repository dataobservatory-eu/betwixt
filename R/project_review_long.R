#' Project a Betwixt review to long form
#'
#' Projects a Betwixt review into one row per reviewed atomic assertion.
#'
#' Descriptive fields are represented as assertions about the reviewed subject.
#' Semantic `subject`, `predicate`, and `value` fields are combined into a
#' single atomic assertion. Review status is inherited from the aligned wide
#' projection.
#'
#' The long projection is derived from [project_review_wide()] and introduces
#' no additional review semantics.
#'
#' @param review A Betwixt review object returned by [read_review()].
#'
#' @return A data frame with one row per reviewed assertion and columns
#'   `assertion_number`, `row_number`, `subject`, `predicate`, `value`,
#'   `status`, `reviewer`, and `generated_at`.
#'
#' @importFrom dplyr select left_join transmute arrange row_number bind_rows
#' @importFrom tidyr pivot_longer
#' @export
project_review_long <- function(review) {
  wide <- project_review_wide(review)

  candidate <- wide[wide$plane == "candidate", ]
  reviewed <- wide[wide$plane == "reviewed", ]
  status <- wide[wide$plane == "status", ]

  descriptive <- intersect(
    c(
      "label", "description", "alternative_label",
      "alternative_description"
    ),
    names(reviewed)
  )

  # Project descriptive fields as atomic assertions.
  descriptions <- reviewed |>
    dplyr::select(row_number, subject, dplyr::all_of(descriptive)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(descriptive),
      names_to = "predicate",
      values_to = "value"
    )

  description_status <- status |>
    dplyr::select(row_number, dplyr::all_of(descriptive)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(descriptive),
      names_to = "predicate",
      values_to = "status"
    )

  descriptions <- dplyr::left_join(
    descriptions,
    description_status,
    by = c("row_number", "predicate")
  )

  # Combine the semantic triplet into one atomic assertion.
  semantic <- reviewed |>
    dplyr::transmute(
      row_number,
      subject,
      predicate,
      value,
      status = status$value
    )

  x <- dplyr::bind_rows(descriptions, semantic) |>
    dplyr::arrange(row_number)

  x |>
    dplyr::mutate(
      assertion_number = dplyr::row_number(),
      reviewer = review$provenance$reviewer,
      generated_at = review$provenance$ended_at
    ) |>
    dplyr::select(
      assertion_number, row_number, subject, predicate, value,
      status, reviewer, generated_at
    )
}
