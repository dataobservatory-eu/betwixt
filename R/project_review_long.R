#' Project a Betwixt review to long form
#'
#' @description
#' Projects a Betwixt review into one row per atomic assertion in the candidate
#' and reviewed states.
#'
#' Descriptive fields are represented as assertions about the subject. Each
#' semantic candidate column is represented as a predicate, with its cell value
#' forming the value of the atomic assertion. Review status is inherited from
#' the aligned wide projection.
#'
#' The long projection is derived from [project_review_wide()] and introduces
#' no additional review semantics.
#'
#' Row-scoped `context_*` fields are inherited by each assertion generated from
#' the corresponding wide row. They provide descriptive context only: they do
#' not become assertions and do not receive review status.
#'
#' @param review A Betwixt review object returned by [read_review()].
#'
#' @return A data frame with one row per candidate or reviewed assertion and
#'   columns `assertion_number`, `row_number`, zero or more `context_*` columns,
#'   `plane`, `subject`, `predicate`, `value`, `status`, `reviewer`, and
#'   `generated_at`.
#'
#' @details
#' The long projection contains one row for each atomic assertion in both the
#' candidate and reviewed states. Consequently, its expected number of rows is
#'
#' \deqn{
#' n_{\\mathrm{long}} =
#' (n_{\\mathrm{candidate}} + n_{\\mathrm{reviewed}})
#' (n_{\\mathrm{descriptive}} + n_{\\mathrm{semantic}})
#' }
#'
#' when each wide row contains the same set of reviewable fields. The status
#' plane supplies the review status of these assertions but does not itself
#' generate additional long-form rows.
#'
#' @examples
#' review_html <- render_review(delini)
#'
#' path <- tempfile(fileext = ".html")
#' writeLines(review_html, path)
#'
#' review <- read_review(path)
#' long <- project_review_long(review)
#'
#' head(long)
#' @importFrom dplyr select left_join transmute arrange row_number bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
project_review_long <- function(review) {
  wide <- project_review_wide(review)

  candidate <- wide[wide$plane == "candidate", ]
  reviewed <- wide[wide$plane == "reviewed", ]
  status <- wide[wide$plane == "status", ]

  # Combine candidate and reviewed states.
  states <- dplyr::bind_rows(candidate, reviewed)

  # Identify descriptive and context columns.
  descriptive <- intersect(
    c(
      "label", "description", "alternative_label",
      "alternative_description"
    ),
    names(states)
  )
  context_cols <- grep("^context_", names(states), value = TRUE)

  # Project descriptive fields as atomic assertions.
  description_cols <- c(
    "row_number", context_cols, "plane", "subject", descriptive
  )
  descriptions <- states |>
    dplyr::select(dplyr::all_of(description_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(descriptive),
      names_to = "predicate", values_to = "value"
    )

  description_status <- status |>
    dplyr::select(dplyr::all_of(c("row_number", descriptive))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(descriptive),
      names_to = "predicate", values_to = "status"
    )

  descriptions <- dplyr::left_join(
    descriptions, description_status,
    by = c("row_number", "predicate")
  )

  # Identify semantic predicate columns.
  reserved <- grepl(
    paste0(
      "^(row_number|plane|evidence_|label$|description$|",
      "alternative_label$|alternative_description$|context_|",
      "subject$|subject_)"
    ),
    names(states)
  )
  predicate_cols <- names(states)[!reserved]

  # Project semantic fields as atomic assertions.
  semantic_cols <- c(
    "row_number", context_cols, "plane", "subject", predicate_cols
  )
  semantic <- states |>
    dplyr::select(dplyr::all_of(semantic_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(predicate_cols),
      names_to = "predicate", values_to = "value"
    )

  # Add review status to semantic assertions.
  semantic_status <- status |>
    dplyr::select(dplyr::all_of(c("row_number", predicate_cols))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(predicate_cols),
      names_to = "predicate", values_to = "status"
    )

  semantic <- dplyr::left_join(
    semantic, semantic_status,
    by = c("row_number", "predicate")
  )

  # Combine descriptive and semantic assertions.
  x <- dplyr::bind_rows(descriptions, semantic) |>
    dplyr::arrange(.data$row_number)

  # Add assertion identity and provenance.
  x <- x |>
    dplyr::mutate(
      assertion_number = dplyr::row_number(),
      reviewer = review$provenance$reviewer,
      reviewer_email = review$provenance$reviewer_email,
      reviewer_iri = review$provenance$reviewer_iri,
      started_at = review$provenance$started_at,
      saved_at = review$provenance$saved_at,
      ended_at = review$provenance$ended_at,
      data_manager = review$provenance$data_manager,
      data_manager_email = review$provenance$data_manager_email,
      data_manager_iri = review$provenance$data_manager_iri,
      candidate_generated_at = review$provenance$generated_at,
      software_agent = review$provenance$software_agent,
      software_version = review$provenance$software_version,
      project_id = review$provenance$project_id
    )

  # Set the canonical long-form column order.
  output_cols <- c(
    "assertion_number", "row_number", context_cols,
    "plane", "subject", "predicate", "value", "status",
    "reviewer", "reviewer_email", "reviewer_iri",
    "started_at", "saved_at", "ended_at",
    "data_manager", "data_manager_email", "data_manager_iri",
    "candidate_generated_at", "software_agent", "software_version",
    "project_id"
  )

  dplyr::select(x, dplyr::all_of(output_cols))
}
