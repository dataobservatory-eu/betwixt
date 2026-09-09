#' Project a Betwixt review to aligned wide planes
#'
#' Projects a Betwixt review into candidate, reviewed, and status planes while
#' preserving the wide structure of the original review.
#'
#' The candidate plane contains the assertions presented for review. The
#' reviewed plane contains the values persisted by the reviewer. The status
#' plane describes the review state of each reviewable field.
#'
#' Semantic assertions may carry explicit `defer` or `reject` qualifications.
#' These take precedence over states inferred from candidate and reviewed
#' values. Descriptive fields currently use inferred review states only.
#'
#' For a finalised row, an unchanged value is `corroborated`, a changed
#' non-empty value is `corrected`, deletion of a candidate value is `rejected`,
#' and an absent descriptive assertion that remains absent is `deferred`.
#' Values in rows that have not been finalised are `pending`, unless an
#' explicit semantic qualification applies.
#'
#' @param review A Betwixt review object returned by [read_review()].
#'
#' @return A data frame containing three structurally aligned wide planes,
#'   identified by the `plane` column as `candidate`, `reviewed`, or `status`.
#'
#' @details
#' Reviewable fields include descriptive fields such as `label`, `description`,
#' `alternative_label`, and `alternative_description`, when present, together
#' with semantic assertion fields represented by `_qualification` columns.
#'
#' An empty reviewed descriptive value is interpreted as rejection only when
#' the corresponding candidate value was non-empty and the row was finalised.
#' If both candidate and reviewed descriptive values are missing in a finalised
#' row, the assertion is interpreted as deferred rather than rejected.
#'
#' @export
project_review_wide <- function(review) {
  candidate <- review$candidate
  reviewed <- review$reviewed

  # Find descriptive and explicitly qualified semantic assertions.
  descriptive <- intersect(
    c(
      "label", "description", "alternative_label",
      "alternative_description"
    ),
    names(candidate)
  )
  q_cols <- grep("_qualification$", names(reviewed), value = TRUE)
  semantic <- sub("_qualification$", "", q_cols)
  cols <- unique(c(descriptive, semantic))

  # Derive the review state of each assertion.
  status <- candidate
  status[cols] <- lapply(cols, function(x) {
    q_col <- paste0(x, "_qualification")
    q <- if (q_col %in% names(reviewed)) reviewed[[q_col]] else "none"

    missing <- is.na(candidate[[x]]) & is.na(reviewed[[x]])
    deleted <- !is.na(candidate[[x]]) & nzchar(candidate[[x]]) &
      !is.na(reviewed[[x]]) & !nzchar(reviewed[[x]])
    changed <- candidate[[x]] != reviewed[[x]]

    dplyr::case_when(
      q == "defer" ~ "deferred",
      q == "reject" ~ "rejected",
      !reviewed$finalised ~ "pending",
      x %in% descriptive & missing ~ "deferred",
      deleted ~ "rejected",
      changed ~ "corrected",
      .default = "corroborated"
    )
  })

  # Keep the three planes structurally identical.
  keep <- names(candidate)
  candidate <- candidate[keep]
  reviewed <- reviewed[keep]
  status <- status[keep]

  dplyr::bind_rows(
    candidate |> dplyr::mutate(plane = "candidate"),
    reviewed |> dplyr::mutate(plane = "reviewed"),
    status |> dplyr::mutate(plane = "status")
  ) |>
    dplyr::relocate(plane, .after = row_number)
}
