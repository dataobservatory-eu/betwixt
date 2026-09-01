#' Add a candidate column to a Betwixt candidate dataset
#'
#' @description
#' Adds the next candidate column to a Betwixt candidate dataset. Candidate
#' columns are stored as consecutive triplets consisting of a candidate value,
#' an optional controlled range, and an optional semantic definition.
#'
#' The function determines the next available candidate-column number from
#' existing columns named `col_1`, `col_2`, and so on. It is intended to
#' support construction of candidate datasets by piping successive calls to
#' `add_candidate_column()`.
#'
#' @param x A data frame or tibble representing a Betwixt candidate dataset.
#'
#' @param value A vector containing the candidate values to be reviewed.
#'   Its length must be compatible with the number of rows in `x`.
#'
#' @param range A character vector containing the admissible or suggested
#'   range of candidate values. Use `NA_character_` when no controlled range
#'   is supplied. Defaults to `NA_character_`.
#'
#' @param definition A character vector containing identifiers or URLs
#'   defining the semantic property represented by the candidate column.
#'   Use `NA_character_` when no definition is supplied. Defaults to
#'   `NA_character_`.
#'
#' @return
#' A tibble with three additional columns: `col_n`, `col_n_range`, and
#' `col_n_definition`, where `n` is one greater than the highest existing
#' candidate-column number. If no candidate columns are present, `n` is `1`.
#'
#' @details
#' `add_candidate_column()` does not modify existing candidate columns.
#' The numbering of newly added columns is derived from column names matching
#' the pattern `col_<integer>`.
#'
#' Contextual, non-reviewable information does not need to be added with this
#' function and can be appended with ordinary data manipulation operations,
#' for example `dplyr::mutate(context_1 = held_by)`.
#'
#' @examples
#' candidates <- tibble::tibble(
#'   row_number = 1:2,
#'   evidence_text = c("image-1", "image-2")
#' )
#'
#' candidates <- add_candidate_column(
#'   candidates,
#'   value = c("farmhouse", "bed"),
#'   range = rep("farmhouse | bed | Other...", 2),
#'   definition = rep(
#'     "https://www.wikidata.org/wiki/Property:P31",
#'     2
#'   )
#' )
#'
#' names(candidates)
#'
#' @importFrom dplyr mutate
#'
#' @export
add_candidate_column <- function(
  x,
  value,
  range = NA_character_,
  definition = NA_character_
) {
  existing <- names(x)[grepl("^col_[0-9]+$", names(x))]

  n <- if (length(existing) == 0L) {
    1L
  } else {
    max(as.integer(sub("^col_", "", existing))) + 1L
  }

  x |>
    dplyr::mutate(
      !!paste0("col_", n) := value,
      !!paste0("col_", n, "_range") := range,
      !!paste0("col_", n, "_definition") := definition
    )
}
