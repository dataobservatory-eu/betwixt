#' Construct a Betwixt review dataset
#'
#' @description
#' Constructs the initial wide-form data structure used for a Betwixt review.
#' Each row associates a piece of evidence with a subject and the descriptive
#' information required to present that subject for review.
#'
#' The subject forms the first reviewable column. Additional reviewable
#' columns can subsequently be appended with [add_candidate_column()].
#'
#' @param evidence_url A character vector containing URLs or other resolvable
#'   locations of the evidence presented to the reviewer.
#'
#' @param evidence_text A character vector containing short identifiers or
#'   labels for the evidence.
#'
#' @param evidence_relation A character vector describing the candidate semantic
#'   relation between the evidence and the subject, for example `"depicts"`.
#'
#' @param evidence_relation_range A character vector containing the admissible or
#'   suggested relations between the evidence and the subject. Use
#'   `NA_character_` when no controlled range is supplied. Defaults to
#'   `NA_character_`.
#'
#' @param label A character vector containing human-readable labels for the
#'   subjects presented for review.
#'
#' @param description A character vector containing human-readable
#'   descriptions of the subjects presented for review.
#'
#' @param subject A vector containing the subject identifiers or values to be
#'   reviewed. The subject is stored as the first review column, `col_1`.
#'
#' @param subject_range A character vector containing the admissible or
#'   suggested range of subject values. Use `NA_character_` when no controlled
#'   range is supplied. Defaults to `NA_character_`.
#'
#' @param subject_definition A character vector containing identifiers or URLs
#'   defining the subject field. Use `NA_character_` when no definition is
#'   supplied. Defaults to `NA_character_`.
#'
#' @return
#' A tibble with one row per evidence-subject observation and the columns
#' `row_number`, `evidence_url`, `evidence_text`, `relation`,
#' `relation_range`, `label`, `description`, `col_1`, `col_1_range`,
#' and `col_1_definition`.
#'
#' @details
#' `candidate_dataset()` establishes the initial structure of a Betwixt review
#' dataset. `row_number` is generated automatically as an integer sequence in
#' input order.
#'
#' The subject and its associated range and definition are passed internally
#' to [add_candidate_column()], thereby establishing the same column contract
#' used for subsequent reviewable columns.
#'
#' Additional reviewable assertions can be appended with
#' [add_candidate_column()]. Display-only contextual columns can be added with
#' ordinary data manipulation functions such as [dplyr::mutate()].
#'
#' @examples
#' # Cultural heritage example
#' delini_candidates <- candidate_dataset(
#'   evidence_url = delini$evidence_url,
#'   evidence_text = delini$evidence_text,
#'   evidence_relation = rep("depicts", nrow(delini)),
#'   evidence_relation_range = rep(
#'     "depicts | documents | is evidence for | Other...",
#'     nrow(delini)
#'   ),
#'   label = delini$label,
#'   description = delini$description,
#'   subject = delini$col_1,
#'   subject_range = delini$col_1_range,
#'   subject_definition = delini$col_1_definition
#' )
#'
#' delini_candidates
#'
#' # Statistical example based on the W3C RDF Data Cube Vocabulary
#' w3c_candidates <- candidate_dataset(
#'   evidence_url = rep(
#'     "https://www.w3.org/TR/vocab-data-cube/",
#'     nrow(w3c_life_expectancy)
#'   ),
#'   evidence_text = rep(
#'     "W3C RDF Data Cube Vocabulary",
#'     nrow(w3c_life_expectancy)
#'   ),
#'   evidence_relation = rep(
#'     "documents",
#'     nrow(w3c_life_expectancy)
#'   ),
#'   label = w3c_life_expectancy$observation,
#'   description = paste(
#'     "Life expectancy observation for",
#'     w3c_life_expectancy$area
#'   ),
#'   subject = w3c_life_expectancy$observation
#' )
#'
#' w3c_candidates <- w3c_candidates |>
#'   add_candidate_column(
#'     value = w3c_life_expectancy$area
#'   ) |>
#'   add_candidate_column(
#'     value = w3c_life_expectancy$period
#'   ) |>
#'   add_candidate_column(
#'     value = w3c_life_expectancy$sex
#'   ) |>
#'   add_candidate_column(
#'     value = w3c_life_expectancy$life_expectancy
#'   )
#'
#' w3c_candidates
#' @importFrom tibble tibble
#'
#' @export
candidate_dataset <- function(
    evidence_url,
    evidence_text,
    evidence_relation,
    evidence_relation_range = NA_character_,
    label,
    description,
    subject,
    subject_range = NA_character_,
    subject_definition = NA_character_
) {

  x <- tibble::tibble(
    row_number = seq_along(evidence_url),
    evidence_url = evidence_url,
    evidence_text = evidence_text,
    evidence_relation = evidence_relation,
    evidence_relation_range = evidence_relation_range,
    label = label,
    description = description
  )

  add_candidate_column(
    x,
    value = subject,
    range = subject_range,
    definition = subject_definition
  )
}
