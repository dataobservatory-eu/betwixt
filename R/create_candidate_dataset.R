#' Construct a Betwixt candidate dataset
#'
#' @description
#' Constructs the initial wide-form candidate dataset used for a Betwixt
#' review. Each row associates a piece of evidence with a subject and the
#' descriptive information required to present that subject for review.
#'
#' The subject forms the first reviewable candidate column. Additional
#' reviewable columns can subsequently be appended with
#' [add_candidate_column()].
#'
#' An optional evidence relation can describe a reviewable semantic relation
#' between the evidence and the subject, for example `"depicts"` or
#' `"documents"`.
#'
#' @param subject A vector containing the subject identifiers or values to be
#'   reviewed. The subject is stored as the subject column named `subject`.
#'
#' @param evidence_media_url An optional character vector containing URLs or
#'   other resolvable locations of media presented directly to the reviewer.
#'
#' @param evidence_url An optional character vector containing URLs or other
#'   resolvable locations of evidence resources that can be opened by the
#'   reviewer.
#'
#' @param evidence_text An optional character vector containing short identifiers or
#'   labels for the evidence.
#'
#' @param label An optional character vector containing human-readable labels for the
#'   subjects presented for review.
#'
#' @param description An optional character vector containing human-readable
#'   descriptions of the subjects presented for review.
#'
#' @param alternative_label An optional character vector containing alternative
#'   human-readable labels for the subjects presented for review, for example
#'   a translation or a label intended for a different user group.
#'
#' @param alternative_description An optional character vector containing
#'   alternative human-readable descriptions of the subjects presented for
#'   review, for example a translation, a more detailed description, or a
#'   description intended for a different user group.
#'
#' @param evidence_relation An optional character vector describing a candidate
#'   semantic relation between the evidence and the subject, for example
#'   `"depicts"`. If `NULL`, no evidence relation columns are added. Defaults
#'   to `NULL`.
#'
#' @param evidence_relation_range An optional character vector containing the
#'   admissible or suggested evidence relations. Use [add_candidate_range()] to
#'   construct controlled ranges. This argument can only be used when
#'   `evidence_relation` is supplied. Defaults to `NA_character_`.
#'
#' @param subject_range A character vector containing the admissible or
#'   suggested subject values. Use [add_candidate_range()] to construct controlled
#'   ranges, or `NA_character_` when no controlled range is supplied. Defaults
#'   to `NA_character_`.
#'
#' @param subject_definition A character vector containing resolvable
#'   identifiers or URLs defining the proposed subject. A supplied definition
#'   indicates that the subject already exists as an identified entity;
#'   `NA_character_` indicates an unresolved candidate subject. Defaults to
#'   `NA_character_`.
#'
#' @return
#' A tibble with one row per evidence-subject observation and the columns
#' `row_number`, `evidence_url`, `evidence_media_url`, `evidence_text`, `label`,
#' `description`, `alternative_label`, `alternative_description`, `subject`,
#' `subject_range`, and `subject_definition`.
#'
#' If `evidence_relation` is supplied, the tibble additionally contains
#' `evidence_relation` and `evidence_relation_range`.
#'
#' @details
#' `create_candidate_dataset()` establishes the initial structure of a Betwixt
#' candidate dataset. `row_number` is generated automatically as an integer
#' sequence in input order.
#'
#' The subject, its candidate range, and its definition are passed internally
#' to [add_candidate_column()], thereby establishing the same column contract
#' used for subsequent reviewable assertions.
#'
#' Additional reviewable assertions can be appended with
#' [add_candidate_column()]. Display-only contextual columns can be added with
#' ordinary data manipulation functions such as [dplyr::mutate()].
#'
#' At least one of `evidence_media_url` or `evidence_url` must be supplied for
#' each row.
#'
#' The evidence relation is distinct from review provenance. When present, it
#' represents a candidate semantic relation between the evidence and the
#' subject and is itself available for review.
#'
#' @examples
#' # Candidate dataset without a reviewable evidence relation
#' delini_candidates <- create_candidate_dataset(
#'   evidence_media_url = delini$evidence_media_url,
#'   evidence_text = delini$evidence_text,
#'   label = delini$label,
#'   description = delini$description,
#'   subject = delini$subject,
#'   subject_range = delini$subject_range,
#'   subject_definition = delini$subject_definition
#' )
#'
#' delini_candidates
#'
#' # Candidate dataset with a reviewable evidence relation
#' delini_dual_candidates <- create_candidate_dataset(
#'   evidence_media_url = delini$evidence_media_url,
#'   evidence_text = delini$evidence_text,
#'   label = delini$label,
#'   description = delini$description,
#'   subject = delini$subject,
#'   subject_definition = delini$subject_definition,
#'   evidence_relation = rep("depicts", nrow(delini)),
#'   evidence_relation_range = rep(
#'     add_candidate_range("depicts", "documents", "Other…"),
#'     nrow(delini)
#'   )
#' )
#'
#' delini_dual_candidates
#'
#' # Statistical example based on the W3C RDF Data Cube Vocabulary
#' w3c_candidates <- create_candidate_dataset(
#'   evidence_media_url = rep(
#'     "https://www.w3.org/TR/vocab-data-cube/",
#'     nrow(w3c_life_expectancy)
#'   ),
#'   evidence_text = rep(
#'     "W3C RDF Data Cube Vocabulary",
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
#'
#' @importFrom tibble tibble
#' @export
create_candidate_dataset <- function(
  subject,
  evidence_url = NA_character_,
  evidence_media_url = NA_character_,
  evidence_text = NA_character_,
  label = NA_character_,
  description = NA_character_,
  alternative_label = NA_character_,
  alternative_description = NA_character_,
  evidence_relation = NULL,
  evidence_relation_range = NA_character_,
  subject_range = NA_character_,
  subject_definition = NA_character_
) {
  if (is.null(evidence_relation) &&
    !all(is.na(evidence_relation_range))) {
    stop(
      "evidence_relation_range requires evidence_relation.",
      call. = FALSE
    )
  }

  if (is.null(evidence_relation)) {
    x <- tibble::tibble(
      row_number = seq_along(subject),
      evidence_url = evidence_url,
      evidence_media_url = evidence_media_url,
      evidence_text = evidence_text,
      label = label,
      description = description,
      alternative_label = alternative_label,
      alternative_description = alternative_description,
      subject = subject,
      subject_range = subject_range,
      subject_definition = subject_definition
    )
  } else {
    x <- tibble::tibble(
      row_number = seq_along(subject),
      evidence_url = evidence_url,
      evidence_media_url = evidence_media_url,
      evidence_text = evidence_text,
      evidence_relation = evidence_relation,
      evidence_relation_range = evidence_relation_range,
      label = label,
      description = description,
      alternative_label = alternative_label,
      alternative_description = alternative_description,
      subject = subject,
      subject_range = subject_range,
      subject_definition = subject_definition
    )
  }

  x
}
