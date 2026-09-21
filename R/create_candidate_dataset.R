#' Construct a Betwixt candidate dataset
#'
#' @description
#' Constructs an initial wide-form candidate dataset for a Betwixt review.
#' Each row associates an input with a subject and provides the information
#' required to present candidate semantic assertions for review.
#'
#' The subject forms the first reviewable candidate column. Additional
#' reviewable columns can subsequently be appended with
#' [add_candidate_column()].
#'
#' An optional input predicate can express a candidate semantic relation
#' between the input and the subject, for example `"depicts"` or
#' `"documents"`. When supplied, this relation is itself reviewable.
#'
#' @param subject A vector containing the subject identifiers or values to be
#'   reviewed. The subject is stored in the `subject` column.
#'
#' @param input_url An optional character vector containing URLs or other
#'   resolvable locations of input resources that can be opened by the
#'   reviewer.
#'
#' @param input_media_url An optional character vector containing URLs or
#'   other resolvable locations of media that can be presented directly to
#'   the reviewer.
#'
#' @param input_label An optional character vector containing short
#'   human-readable labels identifying the inputs.
#'
#' @param input_description An optional character vector containing
#'   human-readable descriptions of the inputs.
#'
#' @param label An optional character vector containing human-readable labels
#'   for the subjects presented for review.
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
#' @param input_predicate An optional character vector expressing a candidate
#'   semantic relation from the input to the subject, for example `"depicts"`
#'   or `"documents"`. If `NULL`, no input-predicate candidate columns are
#'   added. Defaults to `NULL`.
#'
#' @param input_predicate_range An optional character vector containing the
#'   admissible or suggested values for `input_predicate`. Use
#'   [add_candidate_range()] to construct controlled ranges. This argument can
#'   only be used when `input_predicate` is supplied. Defaults to
#'   `NA_character_`.
#'
#' @param subject_range A character vector containing the admissible or
#'   suggested subject values. Use [add_candidate_range()] to construct
#'   controlled ranges, or `NA_character_` when no controlled range is
#'   supplied. Defaults to `NA_character_`.
#'
#' @param subject_definition A character vector containing resolvable
#'   identifiers or URLs defining the proposed subject. A supplied definition
#'   indicates that the subject already exists as an identified entity;
#'   `NA_character_` indicates an unresolved candidate subject. Defaults to
#'   `NA_character_`.
#'
#' @param data_manager_name Character string containing the name of the person
#'   responsible for preparing the candidate dataset.
#'
#' @param data_manager_iri Character string containing an IRI identifying the
#'   data manager, such as an ORCID, ISNI, or Wikidata URI.
#'
#' @param data_manager_email Character string containing the data manager's
#'   email address.
#'
#' @param table_id Character string identifying the project within which the
#'   candidate dataset was generated.
#'
#' @return
#' A tibble representing the initial Betwixt candidate dataset, with one row
#' per input-subject observation. The table contains structural coordinates,
#' input information, subject presentation information, and the candidate
#' subject assertion.
#'
#' The core columns are `row_number`, `input_url`, `input_media_url`,
#' `input_label`, `input_description`, `label`, `description`,
#' `alternative_label`, `alternative_description`, `subject`,
#' `subject_range`, and `subject_definition`.
#'
#' If `input_predicate` is supplied, the tibble additionally contains
#' `input_predicate` and `input_predicate_range`.
#'
#' @details
#' `create_candidate_dataset()` establishes the initial structure of a Betwixt
#' candidate dataset. `row_number` is generated automatically as an integer
#' sequence in input order.
#'
#' Input columns describe or identify the serialised artefact supplied to the
#' review process. They are distinct from the subject and from the semantic
#' assertions proposed about that subject. An input may be a document, image,
#' dataset, web resource, or another serialised artefact from which candidate
#' assertions have been generated.
#'
#' `input_label` and `input_description` are separate semantic fields.
#' Renderers may choose to present them together, but their distinction is
#' preserved in the candidate dataset.
#'
#' When `input_predicate` is supplied, it expresses a candidate relation
#' directed from the input to the subject:
#'
#' ```
#' input -- input_predicate --> subject
#' ```
#'
#' The input predicate is therefore distinct from review provenance. It is a
#' candidate domain assertion and can itself be accepted, rejected, deferred,
#' or otherwise reviewed.
#'
#' The subject, its candidate range, and its definition follow the same
#' candidate-column contract used by [add_candidate_column()] for subsequent
#' reviewable assertions.
#'
#' Additional reviewable assertions can be appended with
#' [add_candidate_column()]. Display-only contextual information can be added
#' as `context_*` columns using ordinary data manipulation functions such as
#' [dplyr::mutate()].
#'
#' Dataset-level provenance is stored in the `provenance` attribute rather
#' than repeated across observation rows. The generation timestamp is
#' recorded as an ISO 8601 UTC value at one-second precision. The software
#' agent and installed Betwixt package version are recorded automatically.
#'
#' At least one of `input_media_url` or `input_url` must be supplied for each
#' row.
#'
#' @examples
#' # Candidate dataset without a reviewable input predicate
#' delini_candidates <- create_candidate_dataset(
#'   input_media_url = delini$input_media_url,
#'   input_label = delini$input_label,
#'   input_description = delini$input_description,
#'   label = delini$label,
#'   description = delini$description,
#'   subject = delini$subject,
#'   subject_range = delini$subject_range,
#'   subject_definition = delini$subject_definition
#' )
#'
#' delini_candidates
#'
#' # Candidate dataset with a reviewable input predicate
#' delini_dual_candidates <- create_candidate_dataset(
#'   input_media_url = delini$input_media_url,
#'   input_label = delini$input_label,
#'   input_description = delini$input_description,
#'   label = delini$label,
#'   description = delini$description,
#'   subject = delini$subject,
#'   subject_definition = delini$subject_definition,
#'   input_predicate = rep("depicts", nrow(delini)),
#'   input_predicate_range = rep(
#'     add_candidate_range("depicts", "documents", "Other…"),
#'     nrow(delini)
#'   )
#' )
#'
#' delini_dual_candidates
#'
#' # Statistical example based on the W3C RDF Data Cube Vocabulary
#' w3c_candidates <- create_candidate_dataset(
#'   input_url = rep(
#'     "https://www.w3.org/TR/vocab-data-cube/",
#'     nrow(w3c_life_expectancy)
#'   ),
#'   input_label = rep(
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
#'     name = "area",
#'     value = w3c_life_expectancy$area
#'   ) |>
#'   add_candidate_column(
#'     name = "period",
#'     value = w3c_life_expectancy$period
#'   ) |>
#'   add_candidate_column(
#'     name = "sex",
#'     value = w3c_life_expectancy$sex
#'   ) |>
#'   add_candidate_column(
#'     name = "life_expectancy",
#'     value = w3c_life_expectancy$life_expectancy
#'   )
#'
#' w3c_candidates

#' @seealso [create_candidate_template()]
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
#' @export
create_candidate_dataset <- function(
  subject,
  input_url = NA_character_,
  input_media_url = NA_character_,
  input_label = NA_character_,
  input_description = NA_character_,
  label = NA_character_,
  description = NA_character_,
  alternative_label = NA_character_,
  alternative_description = NA_character_,
  input_predicate = NULL,
  input_predicate_range = NA_character_,
  subject_range = NA_character_,
  subject_definition = NA_character_,
  data_manager_name = "",
  data_manager_iri = "",
  data_manager_email = "",
  table_id = ""
) {
  if (is.null(input_predicate) &&
    !all(is.na(input_predicate_range))) {
    stop(
      "input_predicate_range requires input_predicate.",
      call. = FALSE
    )
  }

  input_description <- dplyr::case_when(
    !is.na(input_label) & !is.na(input_description) ~
      paste(input_label, input_description, sep = ": "),
    !is.na(input_label) ~ input_label,
    TRUE ~ input_description
  )

  if (is.null(input_predicate)) {
    x <- tibble::tibble(
      row_number = seq_along(subject),
      input_url = input_url,
      input_media_url = input_media_url,
      input_description = input_description,
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
      input_url = input_url,
      input_media_url = input_media_url,
      input_label = input_label,
      input_description = input_description,
      input_predicate = input_predicate,
      input_predicate_range = input_predicate_range,
      label = label,
      description = description,
      alternative_label = alternative_label,
      alternative_description = alternative_description,
      subject = subject,
      subject_range = subject_range,
      subject_definition = subject_definition
    )
  }

  attr(x, "provenance") <- list(
    data_manager = data_manager_name,
    data_manager_iri = data_manager_iri,
    data_manager_email = data_manager_email,
    table_id = table_id,
    generated_at = format(
      Sys.time(),
      tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
    ),
    software_agent = "Betwixt",
    software_version = betwixt_version() # see utils.R
  )

  x
}
