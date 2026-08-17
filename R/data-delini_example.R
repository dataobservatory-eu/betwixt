#' Delini Farmstead review data
#'
#' A small reference dataset for demonstrating Betwixt review interfaces
#' using material from the Delini Farmstead case study. The dataset combines
#' evidence, multilingual descriptive metadata, reviewable semantic values,
#' and contextual information.
#'
#' The five rows represent media objects used as evidence in the review:
#' three artefact photographs and two documentary records. The artefact
#' photographs show the Delini farmhouse, a tablet-woven sash, and a bed.
#' The records show photographs of the farmhouse and a floor plan used in
#' its reconstruction.
#'
#' The dataset is designed to support both long and wide review layouts.
#' In a wide review layout, `instance_of` and `inventory_number` can be
#' treated as reviewable predicate columns, while labels and descriptions
#' are editable descriptive metadata and `held_by` provides review context.
#'
#' @format A tibble with 5 rows and 11 columns:
#' \describe{
#'   \item{evidence_id}{Identifier of the media object used as evidence.}
#'   \item{evidence_type}{Role of the media object as evidence, such as
#'     `artefact photograph` or `record`.}
#'   \item{thumbnail_url}{Package-relative path to the thumbnail used in the
#'     review interface.}
#'   \item{subject}{Human-readable placeholder identifying the subject of
#'     the candidate assertion.}
#'   \item{label_en}{English label for the depicted or documented object.}
#'   \item{description_en}{English description of the depicted or documented
#'     object.}
#'   \item{label_hu}{Hungarian label for the depicted or documented object.}
#'   \item{description_hu}{Hungarian description, where available.}
#'   \item{instance_of}{Candidate value for an `instance of` assertion.}
#'   \item{inventory_number}{Candidate inventory number, where available.}
#'   \item{held_by}{Holding institution supplied as contextual information
#'     for the review.}
#' }
#'
#' @details
#' Missing descriptive values are represented by `NA_character_` and may be
#' completed by a curator. A missing candidate assertion, such as an
#' inventory number, should not by itself be interpreted as a request to
#' supply a value.
#'
#' `delini_review` is a compact demonstrator rather than a complete
#' representation of the Delini Farmstead or its documentation.
#'
#' @seealso [delini_range]
#'
#' @examples
#' delini_review
#'
#' @name delini_review
NULL


#' Candidate ranges for the Delini Farmstead review
#'
#' Candidate subjects, predicates, and values used with [delini_review] to
#' demonstrate controlled and open review ranges in Betwixt.
#'
#' The table includes alternative interpretations of the review subject,
#' candidate predicates, selected vocabulary-backed values, and the reserved
#' `other` value used to permit an open proposal.
#'
#' @format A tibble with 14 rows and 5 columns:
#' \describe{
#'   \item{type}{Component of an elementary semantic assertion to which the
#'     range entry applies: `subject`, `predicate`, or `value`.}
#'   \item{rank}{Display order of the candidate within its type.}
#'   \item{label}{Human-readable candidate value.}
#'   \item{namespace}{Vocabulary namespace, where a controlled vocabulary
#'     term is supplied.}
#'   \item{url}{External definition or vocabulary URL, where available.}
#' }
#'
#' @details
#' The dataset intentionally contains both vocabulary-backed and local
#' candidate values. Missing namespaces or URLs therefore do not indicate
#' missing data requiring correction.
#'
#' The value `other` is reserved for an open-range review control that allows
#' the curator to propose a value not otherwise listed.
#'
#' The current range is suitable for demonstrating the long review layout.
#' Wide review layouts may associate ranges with individual predicate
#' columns, for example using a controlled range for `instance_of` while
#' treating `inventory_number` as an open textual value.
#'
#' @seealso [delini_review]
#'
#' @examples
#' delini_range
#'
#' @name delini_range
NULL
