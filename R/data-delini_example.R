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
#' The dataset is designed to support both long and wide review projections.
#' In a wide review, `instance_of` can be treated as a reviewable predicate
#' column, labels and descriptions can be presented as editable descriptive
#' metadata, and `evidence_type` and `held_by` can provide review context.
#' Evidence may be represented by a thumbnail and, where available, a URL
#' to the source inspected by the reviewer.
#'
#' @format A tibble with 5 rows and 12 columns:
#' \describe{
#'   \item{evidence_id}{Identifier of the media object used as evidence.}
#'   \item{thumbnail_url}{URL of the thumbnail presented as visual evidence
#'     in the review interface.}
#'   \item{evidence_url}{URL of the supporting source, where available.}
#'   \item{subject}{Human-readable placeholder identifying the subject of
#'     the candidate assertion.}
#'   \item{label_en}{English label for the depicted or documented object.}
#'   \item{description_en}{English description of the depicted or documented
#'     object.}
#'   \item{label_hu}{Hungarian label for the depicted or documented object.}
#'   \item{description_hu}{Hungarian description, where available.}
#'   \item{inventory_number}{Inventory number, where available.}
#'   \item{instance_of}{Candidate value for an `instance of` assertion.}
#'   \item{evidence_type}{Role of the media object as evidence, such as
#'     `artefact photograph` or `record`.}
#'   \item{held_by}{Holding institution supplied as contextual information
#'     for the review.}
#' }
#'
#' @details
#' The semantic role of a column is not fixed by this dataset. The renderer
#' determines explicitly which columns are presented as evidence, descriptive
#' metadata, reviewable assertions, or contextual information.
#'
#' Missing descriptive values are represented by `NA_character_` and may be
#' completed by a curator. A missing value in another column should not by
#' itself be interpreted as a request to supply a value.
#'
#' `delini_review` is a compact demonstrator rather than a complete
#' representation of the Delini Farmstead or its documentation.
#'
#' @seealso [delini_range], [betwixt_render()]
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
#' Range entries are identified by the component of an elementary assertion
#' to which they apply. Value ranges may additionally identify a `field`,
#' allowing a wide review to associate a range with a particular predicate
#' column.
#'
#' @format A tibble with 13 rows and 6 columns:
#' \describe{
#'   \item{type}{Component of an elementary semantic assertion to which the
#'     range entry applies: `subject`, `predicate`, or `value`.}
#'   \item{field}{Optional predicate field to which a value range applies.
#'     `NA_character_` indicates that the range is not restricted to a
#'     particular wide-review field.}
#'   \item{rank}{Display order of the candidate within its applicable range.}
#'   \item{label}{Human-readable candidate value.}
#'   \item{namespace}{Vocabulary namespace, where a controlled vocabulary
#'     term is supplied.}
#'   \item{url}{External definition or vocabulary URL, where available.}
#' }
#'
#' @details
#' Subject and predicate ranges are general components of an elementary
#' assertion and therefore do not require a `field`. The example value range
#' is associated with `instance_of`, allowing it to be used as the controlled
#' range for that predicate column in a wide review.
#'
#' The dataset intentionally contains both vocabulary-backed and local
#' candidate values. Missing namespaces or URLs therefore do not indicate
#' missing data requiring correction.
#'
#' The value `other` is reserved for an open-range review control that allows
#' the curator to propose a value not otherwise listed.
#'
#' When a current assertion value also occurs in its applicable range,
#' Betwixt may exclude that entry from the alternatives because the current
#' value is rendered separately as the initially selected value.
#'
#' @seealso [delini_review], [betwixt_render()]
#'
#' @examples
#' delini_range
#'
#' @name delini_range
NULL
