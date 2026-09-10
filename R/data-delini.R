#' Delini cultural heritage candidate data
#'
#' @description
#' A small cultural heritage dataset describing three artefacts and two
#' archival records associated with the Delini farmstead at the Ethnographic
#' Open-Air Museum of Latvia. It provides a worked example of heterogeneous
#' semantic candidates that can be presented for human review with Betwixt.
#'
#' The three artefacts are the Delini farmhouse, a tablet-woven sash, and a
#' bed. The two archival records document the farmhouse: one contains
#' photographs of it, while the other is a floor plan prepared for its
#' reconstruction.
#'
#' Each row associates an evidential resource with a described entity and
#' candidate semantic values. Candidate values are accompanied, where
#' applicable, by controlled ranges and semantic definitions.
#'
#' @format A data frame with 5 rows and 16 variables:
#' \describe{
#'   \item{row_number}{Integer row identifier.}
#'   \item{evidence_url}{Optional URL of an evidential resource.}
#'   \item{evidence_media_url}{URL of the evidential media resource.}
#'   \item{evidence_text}{Short human-readable identifier for the evidence.}
#'   \item{label}{Human-readable label of the described entity.}
#'   \item{description}{Human-readable description of the described entity.}
#'   \item{subject}{Candidate subject identifier or value.}
#'   \item{subject_range}{Optional controlled range for the subject.}
#'   \item{subject_definition}{Optional semantic definition or resolvable
#'   reference associated with the subject.}
#'   \item{instance_of}{Candidate value for the `instance of` assertion.}
#'   \item{instance_of_range}{Optional controlled range for the `instance of`
#'   value.}
#'   \item{instance_of_definition}{Optional semantic definition associated
#'   with the `instance of` candidate.}
#'   \item{heritage_of}{Candidate value for the `heritage of` assertion.}
#'   \item{heritage_of_range}{Optional controlled range for the `heritage of`
#'   value.}
#'   \item{heritage_of_definition}{Optional semantic definition associated
#'   with the `heritage of` candidate.}
#'   \item{context_1}{Display-only holding-institution context supplied to
#'   the reviewer.}
#' }
#'
#' @source
#' Delini cultural heritage materials used in the Betwixt worked example.
#'
#' @examples
#' data(delini)
#' head(delini)
#'
"delini"
