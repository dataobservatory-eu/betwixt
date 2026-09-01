#' Delini cultural heritage candidate data
#'
#' @description
#' A small cultural heritage dataset describing the Delini farmstead and
#' associated artefacts and archival records. The data provide a worked
#' example of heterogeneous semantic candidates that can be presented for
#' human review with Betwixt.
#'
#' Each row associates an evidential resource with a described entity and
#' candidate semantic values. Candidate values are accompanied, where
#' applicable, by controlled ranges and semantic definitions.
#'
#' @format A data frame with 5 rows and 15 variables:
#' \describe{
#'   \item{row_number}{Integer row identifier.}
#'   \item{evidence_url}{URL of the evidential resource.}
#'   \item{evidence_text}{Short human-readable identifier for the evidence.}
#'   \item{label}{Human-readable label of the described entity.}
#'   \item{description}{Human-readable description of the described entity.}
#'   \item{col_1}{Candidate identifier or value for the first semantic
#'   component.}
#'   \item{col_1_range}{Optional controlled range for `col_1`.}
#'   \item{col_1_definition}{Optional semantic definition associated with
#'   `col_1`.}
#'   \item{col_2}{Candidate value for the second semantic component.}
#'   \item{col_2_range}{Optional controlled range for `col_2`.}
#'   \item{col_2_definition}{Optional semantic definition associated with
#'   `col_2`.}
#'   \item{col_3}{Candidate value for the third semantic component.}
#'   \item{col_3_range}{Optional controlled range for `col_3`.}
#'   \item{col_3_definition}{Optional semantic definition associated with
#'   `col_3`.}
#'   \item{context_1}{Display-only contextual information supplied to the
#'   reviewer.}
#' }
#'
#' @source
#' Delini cultural heritage materials used in the Betwixt worked example.
#'
#' @references
#' Antal, D. [Insert final conference paper citation and DOI.]
#'
#' @examples
#' data(delini)
#' head(delini)
#'
"delini"
