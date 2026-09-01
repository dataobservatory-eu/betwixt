#' W3C RDF Data Cube life expectancy data
#'
#' @description
#' A small statistical dataset based on the life expectancy example in the
#' W3C RDF Data Cube Vocabulary specification. The observations demonstrate
#' a multidimensional statistical use case that can be transformed into
#' candidate data for human review with Betwixt.
#'
#' The example contains male life expectancy observations for four Welsh
#' areas during the 2004--2006 reference period.
#'
#' @format A data frame with 4 rows and 5 variables:
#' \describe{
#'   \item{observation}{Identifier of the RDF Data Cube observation.}
#'   \item{area}{Reference area of the observation.}
#'   \item{period}{Reference period of the observation.}
#'   \item{sex}{Sex dimension of the observation.}
#'   \item{life_expectancy}{Life expectancy, measured in years.}
#' }
#'
#' @source
#' W3C RDF Data Cube Vocabulary, life expectancy example:
#' \url{https://www.w3.org/TR/vocab-data-cube/}
#'
#' @references
#' Cyganiak, R., Reynolds, D., and Tennison, J. (2014).
#' RDF Data Cube Vocabulary. W3C Recommendation, 16 January 2014.
#'
#' @examples
#' data(w3c_life_expectancy)
#' w3c_life_expectancy
#'
"w3c_life_expectancy"
