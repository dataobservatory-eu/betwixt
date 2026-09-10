#' Small country GDP candidate dataset
#'
#' A small example candidate dataset containing GDP observations for
#' Iceland and Malta in 2023 and 2024. The dataset demonstrates the
#' Betwixt candidate-data structure using statistical data.
#'
#' @format A data frame with 4 rows and the following variables:
#' \describe{
#'   \item{row_number}{Stable integer identifier for the candidate row.}
#'   \item{evidence_url}{Resolvable DOI of the source Eurostat dataset.}
#'   \item{label}{Human-readable label for the observation.}
#'   \item{description}{Human-readable description of the observation.}
#'   \item{subject}{Human-readable name of the country.}
#'   \item{subject_definition}{GeoNames URI identifying the country.}
#'   \item{country_code}{ISO 3166-1 alpha-2 country code.}
#'   \item{country_code_definition}{URI defining the country-code system.}
#'   \item{gdp}{Gross domestic product at market prices.}
#'   \item{gdp_definition}{URI identifying the Eurostat GDP concept B1GQ.}
#'   \item{context_year}{Reference year of the GDP observation; not reviewed.}
#'   \item{context_unit}{Unit in which GDP is expressed; not reviewed.}
#' }
#'
#' @source Eurostat, National Accounts. The source dataset is identified by
#'   \doi{10.2908/NAIDA_10_GDP}.
#'
#' @examples
#' small_countries_dataset
#' validate_candidate_dataset(small_countries_dataset)
#'
#' @keywords datasets
"small_countries_dataset"
