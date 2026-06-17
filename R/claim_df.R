#' Coerce an object to a claim_df
#'
#' Converts supported objects into a `claim_df`.
#'
#' @param x An object to coerce.
#'
#' @return A `claim_df`.
#'
#' @examples
#' andorra_gdp <- data.frame(
#'   scope = "country=AD;year=2023",
#'   subject = "country",
#'   predicate = "GDP",
#'   value = "3.73 billion EUR"
#' )
#'
#' as_claim_df(andorra_gdp)
#'
#' latin_box <- data.frame(
#'   scope = "box45",
#'   subject = "page",
#'   predicate = "language",
#'   value = "Latin"
#' )
#'
#' as_claim_df(latin_box)
#'
#' @export
as_claim_df <- function(x) {
  UseMethod("as_claim_df")
}

#' @rdname as_claim_df
#' @export
as_claim_df.data.frame <- function(x) {

  validate_claim_df(x)

  new_claim_df(x)
}


