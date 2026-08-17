#' Scoped semantic claims
#'
#' A `claim_df` is a tabular collection of scoped semantic claims.
#'
#' Each row represents a semantic claim consisting of:
#'
#' - `scope`
#' - `subject`
#' - `predicate`
#' - `value`
#'
#' A `claim_df` may contain a single claim created with [claim()]
#' or multiple claims created with [as_claim_df()].
#'
#' @section Required columns:
#'
#' A valid `claim_df` must contain the columns:
#'
#' * `scope`
#' * `subject`
#' * `predicate`
#' * `value`
#'
#' @seealso [claim()], [as_claim_df()]
#'
#' @name claim_df
NULL

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


#' @rdname claim_df
#' @export
is.claim_df <- function(x) {
  inherits(x, "claim_df")
}
