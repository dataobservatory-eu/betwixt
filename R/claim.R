#' Create a semantic claim
#'
#' Creates a single semantic claim as a one-row `claim_df`.
#' A claim consists of four elements:
#'
#' - `scope`: the context in which the claim is made;
#' - `subject`: the entity or class of entities under consideration;
#' - `predicate`: the asserted property;
#' - `value`: the asserted value.
#'
#' The meaning of a claim derives from its scope. A scope may represent
#' a statistical observation, an archival record set, a filesystem folder,
#' a namespace, or any other well-defined context.
#'
#' @param scope Scope of the claim.
#' @param subject Subject within the scope.
#' @param predicate Predicate of the claim.
#' @param value Claimed value.
#'
#' @return A one-row `claim_df`.
#'
#' @examples
#' # Statistical example:
#' #
#' # A claim about the GDP of Andorra in a given year.
#' #
#' ad_gdp <- claim(
#'   scope = "country=AD;year=2023",
#'   subject = "Andorra",
#'   predicate = "GDP",
#'   value = "3.73 billion EUR"
#' )
#'
#' ad_gdp
#' dim(ad_gdp)
#'
#' # Archival example:
#' #
#' # A folder contains six JPG files that are digital surrogates
#' # of two historical letters:
#' #
#' # box45/
#' #   letterA_p1r.jpg
#' #   letterA_p1v.jpg
#' #   letterB_p1r.jpg
#' #   letterB_p1v.jpg
#' #   letterB_p2r.jpg
#' #   letterB_p2v.jpg
#' #
#' # Instead of creating six separate claims, the claim is made
#' # at the scope of the box represented by the folder.
#' #
#' latin_box <- claim(
#'   scope = "box45",
#'   subject = "page",
#'   predicate = "language",
#'   value = "Latin"
#' )
#'
#' latin_box
#' dim(latin_box)
#'
#' @export
claim <- function(
    scope,
    subject,
    predicate,
    value
) {

  x <- tibble::tibble(
    scope = scope,
    subject = subject,
    predicate = predicate,
    value = value
  )

  validate_claim_df(x)

  new_claim_df(x)
}

#' @noRd
#' @keywords internal
new_claim_df <- function(x) {

  stopifnot(
    is.data.frame(x),
    all(c(
      "scope",
      "subject",
      "predicate",
      "value"
    ) %in% names(x))
  )

  class(x) <- c(
    "claim_df",
    class(x)
  )

  x
}

#' @noRd
#' @keywords internal
validate_claim_df <- function(x) {

  required <- c(
    "scope",
    "subject",
    "predicate",
    "value"
  )

  missing_cols <- setdiff(required, names(x))

  if (length(missing_cols)) {
    stop(
      "Missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  TRUE
}

#' @rdname claim_df
#' @export
print.claim_df <- function(x, ...) {

  n_claims <- nrow(x)

  scopes <- unique(x$scope)

  cat(
    "<claim_df>\n",
    "Claims: ", n_claims, "\n",
    "Scopes: ", length(scopes), "\n",
    sep = ""
  )

  if (length(scopes) == 1) {
    cat("Scope: ", scopes, "\n\n", sep = "")
  } else {
    cat(
      "Scope examples: ",
      paste(utils::head(scopes, 3), collapse = ", "),
      "\n\n",
      sep = ""
    )
  }

  print(tibble::as_tibble(x), ...)

  invisible(x)
}
