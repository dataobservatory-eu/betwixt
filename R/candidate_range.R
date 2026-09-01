#' Create a Betwixt candidate range
#'
#' @description
#' Creates the canonical representation of a candidate value range.
#'
#' Values may be supplied as a character vector or as one or more
#' pipe-delimited character strings. Whitespace surrounding values is removed.
#'
#' @param ... Character values defining the candidate range.
#'
#' @return A single pipe-delimited character string. If no values are
#'   supplied, `NA_character_` is returned.
#'
#' @examples
#' candidate_range("depicts", "documents")
#' candidate_range(c("depicts", "documents"))
#' candidate_range("depicts|documents")
#' candidate_range("depicts | documents")
#'
#' @export
candidate_range <- function(...) {
  values <- unlist(list(...), use.names = FALSE)

  if (length(values) == 0L || all(is.na(values))) {
    return(NA_character_)
  }

  values <- values[!is.na(values)]
  values <- unlist(strsplit(values, "|", fixed = TRUE))
  values <- trimws(values)
  values <- values[nzchar(values)]

  paste(values, collapse = "|")
}
