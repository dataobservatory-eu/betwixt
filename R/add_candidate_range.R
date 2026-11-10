#' Create a Betwixt candidate range
#'
#' @description
#' Creates the canonical pipe-delimited representation of a candidate
#' value range used in Betwixt semantic review.
#'
#' Values may be supplied as a character vector or as one or more
#' pipe-delimited character strings. Whitespace surrounding values is removed.
#'
#' Candidate ranges may include ordinary values and reserved special values
#' that control how the reviewer can respond to a semantic assertion.
#'
#' @param ... Character values defining the candidate range.
#'
#' @details
#' Candidate ranges define the values offered to a reviewer when examining
#' a semantic assertion. The values are preserved in their supplied order.
#'
#' The function combines the supplied values into a single pipe-delimited
#' string. It does not validate their semantic meaning, resolve entities,
#' or interpret special values.
#'
#' @section Special candidate-range values:
#' Betwixt recognises the following reserved values in candidate ranges:
#'
#' \describe{
#'   \item{`<other>`}{
#'     Allows the reviewer to enter a value outside the proposed candidate
#'     range. Displayed as "Other curator entered".
#'   }
#'   \item{`<unknown>`}{
#'     Indicates that an applicable value may exist, but its value is
#'     unknown. Displayed as "Value unknown".
#'   }
#'   \item{`<no-value>`}{
#'     Indicates that no applicable value is possible.
#'     Displayed as "No value possible".
#'   }
#' }
#'
#' `Other…` (Unicode ellipsis) and `Other...`
#' (three periods) are also recognised as equivalents of `<other>`.
#'
#' The distinction between these values is semantic:
#'
#' - `<other>` requests a curator-entered replacement value.
#' - `<unknown>` expresses uncertainty about an applicable value.
#' - `<no-value>` expresses the absence of an applicable value.
#'
#' Ordinary strings such as `Other`, `Unknown`, and `None` retain their
#' literal meanings and are not interpreted as reserved values.
#'
#' Special values are optional and must be explicitly included in the
#' candidate range. They are not automatically appended.
#'
#' `add_candidate_range()` preserves the supplied tokens. Their special
#' behaviour is applied when the candidate range is rendered in the
#' Betwixt review interface.
#'
#' @return A single pipe-delimited character string. If no values are
#'   supplied, `NA_character_` is returned.
#'
#' @examples
#' # Ordinary candidate ranges
#' add_candidate_range("depicts", "documents")
#' add_candidate_range(c("depicts", "documents"))
#' add_candidate_range("depicts|documents")
#' add_candidate_range("depicts | documents")
#'
#' # Allow a curator to enter another value
#' add_candidate_range("Spreading", "Debunking", "<other>")
#'
#' # Allow explicit uncertainty or absence of an applicable value
#' add_candidate_range("Farmhouse", "Dwelling", "<unknown>", "<no-value>")
#'
#' # Combine ordinary and special values
#' add_candidate_range(
#'   "Spreading", "Debunking", "Discussing",
#'   "<other>", "<unknown>", "<no-value>"
#' )
#'
#' @export
add_candidate_range <- function(...) {
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
