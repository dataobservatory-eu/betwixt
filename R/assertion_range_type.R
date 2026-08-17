#' Determine the range type of a review assertion
#'
#' Maps an assertion column to the range used by the selected review
#' projection. Wide review columns contain assertion values. Long review
#' columns represent explicit subject, predicate, or value components.
#'
#' @param column Character string naming an assertion column.
#' @param template Character string naming the review template.
#'
#' @return A character string naming the range type, or `NULL` when the
#'   long assertion column does not have a defined range type.
#'
#' @keywords internal
#' @noRd
assertion_range_type <- function(column, template) {
  if (template == "wide_review") {
    return("value")
  }

  if (column %in% c("subject", "predicate")) {
    return(column)
  }

  if (grepl("^value[0-9]*$", column)) {
    return("value")
  }

  NULL
}
