#' Determine the range type of an assertion
#'
#' Determines which review range applies to an assertion column.
#'
#' In a long review, `subject`, `predicate`, and value columns correspond to
#' the `subject`, `predicate`, and `value` range types. Numbered value columns,
#' such as `value1` and `value2`, also use the `value` range.
#'
#' In a wide review, assertion columns represent predicates and their cells
#' contain values. Wide assertion columns therefore use the `value` range.
#'
#' @param column Character string naming an assertion column.
#' @param template Character string identifying the review template.
#'
#' @return A character string identifying the applicable range type, or
#'   `NULL` when no range type applies.
#'
#' @keywords internal
#' @noRd
assertion_range_type <- function(
  column,
  template
) {
  # Wide assertion columns contain values.
  if (template == "wide_review") {
    return("value")
  }

  # Long subject and predicate columns use their corresponding ranges.
  if (column %in% c("subject", "predicate")) {
    return(column)
  }

  # Long value and numbered value columns use the value range.
  if (grepl("^value[0-9]*$", column)) {
    return("value")
  }

  # Other explicitly selected columns have no inferred range type.
  NULL
}
