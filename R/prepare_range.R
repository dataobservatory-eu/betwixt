#' Prepare a review range
#'
#' Converts rows from a review range table into the list structure used by
#' Betwixt review templates.
#'
#' Review ranges define proposed or permitted values for review controls.
#' A range may apply to a general component of an elementary assertion,
#' such as `subject`, `predicate`, or `value`. In a wide review, a value
#' range may additionally be restricted to a particular predicate column
#' using `field`.
#'
#' The current value may be excluded from the prepared alternatives because
#' it is rendered separately as the selected value of the review control.
#'
#' The reserved label `other` represents an open-range option and is marked
#' separately from ordinary range values.
#'
#' @param range A data frame containing review range definitions, or `NULL`.
#'   The data frame must contain `type`, `rank`, `label`, `namespace`, and
#'   `url`. It may additionally contain `field`.
#' @param type Character string identifying the range type to prepare.
#' @param field Optional character string identifying a specific wide-review
#'   field. If supplied and `range` contains a `field` column, only rows for
#'   that field are returned.
#' @param current Optional current value to exclude from the prepared range.
#'
#' @return A list of range entries ordered by `rank`. Each entry contains
#'   `label`, `display_label`, `namespace`, `url`, `has_url`, and `is_other`.
#'   An empty list is returned when no applicable range is available.
#'
#' @keywords internal
#' @noRd
prepare_range <- function(
  range,
  type,
  field = NULL,
  current = NULL
) {
  # Return early when no range is supplied.
  if (is.null(range)) {
    return(list())
  }

  # Select the requested range type.
  x <- range[range$type == type, , drop = FALSE]

  # Restrict wide ranges to the requested predicate field.
  if (!is.null(field) && "field" %in% names(x)) {
    x <- x[
      !is.na(x$field) & x$field == field, ,
      drop = FALSE
    ]
  }

  # Exclude the current value from the proposed alternatives.
  if (!is.null(current) && !is.na(current)) {
    x <- x[
      is.na(x$label) |
        as.character(x$label) != as.character(current), ,
      drop = FALSE
    ]
  }

  # Return early when no applicable range remains.
  if (nrow(x) == 0) {
    return(list())
  }

  # Preserve the declared range order.
  x <- x[order(x$rank), , drop = FALSE]

  # Prepare the range entries expected by the templates.
  lapply(seq_len(nrow(x)), function(i) {
    label <- as.character(x$label[i])
    namespace <- x$namespace[i]
    url <- x$url[i]

    is_other <- identical(
      tolower(label),
      "other"
    )

    display_label <- label

    if (!is_other && !is.na(namespace) && nzchar(namespace)) {
      display_label <- paste0(
        namespace,
        ":",
        label
      )
    }

    list(
      label = label,
      display_label = display_label,
      namespace = if (is.na(namespace)) "" else namespace,
      url = if (is.na(url)) "" else url,
      has_url = !is.na(url) && nzchar(url),
      is_other = is_other
    )
  })
}
