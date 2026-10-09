#' Validate external resources
#'
#' Checks that input resource references use HTTP or HTTPS URLs.
#' Pipe-separated resource references are supported.
#'
#' @param x A Betwixt candidate dataset.
#' @return Invisibly returns `x` if validation succeeds.
#' @export
validate_external_resources <- function(x) {
  url_cols <- intersect(
    c("input_url", "input_media_url"),
    names(x)
  )

  for (col in url_cols) {
    values <- x[[col]]
    values <- values[!is.na(values)]
    values <- unlist(strsplit(values, "|", fixed = TRUE))
    values <- trimws(values)
    values <- values[nzchar(values)]

    invalid <- !grepl("^https?://[^[:space:]]+$", values)

    if (any(invalid)) {
      stop("Invalid URL in `", col, "`.", call. = FALSE)
    }
  }

  invisible(x)
}
