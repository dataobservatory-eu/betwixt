#' Validate local resources
#'
#' Checks that local input resources exist and are readable.
#' Relative paths are resolved against `path`.
#' Pipe-separated resource references are supported.
#'
#' @param x A Betwixt candidate dataset.
#' @param path Directory containing the local review resources.
#' @return Invisibly returns `x` if validation succeeds.
#' @export
validate_local_resources <- function(x, path) {
  if (!dir.exists(path)) {
    stop("Review directory does not exist: ", path, call. = FALSE)
  }

  resource_cols <- intersect(
    c("input_url", "input_media_url"),
    names(x)
  )

  for (col in resource_cols) {
    values <- x[[col]]
    values <- values[!is.na(values)]
    values <- unlist(strsplit(values, "|", fixed = TRUE))
    values <- trimws(values)
    values <- values[nzchar(values)]

    if (!length(values)) next

    # Resolve relative paths against the review directory.
    absolute <- grepl("^([A-Za-z]:[/\\\\]|/|\\\\\\\\)", values)
    files <- values
    files[!absolute] <- file.path(path, values[!absolute])

    missing <- !file.exists(files)

    if (any(missing)) {
      stop(
        "Missing local resource(s) in `", col, "`: ",
        paste(values[missing], collapse = ", "),
        call. = FALSE
      )
    }

    unreadable <- file.access(files, mode = 4) != 0

    if (any(unreadable)) {
      stop(
        "Unreadable local resource(s) in `", col, "`: ",
        paste(values[unreadable], collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(x)
}
