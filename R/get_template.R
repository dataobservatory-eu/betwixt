#' Retrieve a bundled Mustache template
#'
#' Returns the contents of a Mustache template bundled with
#' the package.
#'
#' @param template Template name without the `.mustache`
#'   extension.
#'
#' @return A character string containing the template.
#'
#' @keywords internal
#' @noRd
get_template <- function(template = "default") {
  legacy_templates <- c(
    default = "templates/default.mustache",
    image_review = "templates/image_review.mustache"
  )

  review_templates <- list(
    long_review = c(
      html = "long_review/template_long.mustache",
      css = "long_review/long_style.css",
      js = "long_review/long_script.js"
    ),
    wide_review = c(
      html = "wide_review/template_wide.mustache",
      css = "wide_review/wide_style.css",
      js = "wide_review/wide_script.js"
    )
  )

  if (template %in% names(legacy_templates)) {
    path <- system.file(
      legacy_templates[[template]],
      package = "betwixt"
    )

    if (!nzchar(path)) {
      stop("Template file not found: '", template, "'.", call. = FALSE)
    }

    return(
      paste(
        readLines(path, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )
    )
  }

  if (!template %in% names(review_templates)) {
    stop("Unknown template: '", template, "'.", call. = FALSE)
  }

  files <- review_templates[[template]]

  paths <- vapply(
    files,
    system.file,
    FUN.VALUE = character(1),
    package = "betwixt"
  )

  missing <- names(paths)[!nzchar(paths)]

  if (length(missing)) {
    stop(
      "Missing ", template, " template file(s): ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  lapply(paths, function(path) {
    paste(
      readLines(path, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
  })
}
