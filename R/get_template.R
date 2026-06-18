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
get_template <- function(
    template = "default"
) {

  template_file <- system.file(
    "templates",
    paste0(template, ".mustache"),
    package = "betwixt"
  )

  paste(
    readLines(template_file, warn = FALSE),
    collapse = "\n"
  )
}
