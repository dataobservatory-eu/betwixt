#' @noRd
#' @keywords internal
betwixt_version <- function() {
  as.character(getNamespaceInfo(asNamespace("betwixt"), "spec")["version"])
}


#' @noRd
#' @keywords internal
escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

#' @noRd
#' @keywords internal
qualification_html <- function() {
  paste0(
    '<div class="qualify">',
    '<button data-qualify="defer">Defer</button>',
    '<button data-qualify="reject">Reject</button>',
    "</div>"
  )
}
