

#' @noRd
#' @keywords internal
betwixt_version <- function() {
  as.character(getNamespaceInfo(asNamespace("betwixt"), "spec")["version"])
}
