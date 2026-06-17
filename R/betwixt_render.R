#' Render a claim for review
#'
#' @param claim A claim.
#'
#' @return Character HTML fragment.
#' @export
betwixt_render <- function(claim) {

  paste(
    "Claim:",
    claim$subject,
    claim$predicate,
    claim$value,
    "within",
    claim$scope
  )
}
