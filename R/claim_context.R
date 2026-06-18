#' @keywords internal
#' @noRd
claim_context <- function(x) {

  if (!inherits(x, "claim_df")) {
    stop("x must inherit from claim_df", call. = FALSE)
  }

  claims <- lapply(
    seq_len(nrow(x)),
    function(i) {
      as.list(x[i, , drop = FALSE])
    }
  )

  list(claims = claims)
}
