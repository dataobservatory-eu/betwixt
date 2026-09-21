#' Identify a Betwixt representation
#'
#' Identifies whether a Betwixt table uses wide, dual-wide, long, or dual-long
#' representation. It returns `"invalid"` for tables that cannot be identified
#' as a valid Betwixt representation.
#'
#' @param x A data frame representing a Betwixt table.
#'
#' @return One of `"wide"`, `"dual_wide"`, `"long"`, `"dual_long"`, or
#'   `"invalid"`.
#'
#' @examples
#' x <- data.frame(
#'   row_id = 1:2,
#'   subject = c("Q1", "Q2"),
#'   instance_of = c("human", "human"),
#'   occupation = c("composer", "pianist")
#' )
#'
#' betwixt_representation(x)
#' # "wide"
#'
#' @export
betwixt_representation <- function(x) {
  if (!is.data.frame(x)) {
    return("invalid")
  }

  nms <- names(x)

  if (!all(c("row_id", "subject") %in% nms)) {
    return("invalid")
  }

  excluded <- grepl(
    "^(row_id$|comment_|plane$|input_|subject_)",
    nms
  )

  remaining <- nms[!excluded]
  has_long <- all(c("subject", "predicate", "value") %in% remaining)

  shape <- dplyr::case_when(
    length(remaining) > 3L ~ "wide",
    has_long ~ "long",
    length(remaining) >= 3L ~ "wide",
    TRUE ~ "invalid"
  )

  if (shape == "invalid") {
    return(shape)
  }

  if (any(grepl("^input_", nms))) {
    paste0("dual_", shape)
  } else {
    shape
  }
}
