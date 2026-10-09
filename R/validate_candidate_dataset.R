#' Validate a Betwixt candidate dataset
#'
#' Checks the structural requirements of a Betwixt candidate dataset.
#' Resource validation is handled separately.
#'
#' @param x A data frame representing a Betwixt candidate dataset.
#' @return Invisibly returns `x` if validation succeeds.
#' @seealso [validate_local_resources()], [validate_external_resources()]
#' @export
validate_candidate_dataset <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  required <- c("row_id", "subject")
  missing <- setdiff(required, names(x))

  if (length(missing)) {
    stop(
      "Missing required column(s): ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  character_cols <- c(
    "input_url", "input_media_url", "input_label", "input_description",
    "input_predicate", "label", "description",
    "alternative_label", "alternative_description", "subject"
  )

  present <- intersect(character_cols, names(x))
  wrong <- present[!vapply(x[present], is.character, logical(1))]

  if (length(wrong)) {
    stop(
      "Column(s) must be character: ",
      paste(wrong, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # Row number validation ----------------------------------------------------
  if (!is.integer(x$row_id)) {
    stop("`row_id` must be integer.", call. = FALSE)
  }

  if (anyNA(x$row_id)) {
    stop("`row_id` cannot contain missing values.", call. = FALSE)
  }

  if (anyDuplicated(x$row_id)) {
    stop("`row_id` must be unique.", call. = FALSE)
  }

  if (any(x$row_id < 1L)) {
    stop("`row_id` must contain positive integers.", call. = FALSE)
  }

  # Variable name consistency ------------------------------------------------
  metadata <- grep("_(range|definition)$", names(x), value = TRUE)
  base <- sub("_(range|definition)$", "", metadata)
  missing <- unique(base[!base %in% names(x)])

  if (length(missing)) {
    stop(
      "Candidate metadata without candidate column(s): ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(x)
}
