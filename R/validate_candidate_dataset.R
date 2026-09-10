#' Validate a Betwixt candidate dataset
#'
#' Checks whether a data frame conforms to the structural requirements of a
#' Betwixt candidate dataset.
#'
#' @param x A data frame representing a Betwixt candidate dataset.
#'
#' @details
#' Candidate datasets can be created in a spreadsheet application after
#' initialising a conforming header with [create_candidate_template()].
#' Re-imported datasets can be checked with `validate_candidate_dataset()`
#' before rendering with [render_review()].
#'
#' @return Invisibly returns `x` if validation succeeds. Otherwise, an error
#'   describes the first validation failure.
#'
#' @examples
#' x <- create_candidate_template()
#' validate_candidate_dataset(x)
#'
#' @export
validate_candidate_dataset <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  required <- c("row_number", "subject")
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
    "evidence_url", "evidence_media_url", "evidence_text",
    "evidence_relation", "label", "description",
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
  if (!is.integer(x$row_number)) {
    stop("`row_number` must be integer.", call. = FALSE)
  }

  if (anyNA(x$row_number)) {
    stop("`row_number` cannot contain missing values.", call. = FALSE)
  }

  if (anyDuplicated(x$row_number)) {
    stop("`row_number` must be unique.", call. = FALSE)
  }

  if (any(x$row_number < 1L)) {
    stop("`row_number` must contain positive integers.", call. = FALSE)
  }

  # URL validation -------------------------------------------------------

  url_cols <- intersect(
    c("evidence_url", "evidence_media_url"),
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

  ## Variable name consistency validation -----------------------------------
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
