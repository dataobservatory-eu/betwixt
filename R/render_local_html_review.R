#' Render a local HTML Betwixt review
#' @rdname render_review
#' @examples
#' # Create a local review using photographs bundled with Betwixt.
#' data(delini)
#'
#' review_path <- tempfile("delini-review-")
#' dir.create(file.path(review_path, "media"), recursive = TRUE)
#'
#' # Resolve packaged images and copy them into the review directory.
#' source_media <- sub(
#'   "^.*?/images/delini/",
#'   "media/delini/",
#'   delini$input_media_url
#' )
#' source_files <- system.file(source_media, package = "betwixt")
#' stopifnot(all(nzchar(source_files)))
#'
#' file.copy(source_files, file.path(review_path, "media"))
#'
#' # Use relative references in the candidate dataset.
#' candidate <- create_candidate_dataset(
#'   subject = delini$subject,
#'   input_media_url = file.path("media", basename(source_files)),
#'   input_label = delini$input_label,
#'   input_description = delini$input_description,
#'   label = delini$label,
#'   description = delini$description,
#'   subject_range = delini$subject_range,
#'   subject_definition = delini$subject_definition
#' )
#'
#' render_local_html_review(
#'   candidate,
#'   title = "Deliņi Farmstead Review",
#'   filename_stem = "delini-review",
#'   path = review_path
#' )
#' @export

render_local_html_review <- function(
  candidate,
  cols = NULL,
  subheadings = NULL,
  title = "Betwixt Review",
  description = "Please review the following claims.",
  filename_stem = "betwixt-review",
  reviewer_name = "",
  reviewer_iri = "",
  table_id = "",
  project_id = "",
  sequence = 0L,
  row_comment = FALSE,
  review_comment = FALSE,
  path = NULL
) {
  validate_candidate_dataset(candidate)
  validate_local_resources(candidate, path)

  # Validate candidate structure and local resources.
  if (length(sequence) != 1L ||
    is.na(sequence) ||
    sequence < 0 ||
    sequence != as.integer(sequence)) {
    stop(
      "sequence must be a single non-negative integer.",
      call. = FALSE
    )
  }

  # Subject is a required column
  if (!"subject" %in% names(candidate)) {
    stop(
      "`render_review()` requires a `subject` column. ",
      "Each candidate row must identify the subject of the assertions ",
      "being reviewed.",
      call. = FALSE
    )
  }

  # Store the validated sequence as an integer.
  sequence <- as.integer(sequence)

  # Record the creation time in ISO 8601 UTC at one-second precision.
  original_created_at <- format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  )

  # Record the filename of the HTML artefact created by this render.
  original_filename <- if (sequence == 0L) {
    paste0(filename_stem, ".html")
  } else {
    paste0(filename_stem, "_", sequence, ".html")
  }

  # Prepare the candidate data for rendering.
  context <- prepare_review_context(candidate)

  # Generate the review table.
  table_html <- review_context_html(
    context,
    cols = cols,
    subheadings = subheadings,
    row_comment = row_comment
  )

  # Locate the packaged review resources.
  css_file <- system.file(
    "templates", "css", "betwixt-review.css",
    package = "betwixt"
  )

  js_file <- system.file(
    "templates", "js", "betwixt-review.js",
    package = "betwixt"
  )

  # Require both resources before constructing the review.
  if (!nzchar(css_file)) {
    stop("Betwixt review CSS was not found.", call. = FALSE)
  }

  if (!nzchar(js_file)) {
    stop("Betwixt review JavaScript was not found.", call. = FALSE)
  }

  # Read the resources into the standalone document.
  css <- paste(readLines(css_file, warn = FALSE), collapse = "\n")
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  # Escape document-level text before inserting it into HTML.
  escape_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }

  # Render non-empty candidate provenance above the site footer.
  provenance <- context$provenance

  provenance_items <- c(
    if (nzchar(provenance$data_manager)) {
      paste0("Data manager: ", escape_html(provenance$data_manager))
    },
    if (nzchar(provenance$data_manager_iri)) {
      paste0("IRI: ", escape_html(provenance$data_manager_iri))
    },
    if (nzchar(provenance$table_id)) {
      paste0("Project: ", escape_html(provenance$table_id))
    },
    if (!is.na(provenance$generated_at) &&
      nzchar(provenance$generated_at)) {
      paste0("Generated: ", escape_html(provenance$generated_at))
    },
    paste0(
      escape_html(provenance$software_agent), " ",
      escape_html(provenance$software_version)
    )
  )

  provenance_footer <- paste0(
    '<div class="candidate-provenance">',
    paste(provenance_items, collapse = " \u00b7 "),
    "</div>\n"
  )

  # Render an optional comment field for the review as a whole.
  review_comment_html <- if (review_comment) {
    paste0(
      '<label class="review-comment">Review comment',
      '<textarea id="review-comment" ',
      'placeholder="Optional comment on review"></textarea>',
      "</label>\n"
    )
  } else {
    ""
  }

  # Assemble the standalone review document.
  html <- assemble_review_html(
    title = title,
    description = description,
    table_html = table_html,
    css = css,
    js = js,
    reviewer_name = reviewer_name,
    reviewer_iri = reviewer_iri,
    project_id = project_id,
    table_id = table_id,
    sequence = sequence,
    original_filename = original_filename,
    original_created_at = original_created_at,
    filename_stem = filename_stem,
    review_comment_html = review_comment_html,
    provenance_footer = provenance_footer
  )

  # Return the HTML directly when no output path is requested.
  if (is.null(path)) {
    return(html)
  }

  # Write the review using the filename recorded in its provenance.
  output <- file.path(path, original_filename)

  writeLines(html, output, useBytes = TRUE)

  message("Review rendered: ", output)

  invisible(html)
}
