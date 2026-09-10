#' Read a Betwixt review
#'
#' Reads a standalone Betwixt review HTML file and reconstructs the original
#' candidate state, the persisted reviewed state, review metadata, and review
#' provenance.
#'
#' `read_review()` does not finalise semantic knowledge or construct a
#' subsequent stabilised semantic state. It reconstructs the review artefact
#' as saved by Betwixt.
#'
#' The returned `candidate` and `reviewed` data frames preserve the same
#' evidence, descriptive, and semantic fields. The `reviewed` data additionally
#' contains assertion qualifications, row finalisation state, row outcome, and
#' optional row-level comments.
#'
#' Review-wide information such as reviewer identity and IRI, lifecycle
#' timestamps, and the review-level comment is returned separately in
#' `provenance`.
#'
#' @param path A single character string giving the path to a standalone
#'   Betwixt review HTML file.
#'
#' @return A named list with four elements:
#'
#' \describe{
#'   \item{metadata}{
#'     Review-level metadata including `title`, `description`, `project_id`,
#'     `sequence`, and `status`.
#'   }
#'   \item{provenance}{
#'     Review provenance including the original and saved filenames,
#'     creation and lifecycle timestamps, reviewer name, IRI and email,
#'     and the optional review-level comment.
#'   }
#'   \item{candidate}{
#'     A data frame reconstructing the candidate state presented for review.
#'     It contains row number, evidence fields, descriptive fields, and
#'     candidate semantic assertion values.
#'   }
#'   \item{reviewed}{
#'     A data frame reconstructing the persisted reviewed state. It contains
#'     the current descriptive and semantic values, assertion qualifications,
#'     row finalisation state, row outcome, and optional row-level comments.
#'   }
#' }
#'
#' Empty optional evidence fields and comments are returned as `NA_character_`.
#' `row_number` is returned as integer and `reviewed$finalised` as logical.
#'
#' @details
#' Betwixt review files preserve both original candidate values and the current
#' reviewed values in machine-readable HTML attributes and controls.
#' `read_review()` uses these persisted values to reconstruct the two states
#' independently.
#'
#' A finalised Betwixt review is a completed review artefact. Reading such a
#' file does not promote reviewed assertions into a subsequent stabilised
#' semantic state or apply them to an external knowledge system.
#'
#' @examples
#' \dontrun{
#' review <- read_review("muis-garments-review_1-finalised.html")
#'
#' review$metadata
#' review$provenance
#' review$candidate
#' review$reviewed
#' }
#'
#' @export
read_review <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("path must be a single non-empty character string.", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("Review file does not exist: ", path, call. = FALSE)
  }

  document <- xml2::read_html(path)
  review_table <- xml2::xml_find_first(document, '//*[@id="review-table"]')

  if (inherits(review_table, "xml_missing")) {
    stop("The file does not contain a Betwixt review table.", call. = FALSE)
  }

  # Read a metadata input.
  input_value <- function(id) {
    xpath <- paste0('//*[@id="', id, '"]')
    node <- xml2::xml_find_first(document, xpath)
    if (inherits(node, "xml_missing")) {
      return(NA_character_)
    }

    value <- xml2::xml_attr(node, "value")
    if (is.na(value) || !nzchar(value) || identical(value, "NA")) {
      return(NA_character_)
    }
    value
  }

  candidate_provenance_value <- function(name) {
    xpath <- paste0('//*[@data-provenance="', name, '"]')
    node <- xml2::xml_find_first(document, xpath)

    if (inherits(node, "xml_missing")) {
      return(NA_character_)
    }

    value <- xml2::xml_attr(node, "value")

    if (is.na(value) || !nzchar(value) || identical(value, "NA")) {
      return(NA_character_)
    }

    value
  }

  # Read text content.
  text_value <- function(id) {
    xpath <- paste0('//*[@id="', id, '"]')
    node <- xml2::xml_find_first(document, xpath)
    if (inherits(node, "xml_missing")) {
      return(NA_character_)
    }

    value <- xml2::xml_text(node)
    if (!nzchar(value)) {
      return(NA_character_)
    }
    value
  }

  # Read optional node text.
  node_text <- function(node) {
    if (inherits(node, "xml_missing")) {
      return(NA_character_)
    }
    value <- xml2::xml_text(node)
    if (!nzchar(value)) {
      return(NA_character_)
    }
    value
  }

  # Read the current value of a review control.
  control_value <- function(node) {
    name <- xml2::xml_name(node)
    if (name == "textarea") {
      return(xml2::xml_text(node))
    }
    if (name == "input") {
      return(xml2::xml_attr(node, "value"))
    }
    if (name == "a") {
      return(xml2::xml_text(node))
    }

    option <- xml2::xml_find_first(node, ".//option[@selected]")
    if (inherits(option, "xml_missing")) {
      option <- xml2::xml_find_first(node, ".//option[1]")
    }
    xml2::xml_attr(option, "value")
  }

  # Read the review rows.
  row_nodes <- xml2::xml_find_all(review_table, ".//tbody/tr")

  rows <- lapply(row_nodes, function(row) {
    # Read evidence.
    media_xpath <- './/a[contains(@class, "evidence-media-link")]'
    url_xpath <- './/a[contains(@class, "evidence-link")]'
    text_xpath <- './/div[contains(@class, "media-id")]'

    media_nodes <- xml2::xml_find_all(row, media_xpath)
    url_nodes <- xml2::xml_find_all(row, url_xpath)
    text_node <- xml2::xml_find_first(row, text_xpath)

    evidence_media_url <- paste(
      xml2::xml_attr(media_nodes, "href"),
      collapse = " | "
    )
    evidence_url <- paste(xml2::xml_attr(url_nodes, "href"), collapse = " | ")
    evidence_text <- xml2::xml_text(text_node)

    # Read descriptive fields.
    descriptive_nodes <- xml2::xml_find_all(
      row, ".//*[@data-field and @data-candidate]"
    )
    descriptive <- vapply(
      descriptive_nodes,
      function(node) xml2::xml_attr(node, "data-candidate"),
      character(1)
    )
    names(descriptive) <- vapply(
      descriptive_nodes,
      function(node) xml2::xml_attr(node, "data-field"),
      character(1)
    )
    reviewed_descriptive <- vapply(
      descriptive_nodes, control_value, character(1)
    )
    names(reviewed_descriptive) <- names(descriptive)

    # Read row-scoped context.
    context_nodes <- xml2::xml_find_all(row, ".//*[@data-context]")
    context <- vapply(
      context_nodes,
      function(node) xml2::xml_attr(node, "data-value"),
      character(1)
    )
    names(context) <- vapply(
      context_nodes,
      function(node) xml2::xml_attr(node, "data-context"),
      character(1)
    )

    # Read semantic assertions.
    assertion_nodes <- xml2::xml_find_all(
      row, ".//td[@data-column and @data-candidate]"
    )
    assertions <- vapply(
      assertion_nodes,
      function(node) xml2::xml_attr(node, "data-candidate"),
      character(1)
    )
    names(assertions) <- vapply(
      assertion_nodes,
      function(node) xml2::xml_attr(node, "data-column"),
      character(1)
    )

    reviewed_assertions <- vapply(assertion_nodes, function(node) {
      control <- xml2::xml_find_first(
        node, ".//input | .//textarea | .//select | .//a"
      )
      if (inherits(control, "xml_missing")) {
        return(xml2::xml_text(node))
      }
      control_value(control)
    }, character(1))
    names(reviewed_assertions) <- names(assertions)

    qualifications <- vapply(
      assertion_nodes,
      function(node) xml2::xml_attr(node, "data-qualification"),
      character(1)
    )
    names(qualifications) <- paste0(names(assertions), "_qualification")

    # Read row-level review state.
    comment_xpath <- './/td[contains(@class, "row-comment")]/textarea'
    row_comment <- node_text(xml2::xml_find_first(row, comment_xpath))

    candidate <- c(
      row_number = xml2::xml_attr(row, "data-row"),
      evidence_url = evidence_url,
      evidence_media_url = evidence_media_url,
      evidence_text = evidence_text,
      context,
      descriptive,
      assertions
    )

    reviewed <- c(
      row_number = xml2::xml_attr(row, "data-row"),
      evidence_url = evidence_url,
      evidence_media_url = evidence_media_url,
      evidence_text = evidence_text,
      context,
      reviewed_descriptive,
      reviewed_assertions,
      qualifications,
      finalised = xml2::xml_attr(row, "data-finalised"),
      outcome = xml2::xml_attr(row, "data-outcome"),
      row_comment = row_comment
    )

    list(candidate = candidate, reviewed = reviewed)
  })

  # Assemble candidate and reviewed data.
  candidate <- as.data.frame(
    do.call(rbind, lapply(rows, `[[`, "candidate")),
    stringsAsFactors = FALSE
  )
  reviewed <- as.data.frame(
    do.call(rbind, lapply(rows, `[[`, "reviewed")),
    stringsAsFactors = FALSE
  )

  # Restore field types and missing values.
  candidate$row_number <- as.integer(candidate$row_number)
  reviewed$row_number <- as.integer(reviewed$row_number)
  reviewed$finalised <- reviewed$finalised == "true"

  candidate$evidence_url[candidate$evidence_url == ""] <- NA_character_
  reviewed$evidence_url[reviewed$evidence_url == ""] <- NA_character_

  # Handle cases where there are no evidence media URLs in the betwixt-html
  no_media <- candidate$evidence_media_url == ""
  candidate$evidence_media_url[no_media] <- NA_character_
  reviewed$evidence_media_url[no_media] <- NA_character_

  # Handle cases where there are no evidence columns in the betwixt-html
  no_text <- candidate$evidence_text == ""
  candidate$evidence_text[no_text] <- NA_character_
  reviewed$evidence_text[no_text] <- NA_character_

  # Read review metadata.
  title_xpath <- paste0(
    '//header[contains(concat(" ", normalize-space(@class), " "), ',
    '" review-header ")]/h1'
  )
  description_xpath <- paste0(
    '//header[contains(concat(" ", normalize-space(@class), " "), ',
    '" review-header ")]/p[contains(concat(" ", normalize-space(@class), ',
    '" "), " instructions ")]'
  )

  metadata <- list(
    title = xml2::xml_text(xml2::xml_find_first(document, title_xpath)),
    description = xml2::xml_text(
      xml2::xml_find_first(document, description_xpath)
    ),
    project_id = input_value("project-id"),
    sequence = as.integer(input_value("review-sequence")),
    status = input_value("review-status")
  )

  # Re-assemble review provenance.
  # Read provenance.
  provenance <- list(
    data_manager = candidate_provenance_value("data_manager"),
    data_manager_iri = candidate_provenance_value("data_manager_iri"),
    data_manager_email = candidate_provenance_value("data_manager_email"),
    project_id = candidate_provenance_value("project_id"),
    generated_at = candidate_provenance_value("generated_at"),
    software_agent = candidate_provenance_value("software_agent"),
    software_version = candidate_provenance_value("software_version"),
    original_filename = input_value("original-filename"),
    original_created_at = input_value("original-created-at"),
    filename = input_value("review-filename"),
    reviewer = input_value("reviewer-name"),
    reviewer_email = input_value("reviewer-email"),
    reviewer_iri = input_value("reviewer-iri"),
    started_at = input_value("review-started-at"),
    saved_at = input_value("review-last-saved-at"),
    ended_at = input_value("review-ended-at"),
    review_comment = text_value("review-comment")
  )

  list(
    metadata = metadata,
    provenance = provenance,
    candidate = candidate,
    reviewed = reviewed
  )
}
