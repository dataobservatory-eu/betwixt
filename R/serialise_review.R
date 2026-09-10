#' Serialise a Betwixt review
#'
#' Serialises a reviewed Betwixt dataset as RDF Turtle.
#'
#' @param review A Betwixt review object returned by [read_review()].
#' @param prefix Base IRI for generated resources.
#' @param filename Filename identifying the serialised dataset.
#' @param title Optional dataset title.
#' @param description Optional dataset description.
#' @param reviewer_iri Optional persistent IRI identifying the reviewer.
#'
#' @examples
#' path <- system.file(
#'   "examples",
#'   "muis-garments-review_1-finalised.html",
#'   package = "betwixt"
#' )
#' review <- read_review(path)
#'
#' ttl <- serialise_review(
#'   review,
#'   prefix = "https://usebetwixt.com/examples/",
#'   filename = "muis-garments-review.ttl",
#'   title = "MuIS garment terminology review",
#'   description = "Reviewed terminology assertions for three MuIS garments."
#' )
#' cat(ttl)
#'
#' @return A character string containing RDF Turtle.
#' @export
serialise_review <- function(
  review,
  prefix,
  filename,
  title = NULL,
  description = NULL,
  reviewer_iri = NULL
) {
  x <- project_review_long(review)

  paste(
    serialise_prefixes(),
    serialise_dataset(prefix, filename, title, description,
      n_assertions = nrow(x)
    ),
    serialise_activity(review, prefix, filename, reviewer_iri),
    serialise_reviewer(review, prefix, filename, reviewer_iri),
    serialise_assertions(x, prefix, filename),
    sep = "\n\n"
  )
}


#' @rdname serialise_review
#' @export
serialize_review <- serialise_review

# Prefixes --------------------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_prefixes <- function() {
  paste(
    "@prefix btx: <https://usebetwixt.com/ns/> .",
    "@prefix schema: <https://schema.org/> .",
    "@prefix prov: <http://www.w3.org/ns/prov#> .",
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
    "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
    sep = "\n"
  )
}


# Dataset ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_dataset <- function(
  prefix,
  filename,
  title = NULL,
  description = NULL,
  n_assertions = 0L
) {
  iri <- paste0(prefix, filename)
  stem <- tools::file_path_sans_ext(filename)
  activity_iri <- paste0(prefix, stem, "/activity")

  lines <- c(
    turtle_iri(iri),
    "    a schema:Dataset"
  )

  if (!is.null(title)) {
    lines <- c(
      lines,
      paste0("    ; schema:name ", turtle_literal(title))
    )
  }

  if (!is.null(description)) {
    lines <- c(
      lines,
      paste0("    ; schema:description ", turtle_literal(description))
    )
  }

  lines <- c(
    lines,
    paste0("    ; prov:wasGeneratedBy ", turtle_iri(activity_iri))
  )

  if (n_assertions > 0L) {
    iris <- vapply(
      seq_len(n_assertions),
      function(i) turtle_iri(paste0(prefix, stem, "/assertion/", i)),
      character(1)
    )
    lines <- c(
      lines,
      paste0("    ; btx:assertion ", paste(iris, collapse = ", "))
    )
  }

  paste0(paste(lines, collapse = "\n"), " .")
}
# Review activity -------------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_activity <- function(
  review,
  prefix,
  filename,
  reviewer_iri = NULL
) {
  stem <- tools::file_path_sans_ext(filename)
  iri <- paste0(prefix, stem, "/activity")
  provenance <- review$provenance

  if (is.null(reviewer_iri)) {
    reviewer_iri <- paste0(prefix, stem, "/reviewer")
  }

  lines <- c(
    turtle_iri(iri),
    "    a prov:Activity",
    paste0("    ; prov:wasAssociatedWith ", turtle_iri(reviewer_iri))
  )

  if (!is.null(provenance$started_at)) {
    lines <- c(
      lines,
      paste0(
        "    ; prov:startedAtTime ",
        turtle_literal(provenance$started_at),
        "^^xsd:dateTime"
      )
    )
  }

  if (!is.null(provenance$ended_at)) {
    lines <- c(
      lines,
      paste0(
        "    ; prov:endedAtTime ",
        turtle_literal(provenance$ended_at),
        "^^xsd:dateTime"
      )
    )
  }

  paste0(paste(lines, collapse = "\n"), " .")
}

# Reviewer --------------------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_reviewer <- function(
  review,
  prefix,
  filename,
  reviewer_iri = NULL
) {
  reviewer <- review$provenance$reviewer

  if (is.null(reviewer_iri)) {
    stem <- tools::file_path_sans_ext(filename)
    reviewer_iri <- paste0(prefix, stem, "/reviewer")
  }

  paste(
    turtle_iri(reviewer_iri),
    "    a prov:Agent",
    paste0("    ; rdfs:label ", turtle_literal(reviewer), " ."),
    sep = "\n"
  )
}


# Assertions ------------------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_assertions <- function(x, prefix, filename) {
  vapply(
    seq_len(nrow(x)),
    function(i) serialise_assertion(x[i, ], prefix, filename),
    character(1)
  ) |>
    paste(collapse = "\n\n")
}


# Assertion -------------------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_assertion <- function(x, prefix, filename) {
  stem <- tools::file_path_sans_ext(filename)
  iri <- paste0(prefix, stem, "/assertion/", x$assertion_number)
  status <- paste0(
    toupper(substr(x$status, 1, 1)),
    substr(x$status, 2, nchar(x$status))
  )

  paste(
    turtle_iri(iri),
    "    a btx:Assertion",
    paste0("    ; btx:rowNumber ", x$row_number),
    paste0("    ; btx:subject ", turtle_literal(x$subject)),
    paste0("    ; btx:predicate ", turtle_literal(x$predicate)),
    paste0("    ; btx:value ", turtle_literal(x$value)),
    paste0("    ; btx:status btx:", status, " ."),
    sep = "\n"
  )
}


# Turtle utilities ------------------------------------------------------------

#' @keywords internal
#' @noRd
turtle_literal <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  paste0('"', x, '"')
}

#' @keywords internal
#' @noRd
turtle_iri <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("IRI must be one non-empty character string.", call. = FALSE)
  }

  if (!grepl("^[A-Za-z][A-Za-z0-9+.-]*:", x)) {
    stop("IRI must be absolute.", call. = FALSE)
  }

  if (grepl('[<>"{}|^`\\\\[:space:]]', x)) {
    stop("IRI contains characters not permitted in Turtle IRIs.", call. = FALSE)
  }

  paste0("<", x, ">")
}
