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
  candidate <- x[x$plane == "candidate", ]
  reviewed <- x[x$plane == "reviewed", ]

  paste(
    serialise_prefixes(),
    serialise_dataset(
      candidate, prefix, filename, "candidate",
      title, description,
      activity = "candidate-activity"
    ),
    serialise_dataset(
      reviewed, prefix, filename, "reviewed-1",
      title, description,
      activity = "activity",
      derived_from = "candidate"
    ),
    serialise_candidate_activity(review, prefix, filename),
    serialise_data_manager(review, prefix, filename),
    serialise_activity(review, prefix, filename, reviewer_iri),
    serialise_reviewer(review, prefix, filename, reviewer_iri),
    serialise_assertions(candidate, prefix, filename, "candidate"),
    serialise_assertions(reviewed, prefix, filename, "reviewed-1"),
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
  x,
  prefix,
  filename,
  dataset,
  title = NULL,
  description = NULL,
  activity,
  derived_from = NULL
) {
  stem <- tools::file_path_sans_ext(filename)
  base <- paste0(prefix, stem, "/")
  iri <- paste0(base, dataset)

  lines <- c(
    turtle_iri(iri),
    "    a schema:Dataset"
  )

  if (!is.null(title)) {
    lines <- c(lines, paste0("    ; schema:name ", turtle_literal(title)))
  }

  if (!is.null(description)) {
    lines <- c(
      lines,
      paste0("    ; schema:description ", turtle_literal(description))
    )
  }

  lines <- c(
    lines,
    paste0("    ; prov:wasGeneratedBy ", turtle_iri(paste0(base, activity)))
  )

  if (!is.null(derived_from)) {
    lines <- c(
      lines,
      paste0(
        "    ; prov:wasDerivedFrom ",
        turtle_iri(paste0(base, derived_from))
      )
    )
  }

  if (nrow(x) > 0L) {
    iris <- vapply(
      seq_len(nrow(x)),
      function(i) {
        turtle_iri(paste0(base, dataset, "/assertion/", i))
      },
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
  candidate_activity_iri <- paste0(prefix, stem, "/candidate-activity")
  provenance <- review$provenance

  if (is.null(reviewer_iri)) {
    reviewer_iri <- review$provenance$reviewer_iri
  }

  if (is.na(reviewer_iri) || !nzchar(reviewer_iri)) {
    reviewer_iri <- paste0(prefix, stem, "/reviewer")
  }

  lines <- c(
    turtle_iri(iri),
    "    a prov:Activity",
    paste0("    ; prov:wasAssociatedWith ", turtle_iri(reviewer_iri)),
    paste0("    ; prov:wasInformedBy ", turtle_iri(candidate_activity_iri))
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

# Data manager ---------------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_data_manager <- function(review, prefix, filename) {
  provenance <- review$provenance
  stem <- tools::file_path_sans_ext(filename)

  iri <- provenance$data_manager_iri
  if (is.na(iri) || !nzchar(iri)) {
    iri <- paste0(prefix, stem, "/data-manager")
  }

  paste(
    turtle_iri(iri),
    "    a prov:Agent",
    paste0(
      "    ; rdfs:label ",
      turtle_literal(provenance$data_manager),
      " ."
    ),
    sep = "\n"
  )
}

# Candidate activity ---------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_candidate_activity <- function(review, prefix, filename) {
  provenance <- review$provenance
  stem <- tools::file_path_sans_ext(filename)
  iri <- paste0(prefix, stem, "/candidate-activity")

  manager_iri <- provenance$data_manager_iri
  if (is.na(manager_iri) || !nzchar(manager_iri)) {
    manager_iri <- paste0(prefix, stem, "/data-manager")
  }

  lines <- c(
    turtle_iri(iri),
    "    a prov:Activity",
    paste0(
      "    ; prov:wasAssociatedWith ",
      turtle_iri(manager_iri)
    )
  )

  if (!is.na(provenance$generated_at)) {
    lines <- c(
      lines,
      paste0(
        "    ; prov:endedAtTime ",
        turtle_literal(provenance$generated_at),
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
    reviewer_iri <- review$provenance$reviewer_iri
  }

  if (is.na(reviewer_iri) || !nzchar(reviewer_iri)) {
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
serialise_assertions <- function(x, prefix, filename, dataset) {
  vapply(
    seq_len(nrow(x)),
    function(i) {
      serialise_assertion(x[i, ], prefix, filename, dataset, i)
    },
    character(1)
  ) |>
    paste(collapse = "\n\n")
}


# Assertion -------------------------------------------------------------------

#' @keywords internal
#' @noRd
serialise_assertion <- function(x, prefix, filename, dataset, number) {
  stem <- tools::file_path_sans_ext(filename)
  iri <- paste0(prefix, stem, "/", dataset, "/assertion/", number)

  lines <- c(
    turtle_iri(iri),
    "    a btx:Assertion",
    paste0("    ; btx:rowNumber ", x$row_number),
    paste0("    ; btx:subject ", turtle_literal(x$subject)),
    paste0("    ; btx:predicate ", turtle_literal(x$predicate)),
    paste0("    ; btx:value ", turtle_literal(x$value))
  )

  if (dataset != "candidate") {
    status <- paste0(
      toupper(substr(x$status, 1, 1)),
      substr(x$status, 2, nchar(x$status))
    )
    lines <- c(lines, paste0("    ; btx:status btx:", status))
  }

  paste0(paste(lines, collapse = "\n"), " .")
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
