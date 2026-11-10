#' Add a candidate column to a Betwixt candidate dataset
#'
#' @description
#' Adds a named candidate column to a Betwixt candidate dataset. Candidate
#' columns are stored as triplets consisting of a candidate value, an optional
#' controlled range, and an optional semantic definition.
#'
#' @section Special candidate-range values:
#' Candidate ranges may contain reserved tokens that have
#' special meanings in the review interface:
#'
#' \describe{
#'   \item{`<other>`}{
#'     Allows the reviewer to enter a value not included in
#'     the proposed candidate range.
#'     For backward compatibility, `Other…` (Unicode ellipsis)
#'     and `Other...` (three periods) are also recognised.
#'   }
#'   \item{`<unknown>`}{
#'     Indicates that an applicable value may exist, but
#'     its value is unknown.
#'   }
#'   \item{`<no-value>`}{
#'     Indicates that no applicable value is possible.
#'   }
#' }
#'
#' Reserved tokens are recognised only when they appear
#' as complete range entries. Ordinary strings such as
#' `Other`, `Unknown`, and `None` retain their literal meanings.
#'
#' Special values are optional and are not automatically
#' appended to candidate ranges.
#'
#' @param x A data frame or tibble representing a Betwixt candidate dataset.
#'
#' @param name A character string giving the name of the candidate column.
#'
#' @param value A vector containing the candidate values to be reviewed.
#'   Its length must be compatible with the number of rows in `x`.
#'
#' @param range A character vector containing the admissible or suggested
#'   range of candidate values. Use `NA_character_` when no controlled range
#'   is supplied. Defaults to `NA_character_`.
#'
#' @param definition A character vector containing identifiers or URLs
#'   defining the semantic property represented by the candidate column.
#'   Use `NA_character_` when no definition is supplied. Defaults to
#'   `NA_character_`.
#'
#' @return
#' A tibble with three additional columns: `name`, `name_range`, and
#' `name_definition`, where `name` is the supplied candidate-column name.
#'
#' @details
#' `add_candidate_column()` does not modify existing candidate columns.
#' Candidate columns use semantic names supplied explicitly through `name`.
#'
#' Contextual, non-reviewable information does not need to be added with this
#' function and can be appended with ordinary data manipulation operations,
#' for example `dplyr::mutate(context_1 = held_by)`.
#'
#' @examples
#' candidates <- tibble::tibble(
#'   row_id = 1:2,
#'   input_description = c("image-1", "image-2")
#' )
#'
#' candidates <- add_candidate_column(
#'   candidates,
#'   name = "instance_of",
#'   value = c("farmhouse", "bed"),
#'   range = rep("farmhouse | bed | Other...", 2),
#'   definition = rep(
#'     "https://www.wikidata.org/wiki/Property:P31",
#'     2
#'   )
#' )
#'
#' names(candidates)
#'
#' @importFrom dplyr mutate
#' @export
add_candidate_column <- function(
  x,
  name,
  value,
  range = NA_character_,
  definition = NA_character_
) {
  x[[name]] <- value
  x[[paste0(name, "_range")]] <- range
  x[[paste0(name, "_definition")]] <- definition
  x
}
