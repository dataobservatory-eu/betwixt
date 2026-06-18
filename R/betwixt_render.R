#' Render claims as an HTML review packet
#'
#' Renders a `claim_df` using a Mustache template.
#' By default the rendered HTML is returned as a character
#' vector. Alternatively, it can be written directly to an
#' open connection.
#'
#' @param claim A `claim_df`.
#' @param con Optional writable connection. If `NULL`,
#'   the rendered HTML is returned as a character vector.
#'
#' @return
#' If `con = NULL`, a character vector containing rendered
#' HTML.
#'
#' If `con` is supplied, the HTML is written to the
#' connection and the connection is returned invisibly.
#'
#' @examples
#' ad_gdp <- claim(
#'   scope = "country=AD;year=2023",
#'   subject = "country",
#'   predicate = "GDP",
#'   value = "3.73 billion EUR"
#' )
#'
#' html <- betwixt_render(ad_gdp)
#'
#' @importFrom whisker whisker.render
#' @export
betwixt_render <- function(
    claim,
    con = NULL,
    title = "Betwixt Review"
) {
  context <- claim_context(claim)
  context$title <- title

  default_template <- get_template("default")

  html <- whisker::whisker.render(
    default_template,
    context
  )

  if (is.null(con)) {
    return(html)
  }

  cat(html, "\n", file = con, sep = "")
  invisible(con)
}
