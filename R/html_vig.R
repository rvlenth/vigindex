

#' Convert to an HTML vignette with extras for indexes
#'
#' This is a variant of \code{\link[rmarkdown]{html_vignette}} that includes
#' extra style definitions to format the index in the same way, but with
#' bullets suppressed and visible link coloring for \code{<code>}-tagged
#' text.
#'
#' @param css name(s) of CSS file(s) to include. If \code{"builtin"}, it will default
#'   to the stylesheet used by \code{rmarkdown::html_vignette}, plus the
#'   extra styles in
#'   \code{system.file("resources", "vigi-extras.css", package = "vigindex")}.
#'   The latter may be useful for adding appropriate bullet-suppression and
#'   link-coloring to another style.
#' @param ... additional arguments passed to \code{\link[rmarkdown]{html_vignette}}
#' @export
html_vigindex = function (css = "builtin",...) {
    if (css == "builtin")
        css = c(system.file("rmarkdown", "templates", "html_vignette" ,"resources",
                            "vignette.css", package = "rmarkdown"),
                system.file("resources", "vigi-extras.css", package = "vigindex"))
    rmarkdown::html_vignette(..., css = css)
}
