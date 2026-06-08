#' Quietly load multiple R packages
#'
#' @description
#'
#' Loads multiple R packages quietly while suppressing messages and warnings.
#' The function supports flexible input formats, including unquoted package names,
#' character strings, and nested \code{c()} calls. Packages are attached to the
#' search path using \code{require()}.
#'
#' @param ... Package names to load. Supports:
#'   \itemize{
#'     \item Unquoted package names (e.g., \code{dplyr})
#'     \item Character strings (e.g., \code{"dplyr"})
#'     \item Nested \code{c()} combinations (e.g., \code{c(dplyr, data.table)} or \code{c("dplyr", "data.table")})
#'   }
#'
#' @returns A named invisible list with two elements:
#' \itemize{
#'   \item \code{success}: character vector of successfully loaded packages
#'   \item \code{failure}: character vector of packages that failed to load
#' }
#'
#' @examples
#' temp <- w.library_quiet( c("data.table" , dplyr) , "ggplot2"  )
#'
#' @export
#'
w.library_quiet <- function(...) {

  #
  pkgs <- substitute(list(...))[-1]

  # flatten
  flatten_pkgs <- function(x) {

    if (is.symbol(x)) {
      return(as.character(x))
    }

    if (is.character(x)) {
      return(x)
    }

    if (is.call(x) && as.character(x[[1]]) == "c") {
      return(base::unlist(lapply(x[-1], flatten_pkgs)))
    }

    if (is.call(x)) {
      return(as.character(x))
    }

    return(NULL)
  }
  #
  pkgs <- base::unlist(  base::lapply(pkgs, flatten_pkgs))
  pkgs <- base::unique(as.character(pkgs))

  #
  loaded <- base::vapply(
    pkgs,
    function(pkg) {

      ok <- base::suppressWarnings(
        base::suppressMessages(
          require(
            pkg,
            character.only = TRUE,
            quietly = TRUE
          )
        )
      )
      #
      return(ok)
    },
    logical(1)
  )

  #
  failure = pkgs[!loaded]
  if( length(failure) != 0 ){
    warning("Package not found: \n" , paste( failure , collapse  = '\n' )  )
  }
  #
  return(
    invisible(
      list(
        success = pkgs[loaded],
        failure = failure
      )
    )
  )
  #
}
