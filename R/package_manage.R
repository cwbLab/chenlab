#' Install package
#'
#' @description
#' Check whether a package is installed and optionally install it automatically using a specified installation method.
#'
#' @param package.name Package name.
#' @param method Method used for automatic installation. Can be "I", "B", or a character string.
#'
#' 1. If "I", the package will be installed using utils::install.packages().
#'
#' 2. If "B", the package will be installed using BiocManager::install().
#'
#' 3. A custom installation command can also be provided as a character string, e.g. "utils::install.packages('dplyr')".
#' @param interactive.install Whether to provide an interactive prompt asking the user to confirm before starting the installation.
#' @param check.only Whether to perform package checking only without automatic installation.
#'
#' @returns
#'
#' 1. TRUE: Returned when the package check passes or the package is installed successfully.
#'
#' 2. FALSE: Returned when the package check fails or the installation is unsuccessful.
#'
#' @export
#'
ww.package_install <- function( package.name, method = 'I', interactive.install = TRUE, check.only = FALSE ){

  ######check.only
  if( check.only ){
    return(invisible(requireNamespace(package.name, quietly = TRUE)))
  }

  #######
  if (requireNamespace(package.name, quietly = TRUE)){
    return(invisible(TRUE))
  }

  #######not
  if( method == 'I' ){
    method = sprintf("utils::install.packages( '%s' )", package.name)
  }else if( method == 'B' ){
    if( !requireNamespace("BiocManager", quietly = TRUE) ){  utils::install.packages("BiocManager")  }
    method = sprintf("BiocManager::install( '%s' )", package.name)
  }

  message(sprintf("❗ Package '%s' is not installed.", package.name))
  message(sprintf("✅️ Recommended way to install: %s.", method))

  #
  to_install <- TRUE
  if( interactive.install & base::interactive() ){
    choice <- utils::menu(c("Yes", "No"), title = sprintf("⚠️ Do you want to install '%s' using recommended way?", package.name))
    to_install <- ( choice == 1 )
  }
  if(to_install){
    # install
    install_result <- tryCatch({
      eval(parse(text = method))
      TRUE
    }, error = function(e) {
      FALSE
    })
  }

  ######check
  if (requireNamespace(package.name, quietly = TRUE)) {
    message(sprintf("✅️ Package '%s' was successfully installed automatically.", package.name))
    return(invisible(TRUE))
  }else{
    message(sprintf("❗ Package '%s' is required but not installed. Please install manually using: %s.", package.name, method))
    return(invisible(FALSE))
  }
  #
}







#' Quietly load and check multiple R packages
#'
#' @description
#'
#' Load and check multiple R packages quietly while suppressing messages and warnings.
#' The function supports flexible input formats, including unquoted package names,
#' character strings, and nested \code{c()} calls. Packages are attached to the
#' search path using \code{require()}.
#'
#' The function does not stop when some packages fail to load; it continues running and finally returns the names of both successfully loaded and failed packages.
#'
#' @param ... Package names to load. Supports:
#'   \itemize{
#'     \item Unquoted package names (e.g., \code{dplyr})
#'     \item Character strings (e.g., \code{"dplyr"})
#'     \item Nested \code{c()} combinations (e.g., \code{c(dplyr, data.table)} or \code{c("dplyr", "data.table")})
#'   }
#' @param quiet If TRUE, all messages and warnings will be suppressed when loading R packages.
#'
#' @returns A named invisible list with two elements:
#' \itemize{
#'   \item \code{success}: character vector of successfully loaded packages
#'   \item \code{failure}: character vector of packages that failed to load
#' }
#'
#' @examples
#' temp <- ww.package_library( c("data.table" , dplyr) , "ggplot2"  )
#'
#' @export
#'
ww.package_library <- function(..., quiet = T) {

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
  pkgs <- base::unlist(  base::lapply(pkgs, flatten_pkgs) )
  pkgs <- base::unique(as.character(pkgs))

  #
  if( quiet ){
    loaded <- base::vapply(
      pkgs,
      function(pkg){
        #
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
  }else{
    loaded <- base::vapply(
      pkgs,
      function(pkg) {
        #
        ok <- require(
          pkg,
          character.only = TRUE,
          quietly = TRUE
        )
        #
        return(ok)
      },
      logical(1)
    )
  }

  #
  failure = pkgs[!loaded]
  if( length(failure) != 0 ){
    warning("Packages not found: \n" , paste( failure , collapse  = '\n' )  )
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
