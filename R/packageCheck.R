#' Check package
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
w.packageCheck <- function( package.name, method = 'I', interactive.install = TRUE, check.only = FALSE ){

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

