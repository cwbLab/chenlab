#' Wrapper of rbindlist
#'
#' @description
#' A wrapper around data.table::rbindlist() designed to handle extremely large lists safely by reducing memory pressure and preventing memory overflow.
#'
#' @param l A list containing data.table, data.frame or list objects.
#' @param n The maximum number of elements in each sublist. The original list will be split based on this value.
#' @param max_chunks The number of chunks to split the original list into. If specified, n is ignored.
#' @param fill TRUE fills missing columns with NAs, or NULL for missing list columns. By default FALSE.
#' @param use.names TRUE binds by matching column name, FALSE by position.
#' @param idcol Creates a column in the result showing which list item those rows came from. TRUE names this column ".id". idcol="file" names this column "file". If the input list has names, those names are the values placed in this id column, otherwise the values are an integer vector 1:length(l).
#' @param threads Number of cores used for parallel computation. By default, the maximum computing resources are used.
#'
#' @returns
#' An unkeyed data.table containing a concatenation of all the items passed in.
#'
#' @export
#'
rbindlist_n <- function(l, n = 10000, max_chunks = NULL, fill = FALSE, use.names = TRUE, idcol = NULL,  threads = NULL ) {
  #
  library(parallel)
  library(data.table)
  library(dplyr)

  #detect
  stopifnot(is.list(l))
  if (is.null(threads)) { threads = parallel::detectCores() }

  ######
  len <- length(l)

  #n
  if( !is.null(max_chunks) ){  n = max( 1, ceiling(len / max_chunks) )    }

  #
  if (len <= n){
    result <- data.table::rbindlist(l, fill = fill, use.names = use.names, idcol = idcol)
  }else{
    #
    starts <- seq(1, len, by = n)
    ends <- pmin(starts + n - 1, len)

    #
    intermediate_list <- mclapply(seq_along(starts), function(i){
      data.table::rbindlist( l[starts[i]:ends[i]] , fill = fill, use.names = use.names, idcol = idcol)
    } , mc.cores = threads )

    #
    result <- data.table::rbindlist(intermediate_list, fill = fill, use.names = use.names, idcol = idcol)
  }
  ######
  return(result)
}
