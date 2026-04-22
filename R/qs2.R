
#' wb.qsave
#'
#' @description
#' Wraps the `qs2::qs_save` function to enable fast serialization of either the entire R environment or a set of specified objects to a local file, with significantly improved performance compared to the base R functions `save`, `save.image` and `saveRDS`.
#'
#' @param filename The file name/path.
#'
#' It is recommended to use the `.qrds` extension when saving a single object, and `.qdata` for all other cases. The suffix has no strict meaning and is only intended to help users choose the appropriate loading method with `wb.qread`.
#'
#' @param ... By default, all objects in the R environment are saved. Alternatively, users can specify multiple objects, in which case only the selected objects will be saved to the local file.
#' @param envir The environment in which the objects are located.
#' @param compress_level Consistent with the `qs2::qs_save`.
#' @param shuffle Consistent with the `qs2::qs_save`.
#' @param nthreads Consistent with the `qs2::qs_save`, a single thread is used by default. It is recommended not to use too many cores, as excessive parallelism may increase thread scheduling overhead and lead to negative performance gains.
#'
#' @returns
#' NULL.
#'
#' @examples
#' x1 <- data.frame(int = sample(1e3, replace=TRUE), num = rnorm(1e3) )
#' x2 <- data.frame(int = sample(1e4, replace=TRUE), num = rnorm(1e4) )
#' x3 <- data.frame(int = sample(1e5, replace=TRUE), num = rnorm(1e5) )
#'
#' #1.entire R environment
#' wb.qsave( 'entire_R_environment.qdata' )
#' #wb.qread( 'entire_R_environment.qdata' )
#'
#' #2.x1 and x2
#' wb.qsave( 'x1_x2.qdata' , x1, x2 )
#' #wb.qread( 'x1_x2.qdata' )
#'
#' #3.x1
#' wb.qsave(  'x1.qrds' , x1 )
#' #wb.qread( 'x1.qrds' , return = F )
#' #read_x1 <- wb.qread( 'x1.qrds' , return = T )
#'
#' @export
#'
#'
wb.qsave <- function(filename, ... , envir = .GlobalEnv,
                     compress_level = qs2::qopt( "compress_level" ),
                     shuffle = qs2::qopt("shuffle"), nthreads = 1
){
  #
  suppressMessages(library(qs2))
  #
  threads <- nthreads

  #
  dots <- substitute(list(...))[-1]

  # Q1
  if (length(dots) == 0) {
    #
    obj_names <- ls(envir = envir)
    obj_list  <- mget(obj_names, envir = envir)

    qs2::qs_save(object = obj_list, file = filename,
              compress_level = compress_level ,
              shuffle = shuffle ,
              nthreads = threads )

    message(wb.log_time_title() , "Saved entire environment: ", length(obj_names), " objects → ", filename)

  } else {
    # Q2
    obj_names <- sapply(dots, deparse)
    obj_list  <- mget(obj_names, envir = envir)

    qs2::qs_save(object = obj_list, file = filename,
                 compress_level = compress_level ,
                 shuffle = shuffle ,
                 nthreads = threads )

    message(wb.log_time_title() , "Saved objects: ", paste(obj_names, collapse = ", "), " → ", filename)
  }
  #
  return(invisible(NULL))
}



#' wb.qread
#'
#' @description
#' A wrapper around `qs2::qs_read` for loading objects saved with `wb.qsave` into the R environment.
#'
#' @param filename The file name/path.
#' @param envir The target environment into which the variables will be loaded.
#' @param return Return the saved objects as variables instead of loading them into the target environment.
#'
#' If a single object is saved, it is returned directly; if multiple objects are saved, a list containing these objects is returned.
#' @param nthreads Consistent with `qs2::qs_read`, a single thread is used by default. It is recommended not to use too many cores, as excessive parallelism may increase thread scheduling overhead and lead to negative performance gains.
#'
#' @returns
#' NULL.
#'
#' @export
#'
wb.qread <- function( filename , envir = .GlobalEnv , return = F , nthreads = 1  ){
  #
  threads <- nthreads

  #
  wb_qload_obj_list <- qs2::qs_read( file = filename,  nthreads =  threads)
  message(wb.log_time_title() ,"Loaded ", length(wb_qload_obj_list), " objects from ", filename)

  #
  if( !return ){
    list2env(wb_qload_obj_list, envir = envir)
    return(invisible(NULL))
  }else{
    if( length( wb_qload_obj_list ) == 1  ){
      return(  wb_qload_obj_list[[1]]  )
    }else{
      return( wb_qload_obj_list )
    }
  #
  }
}

