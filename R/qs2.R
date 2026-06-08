
#' w.qsave
#'
#' @description
#' Wraps the `qs2::qs_save` function to enable fast serialization of either the entire R environment or a set of specified objects to a local file, with significantly improved performance compared to the base R functions `save`, `save.image` and `saveRDS`.
#'
#' @param filename The file name/path. The file name does not affect the behavior of the function.
#'
#' We recommend using the `.qrds` extension when saving a single object, and `.qdata` for all other cases. The suffix has no strict meaning and is only intended to help users choose the appropriate loading method with `w.qread`.
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
#' w.qsave( 'entire_R_environment.qdata' )
#' #w.qread( 'entire_R_environment.qdata' )
#'
#' #2.x1 and x2
#' w.qsave( 'x1_x2.qdata' , x1, x2 )
#' #w.qread( 'x1_x2.qdata' )    #load x1 and x2 into the R environment
#' #x1_x2_list <- w.qread( 'x1_x2.qdata' , return = T )    #return a list object
#'
#' #3.x1
#' w.qsave(  'x1.qrds' , x1 )
#' #w.qread( 'x1.qrds' , return = F )    #load x1 into the R environment
#' #x1.copy <- w.qread( 'x1.qrds' , return = T )    #assign x1 to a new R object (x1.copy).
#'
#' @export
#'
#'
w.qsave <- function(filename, ... , envir = base::parent.frame(),
                     compress_level = qs2::qopt( "compress_level" ),
                     shuffle = qs2::qopt("shuffle"), nthreads = 1
){
  #
  suppressMessages(library(qs2))
  message( w.log_time_title() , w.log_text_coloured( s.c = 's' ) )
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

    message(w.log_time_title() , "Saved entire environment: ", length(obj_names), " objects → ", filename)

  } else {
    # Q2
    obj_names <- sapply(dots, deparse)
    obj_list  <- mget(obj_names, envir = envir)

    qs2::qs_save(object = obj_list, file = filename,
                 compress_level = compress_level ,
                 shuffle = shuffle ,
                 nthreads = threads )

    message(w.log_time_title() , "Saved objects: ", paste(obj_names, collapse = ", "), " → ", filename)
  }
  #
  return(invisible(NULL))
}



#' w.qread
#'
#' @description
#' A wrapper around `qs2::qs_read` for loading objects saved with `w.qsave` into the R environment.
#'
#' @param filename The file name/path.
#' @param return Return the saved objects as variables instead of loading them into the target environment. Default: FALSE.
#'
#' If True, return the object directly when only one object is saved, similar to `base::readRDS`; otherwise return a list of all objects.
#' @param envir The target environment into which the variables will be loaded. This argument is ignored when `return` is `TRUE`.
#' @param delete Whether to delete the local file after reading is completed. Default: FALSE.
#' @param nthreads Consistent with `qs2::qs_read`, a single thread is used by default. It is recommended not to use too many cores, as excessive parallelism may increase thread scheduling overhead and lead to negative performance gains.
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
#' w.qsave( 'entire_R_environment.qdata' )
#' #w.qread( 'entire_R_environment.qdata' )
#'
#' #2.x1 and x2
#' w.qsave( 'x1_x2.qdata' , x1, x2 )
#' #w.qread( 'x1_x2.qdata' )    #load x1 and x2 into the R environment
#' #x1_x2_list <- w.qread( 'x1_x2.qdata' , return = T )    #return a list object
#'
#' #3.x1
#' w.qsave(  'x1.qrds' , x1 )
#' #w.qread( 'x1.qrds' , return = F )    #load x1 into the R environment
#' #x1.copy <- w.qread( 'x1.qrds' , return = T )    #assign x1 to a new R object (x1.copy).
#'
#' @export
#'
w.qread <- function( filename , return = FALSE ,
                     envir = base::parent.frame() , delete = FALSE , nthreads = 1  ){
  #
  message( w.log_time_title() , w.log_text_coloured( s.c = 's' ) )
  #
  threads <- nthreads

  #
  w_qload_obj_list <- qs2::qs_read( file = filename,  nthreads =  threads)
  message(w.log_time_title() ,"Loaded ", length(w_qload_obj_list), " objects from ", filename)

  #
  if( delete ){  temp <- base::file.remove( filename )  }
  #
  if( !return ){
    list2env(w_qload_obj_list, envir = envir)
    return(invisible(NULL))
  }else{
    if( length( w_qload_obj_list ) == 1  ){
      return(  w_qload_obj_list[[1]]  )
    }else{
      return( w_qload_obj_list )
    }
  #
  }
}

