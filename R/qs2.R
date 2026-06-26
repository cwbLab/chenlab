
#' w.qsave
#'
#' @description
#' Wraps the `save` function across different versions of the `qs` package to enable fast serialization of either the entire R environment or a set of specified objects to a local file, with significantly improved performance compared to the base R functions `save`, `save.image` and `saveRDS`.
#'
#' @param filename The file name/path. The file name does not affect the behavior of the function.
#'
#' We recommend using the `.qrds` extension when saving a single object, and `.qdata` for all other cases. The suffix has no strict meaning and is only intended to help users choose the appropriate loading method with `w.qread`.
#' @param ... By default, all objects in the R environment are saved. Alternatively, users can specify multiple objects, in which case only the selected objects will be saved to the local file.
#' @param envir The environment in which the objects are located.
#' @param version Version of the serialization package to use. Integer value:
#' \itemize{
#'   \item 1: use the `qs` package
#'   \item 2: use the `qs2` package
#' }
#' @param compress_level As with `qs::qsave` or `qs2::qs_save`.
#'
#' @param nthreads As with `qs::qsave` or `qs2::qs_save`, a single thread is used by default. It is recommended not to use too many cores, as excessive parallelism may increase thread scheduling overhead and lead to negative performance gains.
#'
#' @param shuffle As with the `qs2::qs_save`.
#'
#' @returns
#' NULL.
#'
#' @examples
#' x1 <- data.frame(int = sample(1e3, replace=TRUE), num = rnorm(1e3) )
#' x2 <- data.frame(int = sample(1e4, replace=TRUE), num = rnorm(1e4) )
#' x3 <- data.frame(int = sample(1e5, replace=TRUE), num = rnorm(1e5) )
#'
#' #1. entire R environment
#' w.qsave( 'entire_R_environment.qdata' )
#' # w.qread( 'entire_R_environment.qdata' )
#'
#' #2. x1 and x2
#' w.qsave( 'x1_x2.qdata' , x1, x2 )
#' # w.qread( 'x1_x2.qdata' )    #load x1 and x2 into the R environment
#' # x1_x2_list <- w.qread( 'x1_x2.qdata' , return = T )    #return a list object
#'
#' #3. x1
#' w.qsave(  'x1.qrds' , x1 )
#' # w.qread( 'x1.qrds' , return = F )    #load x1 into the R environment
#' # x1.copy <- w.qread( 'x1.qrds' , return = T )    #assign x1 to a new R object (x1.copy).
#'
#' @export
#'
#'
w.qsave <- function(filename, ... , envir = base::parent.frame(),
                     version = 2,
                     compress_level = qs2::qopt( "compress_level" ),
                     nthreads = 1,
                     shuffle = qs2::qopt("shuffle")
){
  #
  message( w.log_time_title() , w.log_text_coloured( s.c = 's' ),
           "Path to file: " , filename , '.'
           )
  #
  threads <- nthreads
  version <- as.integer( version )
  #
  dots <- base::substitute(list(...))[-1]

  ##########qs
  if ( version == 1  ){
    w.package_install( "qs" , method = 'remotes::install_cran("qs", type = "source", configure.args = "--with-simd=AVX2")' )

    # Q1
    if (length(dots) == 0) {
      #
      obj_names <- base::ls(envir = envir)
      obj_list  <- base::mget(obj_names, envir = envir)

      qs::qsave(x = obj_list,
                 file = filename,
                 compress_level = compress_level ,
                 nthreads = threads )

      message(w.log_time_title() , "Saved entire environment: ", length(obj_names), " objects → ",
              w.log_text_coloured(  text = filename , color = 'red'   ) , '.'
              )

    } else {
      # Q2
      obj_names <- base::sapply(dots, deparse)
      obj_list  <- base::mget(obj_names, envir = envir)

      qs::qsave(x = obj_list,
                  file = filename,
                  compress_level = compress_level ,
                  nthreads = threads )

      message(w.log_time_title() , "Saved objects: ", paste(obj_names, collapse = ", "), " → ",
              w.log_text_coloured(  text = filename , color = 'red'   ), '.'
              )
    }
  }


  ##########qs2
  if ( version == 2  ){
    w.package_install( "qs2" )

    # Q1
    if (length(dots) == 0) {
      #
      obj_names <- base::ls(envir = envir)
      obj_list  <- base::mget(obj_names, envir = envir)

      qs2::qs_save(object = obj_list, file = filename,
                   compress_level = compress_level ,
                   shuffle = shuffle ,
                   nthreads = threads )

      message(w.log_time_title() , "Saved entire environment: ", length(obj_names), " objects → ",
              w.log_text_coloured(  text = filename , color = 'red'   ) , '.'   )

    } else {
      # Q2
      obj_names <- base::sapply(dots, deparse)
      obj_list  <- base::mget(obj_names, envir = envir)

      qs2::qs_save(object = obj_list, file = filename,
                   compress_level = compress_level ,
                   shuffle = shuffle ,
                   nthreads = threads )

      message(w.log_time_title() , "Saved objects: ", paste(obj_names, collapse = ", "), " → ",
              w.log_text_coloured(  text = filename , color = 'red'   ) , '.'
              )
    }
  }

  #
  return(base::invisible(NULL))
}




#####################read
#base function
w_qsRead <- function(filename , version ,  threads ){
  if( version == 1 ){
    w.package_install( "qs" , method = 'remotes::install_cran("qs", type = "source", configure.args = "--with-simd=AVX2")' )
    w_qload_obj_list <- qs::qread( file = filename ,  nthreads =  threads)
  }
  if( version == 2 ){
    w.package_install( "qs2" )
    w_qload_obj_list <- qs2::qs_read( file = filename,  nthreads =  threads)
  }
  return( w_qload_obj_list  )
}


w_baseRead <- function(filename , version  ){
  if( version == 1 ){
    env <- base::new.env()
    invisible( base::load( filename , envir = env) )
    obj_names <- base::ls(env)
    w_qload_obj_list <- base::mget(obj_names, envir = env)
  }
  if( version == 2 ){
    w_qload_obj_list <- list( filename = base::readRDS( filename ) )
  }
  return( w_qload_obj_list  )
}


#' w.qread
#'
#' @description
#' Loads objects saved via `w.qsave` into the R environment.
#'
#' It first attempts to read the object using all available versions of the `qs` package. If none succeed, it will automatically fall back to `base::load` and `base::readRDS` to ensure backward compatibility and robust loading.
#'
#' @param filename The file name/path.
#' @param return Return the saved objects as variables instead of loading them into the target environment. Default: FALSE.
#'
#' If True, return the object directly when only one object is saved, similar to `base::readRDS`; otherwise return a list of all objects.
#'
#' @param envir The target environment into which the variables will be loaded. This argument is ignored when `return` is `TRUE`.
#' @param version Version of the serialization package to use. Integer value:
#' \itemize{
#'   \item 1: use the `qs` package
#'   \item 2: use the `qs2` package
#' }
#'
#' No worries! If reading fails using the specified version, other version-specific functions will be automatically called to attempt loading the object.
#'
#' @param delete Whether to delete the local file after reading is completed. Default: FALSE.
#' @param nthreads As with `qs::qread` or `qs2::qs_read`, a single thread is used by default. It is recommended not to use too many cores, as excessive parallelism may increase thread scheduling overhead and lead to negative performance gains.
#'
#' @returns
#' NULL.
#'
#' @examples
#' x1 <- data.frame(int = sample(1e3, replace=TRUE), num = rnorm(1e3) )
#' x2 <- data.frame(int = sample(1e4, replace=TRUE), num = rnorm(1e4) )
#' x3 <- data.frame(int = sample(1e5, replace=TRUE), num = rnorm(1e5) )
#'
#' #1. entire R environment
#' w.qsave( 'entire_R_environment.qdata' )
#' # w.qread( 'entire_R_environment.qdata' )
#'
#' #2. x1 and x2
#' w.qsave( 'x1_x2.qdata' , x1, x2 )
#' # w.qread( 'x1_x2.qdata' )    #load x1 and x2 into the R environment
#' # x1_x2_list <- w.qread( 'x1_x2.qdata' , return = T )    #return a list object
#'
#' #3. x1
#' w.qsave(  'x1.qrds' , x1 )
#' # w.qread( 'x1.qrds' , return = F )    #load x1 into the R environment
#' # x1.copy <- w.qread( 'x1.qrds' , return = T )    #assign x1 to a new R object (x1.copy).
#'
#' #4. base::save
#' base::save( x1 , x2 , file = 'x1_x2.data'  )
#' # w.qread( 'x1_x2.data' )    #load x1 and x2 into the R environment
#' # x1_x2_list <- w.qread( 'x1_x2.data' , return = T )    #return a list object
#'
#' #5. base::saveRDS
#' base::saveRDS( x1 , file = 'x1.rds'  )
#' # w.qread( 'x1.rds' , return = F )    #load x1 into the R environment
#' # x1.copy <- w.qread( 'x1.rds' , return = T )    #assign x1 to a new R object (x1.copy).
#'
#' @export
#'
w.qread <- function( filename , return = FALSE ,
                     envir = base::parent.frame() ,
                     version = 2,
                     delete = FALSE , nthreads = 1
                     ){
  #
  if( !base::file.exists(  filename  ) ){
    stop(  paste0(  filename ,  ' not found.'  )  )
  }

  #
  message( w.log_time_title() , w.log_text_coloured( s.c = 's' ),
           "Path to file: " , filename , '.' )
  #
  threads <- nthreads
  version <- as.integer( version )

  #######Use qs
  all_version <- c( 1 , 2 )
  all_packages <- c( "qs::qread" , "qs2::qs_read"  )   # match with all_version

  #
  myerror = T
  while ( myerror & length( all_version  ) > 0  ){
    #
    pkgName <- all_packages[[version]]
    version_index  <- which( all_version  == version )
    all_version <- all_version[-version_index]

    w_qload_obj_list <-tryCatch({
        read_result <- w_qsRead( filename = filename , version = version ,  threads = threads )
      },error = function(e) {
        NULL
      }
    )
    #
    if( !is.null( w_qload_obj_list ) ){
      myerror = F
    }else{
      if(  length( all_version  ) > 0  ){
        version = all_version[[1]]
      }
    }
    #
  }

  #######Use base
  if( myerror ){
    all_version <- c( 1 , 2 )
    all_packages <- c( "base::load" , "base::readRDS"  )

    #
    version = 1
    while( myerror & length( all_version  ) > 0  ){
      #
      pkgName <- all_packages[[version]]
      version_index  <- which( all_version == version )
      all_version <- all_version[-version_index]

      w_qload_obj_list <-tryCatch({
        suppressMessages(
          suppressWarnings(
            read_result <- w_baseRead( filename = filename , version = version )
          )
        )
      },error = function(e) {
        NULL
      }
      )
      #
      if( !is.null( w_qload_obj_list ) ){
        myerror = F
      }else{
        if(  length( all_version  ) > 0  ){
          version = all_version[[1]]
        }
      }
      #
    }
  }

  #
  if( !myerror ){
    message(w.log_time_title() , "Method used: ",
            w.log_text_coloured(  text = pkgName , color = 'blue' ),
            ". Loaded ", length(w_qload_obj_list), " objects from ",
            w.log_text_coloured(  text = filename , color = 'red'   ), '.'
            )

    #
    if( delete ){  temp <- base::file.remove( filename )  }
    #
    if( !return ){
      suppressMessages(
        suppressWarnings(
          base::list2env(w_qload_obj_list, envir = envir)
        )
      )
      return(base::invisible(NULL))
    }else{
      if( base::length( w_qload_obj_list ) == 1  ){
        return(  w_qload_obj_list[[1]]  )
      }else{
        return( w_qload_obj_list )
      }
      #
    }
    #
  }else{
    stop( paste0(  '>>> ' ,  filename   ,' <<< All available methods for reading the object have failed. Please check the input file and parameters.'
                   )

    )
  }
  #
}

