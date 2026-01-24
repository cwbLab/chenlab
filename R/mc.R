#' Multithreaded lapply
#' @description
#' A version of lapply that uses multiple threads for parallel computation
#'
#' @param X Same as the X parameter in mclapply
#' @param FUN Same as the FUN parameter in mclapply
#' @param mc.cores Number of cores used for parallel computation. By default, the maximum computing resources are used
#' @param pb Show progress bar. Default is TRUE
#' @param time Display execution time. Default is TRUE
#'
#' @export
wb.mc <- function( X, FUN, mc.cores = NULL, pb = T ,time = T ){
  start_time <- Sys.time()
  #
  if( is.null( mc.cores  ) ){  mc.cores = parallel::detectCores()  }
  if( time ){ message( wb.log_time_title() , wb.log_time_start_end() , 'Threads: ' , mc.cores,'. ','Tasks: ' , length(X) ,'.'  )  }
  #


  #
  if( pb ){
    res <- pbmcapply::pbmclapply( X = X , FUN = FUN , mc.cores = mc.cores  )
  }else{
    res <- parallel::mclapply(  X = X , FUN = FUN , mc.cores = mc.cores  )
  }
  #
  end_time <- Sys.time()
  if( time ){ message( wb.log_time_title() ,
                       wb.log_time_start_end( 'c' ) ,
                       wb.log_time_runtime(  t.minor = start_time ,t.major = end_time  )
                       )
    }

  #
  return( res )

}
