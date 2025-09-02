#' @export
wb.mc <- function( X, FUN, mc.cores = NULL, pb =T ,time = T ){
  start_time <- Sys.time()
  if( time ){ message( paste0( format(Sys.time(), "%Y-%m-%d %H:%M:%S") , ' | ', 'Start' )  )  }
  #
  if( is.null( mc.cores  ) ){  mc.cores = parallel::detectCores()  }

  #
  if( pb ){
    res <- pbmcapply::pbmclapply( X = X , FUN = FUN , mc.cores = mc.cores  )
  }else{
    res <- parallel::mclapply(  X = X , FUN = FUN , mc.cores = mc.cores  )
  }
  #
  end_time <- Sys.time()
  if( time ){ message( paste0( format(Sys.time(), "%Y-%m-%d %H:%M:%S") , ' | ', 'End' )  )  }
  #
  time_taken <- end_time - start_time
  if( time ){ print( time_taken )  }
  #
  return( res )

}
