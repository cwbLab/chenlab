

#' Lapply with progress bar
#'
#' @description
#' A wrapper for `base::lapply` that displays a synchronized progress bar.
#'
#'
#' @param X Same as `base::lapply`.
#' @param FUN Same as `base::lapply`.
#' @param ... Same as `base::lapply`.
#' @param pb Show progress bar. Default is TRUE.
#' @param time Display execution time. Default is TRUE.
#' @param unlist Apply `base::unlist` to the final result. Default is FALSE.
#'
#' @returns
#' Returns results consistent with `base::lapply`.
#'
#' @export
#'
w.pblapply <- function( X , FUN, ... , pb = T , time = T , unlist = F ){

  #
  start_time <- Sys.time()
  if( time ){ message( w.log_time_title() , w.log_text_coloured( s.c = 's' ) , 'Tasks: ' , length(X) ,'.'  )  }

  #
  result <- NULL
  if( pb ){
    #
    w.package_install( 'pbmcapply' )

    total_length <- length(X) + 1
    mypb <- pbmcapply::progressBar( min = 1 , max = total_length , initial = 1, style = "ETA"  )

    result <- base::list()
    for (  i in  seq(  2 , total_length , 1 )  ){
      result <- c( result , base::lapply(  X = X[ i - 1 ] , FUN = FUN , ... ) )
      utils::setTxtProgressBar(mypb, i )
    }
    base::close(mypb)
    #
    names(result) <- names(X)

  }else{
    result  <- base::lapply(  X = X , FUN = FUN , ... )
    names(result) <- names(X)
  }
  #
  end_time <- Sys.time()
  if( time ){ message( w.log_time_title() ,
                       w.log_text_coloured( s.c = 'c' ) ,
                       w.log_time_runtime(  t.minor = start_time ,t.major = end_time  ))
  }
  #
  if( unlist ){
    return(  base::unlist( result  )   )
  }else{
    return(result)
  }
}




#' Smart mclapply
#'
#' @description
#' A smart mclapply function that automatically determines the number of cores for parallel computation, maximizing the use of system resources while ensuring memory does not overflow.
#'
#' Its core logic is to split the input vector into chunks for execution, thereby preventing memory overflow.
#'
#' @param X Same as the X parameter in `parallel::mclapply`.
#' @param FUN Same as the FUN parameter in `parallel::mclapply`.
#' @param ... Same as `parallel::mclapply`.
#' @param mc.cores Maximum number of cores used for parallel computation. By default, the number of cores is automatically determined based on memory usage, ensuring that memory does not overflow while fully utilizing all available system resources.
#'
#' If an integer is provided, the program will force execution with the specified number of threads, ignoring memory protection.
#' @param mem.ratio.max Maximum proportion of available memory allowed when automatically determining the number of cores.
#' @param mem.max Maximum amount of memory (in GB) allowed when automatically determining the number of cores. Defaults to NULL, which will automatically detect the maximum available system memory.
#' @param pb Show progress bar. Default is TRUE.
#' @param time Display execution time. Default is TRUE.
#' @param unlist Apply `base::unlist` to the final result. Default is FALSE.
#'
#' @return
#' Returns results consistent with `parallel::mclapply`.
#'
#' @export
#'
w.smc <- function(X, FUN, ..., mc.cores = NULL, mem.ratio.max = 0.8 , mem.max = NULL ,
                  pb = T , time = T , unlist = F ){
  start_time <- Sys.time()

  #1
  threads = mc.cores
  is_windows <- .Platform$OS.type == "windows"
  total_cores <- parallel::detectCores()

  #windows
  if (is_windows){
    if( time ){ message( w.log_time_title() , w.log_text_coloured( s.c = 's' ) , 'Tasks: ' , length(X) ,'.'  )  }
    #
    if(pb){
      res <- w.pblapply( X = X, FUN = FUN, ... , time = F )
    }else{
      res <- base::lapply( X = X, FUN = FUN, ... )
    }
    #
    end_time <- Sys.time()
    if( time ){ message( w.log_time_title() ,
                         w.log_text_coloured( s.c = 'c' ) ,
                         w.log_time_runtime(  t.minor = start_time ,t.major = end_time  ))
    }
    #
    if( unlist ){
      return(  base::unlist( res )  )
    }else{
      return(res)
    }

  }

  #
  if (  is.null(threads) ){
    target_threads <- total_cores - 1
    target_threads <- max(1, min(target_threads, total_cores))


    #2
    sample_size <- min(5, length(X))
    set.seed(100)
    sample_idx <- sample(seq_along(X), sample_size)

    #
    mean_used <- c()
    for(  temp_idx  in  sample_idx  ){
      temp <- bench::bench_memory(
            suppressMessages( suppressWarnings( capture.output(
              test_results <- base::lapply(X = X[temp_idx], FUN = FUN, ...)
            ) ) )
          )[['mem_alloc']]
      temp <- max( as.numeric( temp ) / 1024^2 , 0.1 )

      mean_used <- c( mean_used , temp )
    }

    #minimum,0.5 MB
    avg_mem_per_task_mb <- max( median( as.numeric( mean_used ) ), 0.4167 ) * 1.2

    #3
    get_total_mem_gb <- function(){
      res <- tryCatch({
		    ps::ps_system_memory()[['avail']] / 1024^3
      }, error = function(e) NA )
      return(res)
    }

    total_mem_gb <- get_total_mem_gb()
    if( !is.na(total_mem_gb)  ){
      total_mem_gb <- total_mem_gb
      if( !is.null( mem.max  ) ){
        total_mem_gb <- as.numeric(mem.max)
      }
    }else{
      if( is.null( mem.max  ) ){
        total_mem_gb <- avg_mem_per_task_mb / 1024
      }else{
        total_mem_gb <- as.numeric(mem.max)
      }
      #
      message( 'Unable to automatically determine available system memory. Users can explicitly set the memory limit via the mem.max parameter. Memory allowed: ',
               w.log_text_coloured( text = round( total_mem_gb , digits = 3  ) , color = 'red' ),
               ' GB.'
              )
    }

    limit_mem_gb <- total_mem_gb * mem.ratio.max

    #4
    max_safe_cores <- floor((limit_mem_gb / (avg_mem_per_task_mb / 1024)) * 0.9)
    max_safe_cores <- max(1, max_safe_cores)

    chunk_size <- floor( max(2, min(length(X), max_safe_cores ) ) * 0.9 )

    myratio <- chunk_size / target_threads
    if( avg_mem_per_task_mb >= 100 & myratio < 100 ){
      target_threads = floor( max( target_threads / 3,  target_threads / 100   ) )
    }

    indices <- split(seq_along(X), ceiling(seq_along(X) / chunk_size))

    #5
    final_results <- vector("list", length(X))
    total_chunks <- length(indices)

    current_cores <- min( target_threads, max_safe_cores  )
    if( time ){ message(
      w.log_time_title(),
      w.log_text_coloured( s.c = 's' ),
      "Tasks total: ", w.log_text_coloured( text = length(X) ,color = 'red' ),
      "; Mem per task: ", w.log_text_coloured( text = round(avg_mem_per_task_mb, 3), color = 'red' ), w.log_text_coloured( text = ' MB' , color = 'red' ),
      "; Threads used: ", w.log_text_coloured( text = current_cores , color = 'red' ),'.'
    )}
    #
    progressr::with_progress({
      a <- 1:total_chunks
      mypb<- progressr::progressor(steps = length(a))

      for (i in seq_along(indices)){
        #
        curr_idx <- indices[[i]]
        #
        batch_res <- parallel::mclapply(
          X = X[curr_idx],
          FUN = FUN,
          ...,
          mc.cores = current_cores
        )
        final_results[curr_idx] <- batch_res
        #
        mypb()
      }

    },
    handlers = progressr::handlers(  progressr::handler_progress(
      format = "[:bar] :percent | Elapsed: :elapsed | ETA: :eta",
      clear = FALSE
    )),
    enable = pb
    )
    #
  }else{
    threads <- max(1, threads)
    #
    if( time ){ message(
      w.log_time_title(),
      w.log_text_coloured( s.c = 's' ),
      "Tasks total: ", w.log_text_coloured( text = length(X) ,color = 'red' ),
      "; Threads used: ", w.log_text_coloured( text =  as.integer(threads), color = 'red' ),'.'
    ) }
    #
    if(pb){
      final_results <- pbmcapply::pbmclapply( X = X, FUN = FUN, ..., mc.cores = as.integer(threads)  )
    }else{
      final_results <- parallel::mclapply( X = X, FUN = FUN, ..., mc.cores = as.integer(threads)  )
    }
    #
  }

  #
  end_time <- Sys.time()
  if( time ){ message( w.log_time_title() ,
                       w.log_text_coloured( s.c = 'c' ) ,
                       w.log_time_runtime(  t.minor = start_time ,t.major = end_time  ))
  }
  #
  if( unlist ){
    return(  base::unlist( final_results )  )
  }else{
    return(final_results)
  }
}


#' Multithreaded lapply
#'
#' @description
#' A special use case of `w.smc`, where memory usage is not limited. By default, all available threads are used for computation.
#'
#' @param X Same as the X parameter in `w.smc`.
#' @param FUN Same as the FUN parameter in `w.smc`.
#' @param ... Same as `w.smc`.
#' @param mc.cores Number of cores used for parallel computation. By default, the maximum computing resources are used.
#' @param pb Show progress bar. Default is TRUE.
#' @param time Display execution time. Default is TRUE.
#' @param unlist Apply `base::unlist` to the final result. Default is FALSE.
#'
#' @export
w.mc <- function( X, FUN, ..., mc.cores = NULL, pb = T ,time = T , unlist = F  ){
  if( is.null( mc.cores  ) ){  mc.cores = parallel::detectCores() - 1 }
  mc.cores <- max(1, mc.cores)
  #
  res <- w.smc(X, FUN, ..., mc.cores = mc.cores , mem.ratio.max = 0.8 , mem.max = NULL ,
               pb = pb ,time = time , unlist = unlist )
  #
  return( res )
}




