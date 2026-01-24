

#' Time of log
#'
#' @param format Format of time:
#'
#' (1) bold and cyan;
#'
#' (2) bold;
#'
#' (3) regular.
#'
#' @returns
#' The returned value is designed to be used with the message function.
#'
#' @export
wb.log_time_title <- function(  format = 1  ){
  time <- crayon::bold$cyan(  paste0( '[', format(Sys.time(), "%Y-%m-%d %H:%M:%S") , '] ' )  )
  if ( format == 1  ){
    #bold&cyan
    time = time
  }else if ( format == 2  ){
    #bold
    time <- crayon::bold(  paste0( '[', format(Sys.time(), "%Y-%m-%d %H:%M:%S") , '] ' )  )
  }else if ( format == 3  ){
    #regular
    time =  paste0( '[', format(Sys.time(), "%Y-%m-%d %H:%M:%S") , '] ' )
  }
  #
  return( time )
}


#' Started/Completed texts of log
#'
#' @param format Options are s (Started) and c (Completed).
#'
#' @returns
#' The returned value is designed to be used with the message function.
#'
#' @export
wb.log_time_start_end <- function(  format = 's'   ){
  op = crayon::green( 'Started. ' )
  if ( format == 's'  ){
    op = op
  }else if( format == 'c' ){
    op = crayon::green( 'Completed. ' )
  }
  #
  return( op )
}


#' Runtime of log
#'
#' @param t.minor Earlier Sys.time object
#' @param t.major Later Sys.time object
#' @param prefix Prefix
#'
#' @returns
#' The returned value is designed to be used with the message function.
#'
#' @export
wb.log_time_runtime <- function(  t.minor , t.major , prefix = 'Runtime: '   ){
  time_diff <- hms::as_hms( as.numeric( t.major - t.minor, units = "secs") )
  op <- paste0( prefix , time_diff , '.' )
  #
  return(op)
}
