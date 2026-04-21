

#' Time of log
#'
#' @param format Format of time. `1` (bold and cyan); `2` (bold); `3` (regular).
#'
#' @returns
#' The returned value is designed to be used with the `message` or `cat` function.
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


#' Color of message
#'
#' @description
#' Control the color of messages displayed via `message` function.
#'
#' @param s.c Character flag indicating the log status. Use `"s"` for "Started" and `"c"` for "Completed". Ignored when `text` is not NULL.
#' @param text Optional character string specifying the custom message to print.
#' @param color Character string specifying the text color supported by the \pkg{crayon} package (e.g. `"green"`, `"red"`, `"yellow"`).
#'
#' @returns
#' A character string with ANSI color codes applied, suitable for printing via `message` function.
#'
#' @export
wb.log_text_coloured <- function( s.c = 's', text = NULL,  color = 'green'   ){
  #
  if( !is.null(text) ){
    mytext <- text
  }else{
    if( s.c == 's' ){ mytext = 'Started. '}else if(  s.c == 'c' ){ mytext = 'Completed. '   }
  }
  #
  cmd <- paste0( "crayon::" , color ,'("', mytext , '")'   )
  op <- eval(parse(text = cmd))
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
