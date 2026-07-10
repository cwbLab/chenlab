

#' Time of log
#'
#' @param format Format of time. `1` (bold and cyan); `2` (bold); `3` (regular).
#'
#' @returns
#' The returned value is designed to be used with the `message` or `cat` function.
#'
#' @export
ww.log_time_title <- function(  format = 1  ){

  text <- paste0( '[', format(Sys.time(), "%Y-%m-%d %H:%M:%S") , '] ' )
  #
  if ( format == 1  ){
    #bold&cyan
    time <- crayon::bold$cyan( text  )
  }else if ( format == 2  ){
    #bold
    time <- crayon::bold(  text  )
  }else if ( format == 3  ){
    #regular
    time <- text
  }
  #
  return( time )
}


#' Color of message
#'
#' @description
#' Control the color of messages displayed via `message` or `cat` function.
#'
#' @param s.c Character flag indicating the log status. Use `"s"` for "Started" and `"c"` for "Completed".
#' @param text Optional character string specifying the custom message to print. If provided, the s.c parameter is ignored.
#' @param color The text color supported by the [crayon::make_style] (e.g. `"red"`, `"#00BFFF"`).
#' @param color.bg The text background color supported by the [crayon::make_style] (e.g. `"red"`, `"#00BFFF"`).
#'
#' @returns
#' A character string with ANSI color codes applied, suitable for printing via `message` or `cat` function.
#'
#' @export
ww.log_text_coloured <- function( s.c = 's', text = NULL,  color = 'green' , color.bg = NULL   ){
  #
  ww.package_library( crayon )
  #
  if( is.null(text) ){
    if( s.c == 's' ){ text = 'Started. '}else if(  s.c == 'c' ){ text = 'Completed. ' }
  }

  #forecolor
  if(  !is.null( color  )  ){
    mfcolor <- crayon::make_style( color , bg = F  )
    text <- mfcolor(text)
  }

  #background color
  if(  !is.null( color.bg  )  ){
    mbcolor <- crayon::make_style( color.bg , bg = T  )
    text <- mbcolor(text)
  }
  #
  return( text )
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
ww.log_time_runtime <- function(  t.minor , t.major , prefix = 'Runtime: '   ){
  time_diff <- hms::as_hms( as.numeric( t.major - t.minor, units = "secs") )
  op <- paste0( prefix , time_diff , '.' )
  #
  return(op)
}
