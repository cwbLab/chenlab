
mymd5 <- function( object , pattern = NULL , recursive = NULL , opfile = NULL ,  mc.cores = NULL  ){

  ww.package_library(  'data.table'  )

  #args
  if( is.null( recursive ) ){  recursive = T   }

  #files
  if (   !dir.exists(  object  )  ){
    files = object
  }else{
    files = list.files(
      path = object ,
      pattern = pattern,
      full.names = TRUE,
      recursive = recursive
    )
    #
  }

  #check
  check_res <- which( file.exists(files) == FALSE  )
  if( length( check_res ) > 0  ){
    stop(  message( '❗ ',
      length(check_res),
      " files do not exist. Operation cancelled! Error files: ", '\n' ,
      paste( files[ check_res ], collapse = "\n")
    )   )
  }
  if( length(files) < 1  ){
    stop( 'No files available for MD5 calculation.' )
  }

  #md5
  res <- ww.mc(files, function(x){
    #
    m_value <- tools::md5sum( x )

    b_size <- file.info(x)$size

    #link
    link_p <- Sys.readlink(x)
    is_symlink <- !is.na(link_p) && nzchar(link_p)

    op <- as.list( c(
      x ,
      dirname = dirname(x),
      filename = basename(x),
      md5 = m_value ,
      size_B = b_size ,
      size_MB = b_size / 1024^2,
      size_GB = b_size / 1024^3,
      isdir = file.info(x)$isdir,
      islink = is_symlink,
      link_path = link_p

      )   )
    return( op )
    #
  } , mc.cores = mc.cores )
  res <- rbindlist(res) %>% as.data.frame()
  colnames(res) <- c(  'file_path' , 'dir_name' , 'file_name' , 'MD5' , 'size_B' , 'size_MB' , 'size_GB' ,
                       'is_dir', 'is_symbolic_link' , 'linked_to'
                       )

  #
  if( !is.null(opfile)  ){
    fwrite( res , file = opfile , sep = '\t' )
  }

  #
  return(res)

}




#######





#' Compute MD5 Checksums
#'
#' @description
#' Recursively calculate the MD5 checksums of a file or all files within a directory, and provide basic file information.
#'
#'
#' @param object Path to a file or directory.
#' @param pattern An optional regular expression. Only file names which match the regular expression will be returned. As with [base::list.files]. Only applicable when the `object` is a directory!
#' @param recursive Logical. Should the listing recurse into directories? As with [base::list.files]. Only applicable when the `object` is a directory!
#' @param opfile Output file name.
#' @param mc.cores Number of cores used for parallel computation. By default, the maximum computing resources are used.
#'
#' @returns
#' A data frame containing MD5 checksums and basic file information.
#'
#' @export
#'
#'
ww.md5 <- function(  object , pattern = NULL , recursive = NULL , opfile = NULL ,  mc.cores = NULL   ){
  #
  res <- mymd5( object = object ,
                pattern = pattern ,
                recursive = recursive ,
                opfile = opfile ,
                mc.cores = mc.cores  )
  return(res)
  #
}













