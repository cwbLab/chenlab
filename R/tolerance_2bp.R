#' Merge genomic coordinates
#'
#' @description
#' Merge genomic coordinates allowing a tolerance of up to 2 bp (default).
#'
#'
#' @param data A data frame with four columns representing chromosome, start coordinate, end coordinate, and strand.
#' @param tolerance Maximum number of tolerated base pairs (inclusive). Default: 2 bp.
#'
#' @returns
#' Data frame.
#'
#' @export
#'
wb.tolerance_merge <- function( data , tolerance = 2  ){
  #
  library(data.table)
  dt <- setDT(data)
  colnames( dt ) <- c( 'chr' , 'start' , 'end' , 'strand'   )

  dt$start <- as.integer(dt$start)
  dt$end <- as.integer(dt$end)

  #
  anchor_cluster <- function(x, tol = tolerance  ){
    o <- order(x)
    xs <- x[o]

    anchor <- xs[1]
    grp <- integer(length(xs))
    g <- 1

    for(i in seq_along(xs)){
      if(xs[i] - anchor > tol){
        g <- g + 1
        anchor <- xs[i]
      }
      grp[i] <- anchor
    }

    res <- numeric(length(xs))
    res[o] <- grp
    return( res )
  }

  #
  dt[, start_anchor := anchor_cluster(start), by=.(chr,strand)]
  dt[, end_anchor := anchor_cluster(end), by=.(chr,strand)]

  dt[, ID_raw := paste(chr, start, end, strand, sep=".")]
  dt[, ID_tolerance := paste(chr, start_anchor, end_anchor, strand, sep=".")]

  #
  dt[ , c( "start_anchor", "end_anchor"  ) := NULL  ]

  #
  return( data.frame(dt) )
}
