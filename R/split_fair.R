#' Fair split
#'
#' @description
#' Split a vector into approximately equal-sized subsets, either by specifying the target chunk length or the number of subsets, while enforcing a minimum subset size.
#'
#' @param x A vector / list / factor to be split.
#' @param chunk.length Target length of each subset. If provided, the input vector will be split into chunks with approximately this length.
#' @param number Target number of subsets. If provided, the input vector will be evenly split into this many parts.
#' @param min.length Minimum allowed length of each subset. If the requested split would produce subsets smaller than this value, the number of subsets will be reduced automatically to satisfy this constraint.
#'
#' @returns
#' A list of subsets.
#'
#'
#' @export
w.split_fair <- function(x, chunk.length = NULL, number = NULL, min.length = 2 ){
  n <- length(x)

  # 1. chunk / number
  if (!is.null(chunk.length)){

    #chunk
    if( min( chunk.length ,min.length ) == chunk.length   ){
      stop("The value of chunk.length must be greater than min.length.")
    }
    #
    num_chunks <- max(1, floor(n / max(1, chunk.length)))

    diff = n - chunk.length
    if( num_chunks == 1 & n > chunk.length &  diff >= min.length ){
      num_chunks = 2
    }

  } else if (!is.null(number)) {
    #number
    if( min( n , number ) == n ){
      stop("The value of number must be less than the length of x.")
    }
    num_chunks <- max(1, number)
  } else {
    stop("Either chunk.length or number must be specified.")
  }

  # 2. min.length
  if (n / num_chunks < min.length) {
    num_chunks <- max(1, floor(n / min.length))
  }

  #
  if (num_chunks <= 1 || n == 0) return(list(x))

  # 3. split_fair
  base_size <- floor(n / num_chunks)


  #
  extra <- n %% num_chunks
  chunk_lengths <- rep(base_size, num_chunks)

  m1 <- T
  if (extra > 0){
    raw_index <- seq( length( chunk_lengths ) , 1 , -1 )

    #chunk.length
    if( !is.null(chunk.length) ){
      mystart <- 1
      while (m1) {
        if( mystart > length( chunk_lengths ) ){  mystart <- 1   }
        #
        if( extra < min.length  ){
          #
          chunk_lengths[ raw_index[ mystart ] ]  <- chunk_lengths[ raw_index[ mystart ] ] - 1
          extra <- extra + 1

          mystart <- mystart + 1
          #
        }else{
          m1  <- F
          chunk_lengths <- c( chunk_lengths , extra )
        }
      }
    }
  }
  if( m1 ){
    extra <- n %% num_chunks
    chunk_lengths <- rep(base_size, num_chunks)
    chunk_lengths[ ( length( chunk_lengths ) - extra + 1 ) :  length( chunk_lengths )  ] <- chunk_lengths[ ( length( chunk_lengths ) - extra + 1 ) :  length( chunk_lengths )  ] + 1
  }

  # 4.
  ends <- cumsum(chunk_lengths)
  starts <- c(1, ends[-length(ends)] + 1)

  res <- lapply(seq_along(starts), function(i){
    x[starts[i]:ends[i]]
  })

  # 5.
  return(res)
}



