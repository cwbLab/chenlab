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
#' @export
wb.split_fair <- function(x, chunk.length = NULL, number = NULL, min.length = 2 ){
  n <- length(x)

  # 1. chunk / number
  if (!is.null(chunk.length)) {
    #chunk
    num_chunks <- max(1, floor(n / max(1, chunk.length)))
  } else if (!is.null(number)) {
    #number
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
  extra <- n %% num_chunks

  #
  chunk_lengths <- rep(base_size, num_chunks)
  if (extra > 0) {
    chunk_lengths[1:extra] <- chunk_lengths[1:extra] + 1
  }

  # 4.
  ends <- cumsum(chunk_lengths)
  starts <- c(1, ends[-length(ends)] + 1)

  res <- lapply(seq_along(starts), function(i) {
    x[starts[i]:ends[i]]
  })

  # 5.
  return(res)
}
