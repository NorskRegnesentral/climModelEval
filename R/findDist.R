#' Finds distribution of patterns in data.
#'
#' This function findes the distribution of patterns given in seqVec in the data.
#' @param data Data, 3d array
#' @param depth depth argument supplied to pstree-function 
#' @param seqVec Vector of sequences to look for
#' @param threshold Threshold for seting values to zero. Defaults to 0.1.
#' 
#' @keywords pattern, patterns, dist, distribution
#' @export
#' @examples
#' findDist()

findDist <- function(data, depth, seqVec, threshold = 0.1) {
  freq <- ifelse(dim(data)[[3]] == 2340, 90, 92)
  x <- matrix(data, ncol = length(data[1,1,]))
  x <- x[keepIndex, ]
  x <- ifelse(x>=threshold, 1, 0)
  res <- parallel::mclapply(1:26, calcDist, x = x, freq = freq, depth = depth, seqVec = seqVec, mc.cores = 1)
  res <- matrix(unlist(res), ncol = 26)
  rownames(res) <- seqVec
  res <- rowMeans(res)
}