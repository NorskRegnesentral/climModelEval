#' Calculate distribution of patterns in data.
#'
#' This function calculates the distribution of patterns given in seqVec in the data, made to be used with (mc)lapply in findDist.
#' @param i Index, to choose right period.
#' @param x Data, 3d array
#' @param freq Length of the periods
#' @param depth depth argument supplied to pstree-function 
#' @param seqVec Vector of sequences to look for
#' @param nmin Argument passed to pstree
#' @param lik Argument passed to pstree
#' 
#' @keywords pattern, patterns, dist, distribution, calc, calculate
#' @export
#' @examples
#' calcDist()

calcDist <- function(i, x, freq, depth, seqVec, nmin = 1, lik = F) {
  x <- x[,((i-1)*freq+1):(i*freq)]
  x <- seqdef(
    data = data.frame(x),
    labels = c("dry", "wet")
  )
  pst <- pstree(x, L=depth, nmin=1, lik=FALSE)
  predict(pst, seqdef(seqVec))
}