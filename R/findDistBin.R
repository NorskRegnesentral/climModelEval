#' Finds distribution of binomial pattern in data.
#'
#' This function sets values bellow threshold to zero, and find distribution of 1's and 0's in given data.
#' @param data Data, 3d array
#' @param threshold Threshold for seting values to zero. Defaults to 0.1.
#' 
#' @keywords pattern, patterns, dist, distribution
#' @export
#' @examples
#' findDistBin()

findDistBin <- function(data, threshold = 0.1) {
  x <- matrix(data, ncol = length(data[1,1,]))         # create matrix with same number of columns as opservations per grid, so one grid per row
  x <- x[keepIndex, ]                                  # only keep what we need
  if(threshold == "sc") {                              # if special case, where the interval (0mm, 1mm) is left out
    x <- x[which(0 == x | x >= 1, arr.ind = TRUE)]     # keep what we want
    x <- ifelse(x>=1, 1, 0)                            # set to 0 if dry, 1 if wet
    N <- length(x)                                     # number of days
    r <- sum(x)/N                                      # wet dayws/days
    list(1-r, r)                                       # returning ( % wet, % dry)
  } else {
    x <- ifelse(x>=threshold, 1, 0)                    # set 0 if dry, 1 if wet
    r <- sum(x)/length(x)
    list(1-r, r)
  }
}