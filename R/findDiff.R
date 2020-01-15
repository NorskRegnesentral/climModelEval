#' Find difference between two datasets
#'
#' This function finds the difference between the mean for each grid in a and b (a-b)
#' @param a Data, 3d array
#' @param b Data, 3d array
#' @param relative If true, returns relative difference. 
#' 
#' @keywords difference, diff, dataHandling
#' @export
#' @examples
#' findDiff()

findDiff <- function(a, b, relative = F) {
  a <- apply(a, c(1, 2), mean) # find mean per gridpoint
  b <- apply(b, c(1, 2), mean)
  diff <- a - b                # find diff
  if (relative) {
    diff <- diff/a
  }
  diff[!keepIndex] <- NA       # place NAs where we should not have any data
  diff                        
}