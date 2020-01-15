#' Calculate IQD between a and b
#'
#' This function calculate the IQD between array a anb b and returns the answer as a grid, with IQD pr gridpoint
#' @param a Data, 3d array
#' @param b Data, 3d array
#' @param periodLength Length of one period/season in days
#' @param dry If true, we calculate for drydays, else wet. Defaults to False
#' @param raw If true, input is expected to be raw model/dataproduct data, else result from findPeriodsLengthGrid. Defaults to False
#' 
#' @keywords periods, IQD, discreet, length, matrix
#' @export
#' @examples
#' IQDdiscreet()

IQDdiscreet <- function(X, Y, periodLength, dry = F, raw = F) {
  if (raw) {
    X <- findPeriodsLengthGrid(X, periodLength, d = dry)
    Y <- findPeriodsLengthGrid(Y, periodLength, d = dry)
  } 
  res <- rowSums((X-Y)^2, na.rm = T, dims = 2)
  res[!keepIndex] <- NA
  res
}
