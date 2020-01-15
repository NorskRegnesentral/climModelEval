#' Finds frequency of wet days
#'
#' This function returns a matrix with frequencey of wet days per gridpoint.
#' @param data Data, 3d array
#' @param treshold Datapoints with values less then treshold are forced to zero.
#' 
#' @keywords frequence, freq, dataHandling
#' @export
#' @examples
#' findFreq()

findFreq <- function(data, threshold = 0.1) {
  data[!keepIndex] <-  NA               # Place NA where we don't want data
  m <- ifelse(data>=threshold, 1, 0)    # 1 if wet, 0 if dry
  m <- rowSums(m, dims = 2)             # sum over z-dimention of data (time/days)
  m <- m/length(data[1,1,])             # divide by total number of days to get frequency of wet days
  m
}