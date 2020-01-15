#' Finds length of wet periods.
#'
#' This function finds the length of wet periods in the dataset and places the data in a given matrix.
#' @param data Data, 3d array, matrix, list or vector
#' @param r Row in matrix m to place data
#' @param m Matrix to place returned data.
#' @param threshold Threshold for seting values to zero. Defaults to 0.1.
#' @param d If true, we calculate for drydays, else wet. Defaults to False
#' 
#' @keywords periods, histogram, hist, length, matrix
#' @export
#' @examples
#' findPeriodsLength()

findPeriodsLength <- function(data, r, m, threshold = 0.1, d = F) {
  data <- ifelse(data>=threshold, 1, 0)                                                 # 1 if wet, 0 if dry
  if (d) {
    data <- ifelse(data == 1, 0, 1)
  }
  x <- matrix(data = NA, nrow = length(data[, 1, 1]), ncol = length(data[1, , 1]))     # create matrix of gridpoints with NA-values
  x[keepIndex] <- 1                                                                    # place 1 in gridpoints where we want data
  indexes <- which(!is.na(x), arr.ind = TRUE)                                          # find matrix indexes where we want data
  l <- length(indexes[,1])                                                             # find number of gridpoints with data
  freq <- ifelse(dim(data)[[3]] == 2340, 90, 92)                                       # get correct number of days per season 
  for (i in 1:l) {                                                                     # for each gridpoint where we should collect data
    for (j in 1:26) {                                                                  # for each year/season
      res <- rle(data[indexes[i,][[1]], indexes[i,][[2]], ((j-1)*freq+1):(j*freq)])    # find information about patterns in given year/season
      d <- res$lengths[which(res$values == 1)]                                         # place lengths of wet periods in d
      for (n in d) {                                                                   # for a period in d
        m[r,n] <- m[r, n] + 1                                                          # increment matrix element at (dataproduct, length of period)
      }
    }
  }
  m
}
