#' Finds length of wet/dry periods per gridcell
#'
#' This function finds the length of wet/dry periods in the dataset per gridcell, and return a matrix with the results (gridcell i, number of periods with length j).
#' @param data Data, 3d array
#' @param periodLength Length of one period/season in days
#' @param frec Return frequency instead of number of occurences. Defaults to TRUE 
#' @param threshold Threshold for seting values to zero. Defaults to 0.1.
#' @param d If true, we calculate for drydays, else wet. Defaults to False
#' 
#' @keywords periods, length, matrix, grid
#' @export
#' @examples
#' findPeriodsLengthGrid()

findPeriodsLengthGrid <- function(data, periodLength, frec = T, threshold = 0.1, d = F) {
  data <- ifelse(data>=threshold, 1, 0)                                                # 1 if wet, 0 if dry
  if (d) {
    data <- ifelse(data == 1, 0, 1)
  }
  data[,,1][!keepIndex] <- NA
  x <- array(data = 0, dim = c(length(data[, 1, 1]), length(data[1, , 1]), periodLength))     # create matrix of gridpoints with NA-values
  for (i in 1:length(data[,1,1])) {                                                                     # for each gridpoint where we should collect data
    for (j in 1:length(data[1,,1])) {
      if (!is.na(data[i, j, 1])) {
        for (k in 1:26) {                                                                  # for each year/season
          res <- rle(data[i, j, ((k-1)*periodLength+1):(k*periodLength)])    # find information about patterns in given year/season
          d <- res$lengths[which(res$values == 1)]                                         # place lengths of wet periods in d
          for (n in d) {                                                                   # for a period in d
            x[i, j,n] <- x[i, j, n] + 1                                                          # increment matrix element at (dataproduct, length of period)
          }
        }
        if (frec) {
          x[i, j, ] <- x[i, j, ]/sum(x[i, j, ])
        }
      } else {
      x[i,j,] <- NA
      }
    }
  }
  x
}
