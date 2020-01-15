#' Finds the mean length of wet periods
#'
#' This function findes the mean length of the wet periods for each gridpoint in the given dataset
#' @param data Data to do computations on, 3d array.
#' @param threshold Threshold for seting values to zero. Defaults to 0.1.
#' @param d If true, we calculate for drydays, else wet. Defaults to False
#'
#' @keywords mean, period, length, dataHandling
#' @export
#' @examples
#' findMeanPeriodLength()

findMeanPeriodLength <- function(data, threshold = 0.1, d = F) {
  data <- ifelse(data>=threshold, 1, 0)                                             # 1 if wet, 0 if dry
  if (d) {
    data <- ifelse(data == 1, 0, 1)
  }
  x <- matrix(data = NA, nrow = length(data[, 1, 1]), ncol = length(data[1, , 1]))  # matrix of gridpoints, with NA-values
  x[keepIndex] <- 1                                                                 # Place 1 where we should have data
  indexes <- which(!is.na(x), arr.ind = TRUE)                                       # get matrix indexes of gridpoints where we have data
  l <- length(indexes[,1])                                                          # number of indexes
  freq <- ifelse(dim(data)[[3]] == 2340, 90, 92)                                    # get correct number of days per season 
  for (i in 1:l) {                                                                  # for each gridpoint where we should collect data
    nPeriods <- 0                                                                   # number of periods is set to 0
    totDays <- 0                                                                    # total days is set to 0
    for (j in 1:26) {                                                               # for each year/season
      res <- rle(data[indexes[i,][[1]], indexes[i,][[2]], ((j-1)*freq+1):(j*freq)]) # find information about patterns in given year/season
      nPeriods <- nPeriods + sum(res$values)                                        # find number of periods, and add to nPeriods
      totDays <- totDays + sum(res$lengths[which(res$values == 1)])                 # find number of wet days, and add to totDays
    }
    x[indexes[i,][[1]], indexes[i,][[2]]] <- totDays/nPeriods                       # calculate mean length of wet periods
  }
  x
}