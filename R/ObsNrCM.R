#' Find indexes for model
#'
#'THIS SHOULD BE CORECCTED
#'This function findes which indexes to use for the model when comparing to the dataproduct, or somthing
#' @param startEndEOBS 
#' @param years Integer vector with value at index k year at index k from data array
#' 
#' @keywords IQD
#' @export
#' @examples
#' ObsNrCM()

ObsNrCM <- function(startEndEOBS,years){
  startEndCM <- matrix(NA,nrow=dim(startEndEOBS)[1],ncol=dim(startEndEOBS)[2]*9)
  fillRow <- 1
  for(y in 1984:2001){
    # 4 years before EOBS
    startYear <- y-4
    # 4 years after EOBS
    endYear <- y+4
    obsNr <- which(years>=startYear & years<=endYear)
    startEndCM[fillRow,] <- obsNr
    fillRow <- fillRow+1
  }
  startEndCM
}