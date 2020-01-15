#' Calculate IQD with crps
#'
#' Calculate integrated quadratic distance (IQ distance) between
#' the empirical cumulative distribution functions (ecdfs) of
#' two data vectors with the crps_sample function.
#' @param modelVec Vector with model data
#' @param obsVec Vector with dataproduct data
#' 
#' @keywords IQD, IQDlocal, crps
#' @export
#' @examples
#' IQDlocal_crps()

IQDlocal_crps<- function(modelVec,obsVec){
  
  IQD <- NA
  
  # Vectors have values (not only NA's)
  if(any(!is.na(modelVec)) & any(!is.na(obsVec))){
    # Remove NA's
    modelVec <- modelVec[!is.na(modelVec)]
    obsVec <- obsVec[!is.na(obsVec)]
    # IQD of ecdf difference
    scoreModel <- 0
    scoreObs <- 0
    for(i in 1:length(obsVec)){
      # method="edf" gives the empirical distribution function
      scoreModel <- scoreModel+crps_sample(y=obsVec[i],dat=modelVec,method="edf")
      scoreObs <- scoreObs+crps_sample(y=obsVec[i],dat=obsVec,method="edf")
    }
    IQD <- as.numeric((scoreModel-scoreObs)/length(obsVec))
  }
  
  IQD
}