#' Calculate IQD with integration
#'
#' Calculate integrated quadratic distance (IQ distance) between
#' the empirical cumulative distribution functions (ecdfs) of 
#' two data vectors with the integrate function. NB! The integrate
#' function is very slow and problematic with the 'subdivisions', 
#' (the maximum number of subintervals), i.e. no convergence even 
#' though the number of subintervals is large.
#' @param modelVec Vector with model data
#' @param obsVec Vector with dataproduct data
#' 
#' @keywords IQD, IQDlocal, integrate
#' @export
#' @examples
#' IQDlocal_integrate()

IQDlocal_integrate<- function(modelVec,obsVec){
  
  IQD <- NA
  
  # Vectors have values (not only NA's)
  if(any(!is.na(modelVec)) & any(!is.na(obsVec))){
    # Remove NA's
    modelVec <- modelVec[!is.na(modelVec)]
    obsVec <- obsVec[!is.na(obsVec)]
    # Limits of integration
    lowerLimit <- range(c(modelVec,obsVec))[1]
    upperLimit <- range(c(modelVec,obsVec))[2]
    # IQD of ecdf difference
    IQD <- integrate(Diff_ecdf,lower=lowerLimit,upper=upperLimit,subdivisions=2000,modelData=modelVec,obsData=obsVec)$value    
  }
  
  IQD
}