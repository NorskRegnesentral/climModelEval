#' Calculate the IQ distance at each location
#'
#' At each location, calculate IQ distance between the ecdf of a single data set 
#' of a climate model ('modelArray') and the ecdf of obervations ('obsArray').
#' Both input arrays are three-dimensional (longitude, latitude, time). The
#' function IQDlocal() is accessed with two options: the crps_sample function
#' (crps=TRUE) and the integrate funtion (crps=FALSE). The former is much faster
#' and with no problems during the calculations. The integrate function is
#' problematic due to the 'subdivisions' (the maximum number of subintervals),
#' i.e. no convergence even though the number of subintervals is large.
#' @param modelArray Array with model data, 3d-array (longitude, latitude, time)
#' @param obsArray Array with dataproduct data, 3d-array (longitude, latitude, time)
#' @param crps If true, crps is used (strongly recomended), otherwise integrate is used
#' @param remove_indices Which indices to remove. Defaults to NULL
#' 
#' @keywords IQD, parallel
#' @export
#' @examples
#' IQD()

IQD <- function(modelArray, obsArray, crps, remove_indices = NULL){
  # Longitude and latitude dimension
  dim <- dim(modelArray)[1:2]
  # Make a (lon x lat,time)-matrix of the (lon,lat,time)-array
  # Each location (lon,lat) is given as a row
  modelArray <- apply(modelArray,c(3),rbind)
  obsArray <- apply(obsArray,c(3),rbind)
  # All combinations of longitude and latitude
  L <- rep(0,dim(modelArray)[1])
  for(i in 1:length(L)){
    if(any(remove_indices == i)){
      #if(any(remove_indices == i)){ # CHECK THIS!!!!
      L[i] <- NA
      #} 
    } else{
      if(crps)
        L[i] <- IQDlocal_crps(modelArray[i,],obsArray[i,])
      else
        L[i] <- IQDlocal_integrate(modelArray[i,],obsArray[i,])
    }
  }
  L <- matrix(L,dim[1],dim[2])
  
  L
}