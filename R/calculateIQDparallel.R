#' Calculate the IQD in parallel
#'
#' This function calculates the IQD between data stored at paths at index i in the two input lists. 
#' @param i input index for parallization, specifies what portion of data to work on 
#' @param RCM_input_list List of paths to input climamodel data
#' @param dataset_input_list List of paths to input dataproduct data
#' @param output_list List of output paths
#' @param season_list List of seasons to calculate for
#' @param remove_indices Which indices to remove. Defaults to NULL
#' 
#' @keywords IQD, parallel
#' @export
#' @examples
#' calculateIQDparallel()

calculateIQDparallel <- function(i,RCM_input_list,dataset_input_list,output_list,seasons_list, remove_indices = NULL){
  
  crps <- TRUE
  season <- seasons_list[i]
  
  if(season == 'summer'){
    days <- 92
  } else if(season == 'winter'){
    days <- 90
  }
  
  years_season <- rep(1980:2005, each = days) # FIX THIS!!!!
  
  obs_nr <- which(years_season >= 1984 & years_season <= 2001)
  start_end_dataset <- matrix(obs_nr, ncol = days, byrow = TRUE) #lager en matrise med alle indekser vi er ute etter
  start_end_model <- ObsNrCM(start_end_dataset, years_season) #Finner indek til alle maalinger fra aar foer og etter den vi ser paa
  
  
  if(length(dataset_input_list) == 1){
    dataset_input_list <- rep(dataset_input_list, length(RCM_input_list))
  }
  
  
  # Load climate model and EOBS data number 'i' 
  name_1 <- load(RCM_input_list[i])
  index_1 <- grep(season, name_1, ignore.case = TRUE)
  model_array <- get(name_1[index_1])
  
  name_2 <- load(dataset_input_list[i])
  index_2 <- grep(season, name_2, ignore.case = TRUE)
  dataset_array <- get(name_2[index_2])
  
  # Number of comparisons (17)
  n_comp <- dim(start_end_model)[1]
  # Array with dimension 140x155x17
  IQD_array <- array(0,dim=c(dim(model_array)[1:2],n_comp))
  for(c in 1:n_comp){
    model_eval <- model_array[,,start_end_model[c,]]
    dataset_eval <- dataset_array[,,start_end_dataset[c,]]
    IQD_array[,,c] <- IQD(model_eval,dataset_eval,crps, remove_indices)
  }
  # Average over all comparisons (the third dimension of the array)
  # The result is an array of dimension 140x155
  IQD_mean_array <- apply(IQD_array,c(1,2),mean)
  # Save the IQD mean array to file
  save(IQD_mean_array,file=output_list[i])
  
  TRUE 
}