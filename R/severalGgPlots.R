#' Creates a list of ggplots
#'
#' Creates a list of ggplots for different combinations of dataset, season, model and method.
#' Input: file_paths for IQD data saved with each type of dataset,
#'        longitude and latitude matrices for all coordinates
#'        guide_limits used for the colour guide in the plots
#' Output: A list of ggplots.
#'         A list of mean and quantile values for each of the plots
#' @param file_paths_in file_paths for IQD data
#' @param lon Longitude coordinates
#' @param lat Latitude coordinates
#' @param guide_lim colour guide in plots
#' @param model_nrs Which models to plot. Defaults to 1:length(model_names)
#' @param quantiles Defaults to c(0.05, 0.95)
#' @param want_summ Defaults to FALSE
#' @param method_nrs Which methods to plot. Defaults to 1:length(method_names)
#' @param type_nrs Which dataproducts to plot. Defaults to 1:length(type_names)
#' @param prec If True, precipitation. Defaults to TRUE
#' @param log If True, logscale. Defaults to FALSE
#' @param do_bootstrap If True, use bootstrap. Defaults to TRUE
#' @param bootstrap_samples Number of bootstrap samples. Defaults to 1000
#' 
#' @keywords plot, iqd, scatterplot, scatter, plots
#' @export
#' @examples
#' severalGgPlots()

severalGgPlots <- function(file_paths_in, lon, lat, guide_lim,
                           model_nrs = 1:length(model_names), quantiles = c(0.05, 0.95),
                           want_summ = FALSE, method_nrs = 1:length(method_names), type_nrs = 1:length(type_names),
                           prec = TRUE, log = FALSE, do_bootstrap = TRUE, bootstrap_samples = 1000){
  
  colsAndNames()
  
  all_plots <- list()
  for(season in seasons){
    all_plots[[season]] <- list()
    for(type in type_names[type_nrs]){
      all_plots[[season]][[type]] <- list()
      for(model in model_names[model_nrs]){
        all_plots[[season]][[type]][[model]] <- list()
        all_plots[[season]][[type]][['data_sets']] <- list()
      }
    }
  }
  
  quantile_vals <- list(summer = NULL, winter = NULL)
  quantile_names <- paste(quantiles, '-quantile', sep = '')
  
  for(t in type_nrs){
    load(file_paths_in[t]) # maybe just path_in?????
    for(season in seasons){
      for(model_type_name in names(IQD_list[[season]])){
        for(method_type_name in names(IQD_list[[season]][[model_type_name]])){
          
          if(prec){
            info <- paste(type_names[t], ' IQD, precipitation\n', season, ' 1980-2005', sep = '')
          } else{
            info <- paste(type_names[t], ' IQD, droughts\n', season, ' 1980-2005', sep = '')
          }
          IQD_mat <- IQD_list[[season]][[model_type_name]][[method_type_name]]
          
          data <- data.frame(lon = as.vector(lon),
                             lat = as.vector(lat),
                             val = as.vector(IQD_mat))
          title <- model_type_name
          subtitle <- method_type_name
          title_col <- colors_list[[model_type_name]] # returns NULL for data_sets
          
          if(want_summ){
            quan <- as.numeric(quantile(IQD_mat, na.rm = TRUE))
            # Text in upper left corner of the plot
            summ <- paste("min = ", format(round(quan[1], 3), nsmall = 3), 
                          ", max = ", format(round(quan[5], 3), nsmall = 3), 
                          ", mean = ", format(round(mean(IQD_mat, na.rm = TRUE), 3), nsmall = 3), sep = "")
          } else{
            summ <- NULL
          }
          
          gg <- makeMapPlot(data = data,
                            title = title,
                            subtitle = subtitle,
                            summ = summ,
                            info = info,
                            title_col = title_col,
                            guide_lim = guide_lim[[season]],
                            log = log)
          
          all_plots[[season]][[type_names[t]]][[model_type_name]][[method_type_name]] <- gg
          
          li <- data.frame(model = model_type_name, method = method_type_name, type = type_names[t], val = mean(IQD_mat, na.rm =TRUE))
          names(li)[4] <- 'mean'
          
          
          if(do_bootstrap){
            IQD_vec <- as.vector(IQD_mat)
            IQD_vec <- IQD_vec[!is.na(IQD_vec)]
            l <- length(IQD_vec)
            
            mean_vec <- vector('numeric', bootstrap_samples)
            
            
            for(b in 1:bootstrap_samples){
              mean_vec[b] <- base::mean(sample(IQD_vec, l, replace = TRUE))
            }
            q <- as.numeric(quantile(mean_vec, probs = quantiles))
          } else{
            q <- as.numeric(quantile(IQD_mat, na.rm = TRUE, probs = quantiles))
          }
          li[[quantile_names[1]]] <- q[1]
          li[[quantile_names[2]]] <- q[2]
          quantile_vals[[season]] <- rbind(quantile_vals[[season]], li)
        }
      }
    }
  }
  return(list(all_plots = all_plots, quantile_vals = quantile_vals))
}