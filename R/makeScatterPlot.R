#' Creates a scatterplot
#'
#' Creates a scatterplot for the phenomenon 'phen' for each season by using the list
#' 'quantile_vals' created from function 'severalGgPlots'
#' plot_name_list contains names of plots per season
#' Also returns a list of the plots created
#' @param plot_name_list Name of output plot with path per season
#' @param quantile_vals As created by severalGgPlots
#' @param phen Phenomenon, ex 'precipitation'
#' @param size Size of plots. Defaults to 3.5
#' 
#' @keywords plot, iqd, scatterplot, scatter, plots
#' @export
#' @examples
#' makeScatterPlot()

makeScatterPlot <- function(plot_name_list, quantile_vals, phen, size = 3.5){
  
  if(!exists("colsAndNames")) {
    stop("colsAndNames function not defined")
  }
  colsAndNames()
  
  gg_list <- list()
  
  combinations <- combn(type_plot_names, 2)
  for(s in 1:length(seasons)){
    season <- seasons[s]
    pdf(plot_name_list[s], width = 10)
    max_mean_val <- max(quantile_vals[[season]]$mean)
    for(i in 1:dim(combinations)[2]){
      
      two_types <- combinations[, i]
      
      plot_data <- filter(quantile_vals[[season]], type == two_types[1], model != 'data_sets')
      plot_data$model <- factor(plot_data$model, levels = model_plot_names)
      plot_data$method <- factor(plot_data$method, levels = method_plot_names)
      names(plot_data)[which(names(plot_data) == 'mean')] <- 'x'
      
      other_data <- filter(quantile_vals[[season]], type == two_types[2], model != 'data_sets')
      plot_data$y <- other_data$mean
      # plot_data$label <- NA
      # plot_data$label[1:2] <- 1:2
      
      gg <- ggplot(data = plot_data) +
        geom_point(aes(x = x, y = y, col = model, shape = method), size = size, na.rm = TRUE) +
        geom_abline(slope = 1, intercept = 0) +
        lims(x = c(0, max_mean_val), y = c(0, max_mean_val)) +
        scale_shape_manual(values = c(8, 15, 16, 17, 18)) +
        scale_colour_manual(values = colors_vec) +
        # labs(x = paste('Mean ', season, ' ', phen, ' IQD,', two_types[1]), y = paste('Mean ', season, ' ', phen, ' IQD,', two_types[2])) +
        labs(x = paste(two_types[1]), y = paste(two_types[2]),
             title = paste('Mean ', phen, ' IQD, ', season, sep = '')) +
        theme(plot.title = element_text(size=18), plot.subtitle = element_text(size = 18), legend.text = element_text(size = 18),
              axis.title = element_text(size = 18), legend.title = element_text(size = 18), axis.text = element_text(size = 15))
      
      
      plot_data_2 <- filter(quantile_vals[[season]], type == two_types[1], model == 'data_sets')
      names(plot_data_2)[which(names(plot_data_2) == 'mean')] <- 'x'
      other_data <- filter(quantile_vals[[season]], type == two_types[2], model == 'data_sets')
      plot_data_2$y <- other_data$mean
      
      gg <- gg + geom_point(data = plot_data_2, aes(x = x, y = y, alpha = method), col = 'cyan', size = 6, shape = c(10, 11, 12), stroke = 1) +
        scale_alpha_manual('data products', values = c(1, 1, 1), guide = guide_legend(override.aes = list(colour = 'cyan', shape = c(10, 11, 12))),
                           labels = plot_data_2$method)
      
      print(gg)
      
      gg_list[[length(gg_list) + 1]] <- gg
      
    }
    dev.off()
  }
  return(gg_list)
}