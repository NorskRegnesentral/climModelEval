#' Creates a ranking plot
#'
#' Creates a ranking plot for the phenomenon 'phen' for each season by using the list
#' 'quantile_vals' created from function 'severalGgPlots'
#' Also returns a list of the plots created
#' @param quantile_vals As created by severalGgPlots
#' @param plot_name Name of output plot with path
#' @param size Size of plots. Defaults to 3.5
#' @param phen Phenomenon, ex 'precipitation'
#' 
#' @keywords plot, rankingPlots, iqd, plots
#' @export
#' @examples
#' rankingPlots()

rankingPlots <- function(quantile_vals, plot_name, size = 3.5, phen){
  
  if(!exists("colsAndNames")) {
    stop("colsAndNames function not defined")
  }
  colsAndNames()
  
  quantile_names <- names(quantile_vals$summer)[(5:6)]
  gg_list <- list()
  
  pdf(plot_name, width = 10)
  for(season in seasons){
    all_dat <- quantile_vals[[season]]
    max_x_val <- max(all_dat[6])
    for(t in 1:length(type_plot_names)){
      type <- type_plot_names[t]
      plot_data <- filter(all_dat, type == type_plot_names[t])
      
      plot_data <- plot_data[order(plot_data$mean), ]
      plot_data$ranking <- 1:dim(plot_data)[1]
      
      type_data <- filter(plot_data, model == 'data_sets')
      plot_data <- filter(plot_data, model != 'data_sets')
      
      type_data$method <- factor(type_data$method, levels = type_plot_names)
      plot_data$model <- factor(plot_data$model, levels = model_plot_names)
      plot_data$method <- factor(plot_data$method, levels = method_plot_names)
      
      gg <- ggplot(data = plot_data) +
        geom_point(aes(x = mean, y = ranking, col = model, shape = method), size = size) +
        scale_shape_manual(values = c(8, 15, 16, 17, 18)) +
        scale_colour_manual(values = colors_vec) +
        labs(x = paste(phen, 'IQD'), y = '', title = paste('Mean ', type, ' IQD, ', season, sep = ''), 
             subtitle = paste(quantile_names[1], 'and', quantile_names[2])) +
        geom_point(aes(x = get(quantile_names[1]), y = ranking), shape = 124, size = 3) +
        geom_point(aes(x = get(quantile_names[2]), y = ranking), shape = 124, size = 3) +
        geom_segment(aes(x = get(quantile_names[1]), xend = get(quantile_names[2]), y = ranking, yend = ranking), size = 0.4) +
        lims(x = c(0, max_x_val)) +
        theme(plot.title = element_text(size=17), plot.subtitle = element_text(size = 18), legend.text = element_text(size = 18), 
              axis.title = element_text(size = 18), legend.title = element_text(size = 18), axis.text = element_text(size = 15))
      
      shapes <- as.numeric(type_data$method) + 9
      
      gg <- gg + geom_point(data = type_data, aes(x = mean, y = ranking, alpha = method), col = 'cyan', size = 6, shape = shapes, stroke = 1) +
        scale_alpha_manual('data products', values = c(1, 1, 1), guide = guide_legend(override.aes = list(colour = 'cyan', shape = c(10, 11, 12))),
                           labels = type_plot_names) +
        geom_point(data = type_data, aes(x = get(quantile_names[1]), y = ranking), shape = 124, size = 3) +
        geom_point(data = type_data, aes(x = get(quantile_names[2]), y = ranking), shape = 124, size = 3) +
        geom_segment(data = type_data, aes(x = get(quantile_names[1]), xend = get(quantile_names[2]), y = ranking, yend = ranking), size = 0.4)
      
      print(gg)
      
      gg_list[[length(gg_list) + 1]] <- gg
    }
  }
  dev.off()
  return(gg_list)
}