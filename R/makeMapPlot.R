#' Make a map plot
#'
#' Used in severalGgPlots to make map plots
#' Takes in a data.frame with columns val, lon and lat.
#' Prints the values from the data.frame over a map of Fennoscandia
#' @param data Data to plot
#' @param z_lab Defaults to ''
#' @param option Passed to "scale_colour_viridi" as option argument. Defaults to 'D'
#' @param title Passed to "labs" as title argument. Defaults to NULL,
#' @param summ Defaults to NULL 
#' @param info Defaults to NULL
#' @param title_col Defaults to 'black'
#' @param guide_lim Defaults to c(min(data$val), max(data$val))
#' @param log If True, logscale. Defaults to FALSE
#' @param subtitle Passed to "labs" as subtitle argument. Defaults to NULL
#' @param ratio Defaults to 1.8
#' @param xlim xlimit. Defaults to c(3, 35)
#' 
#' @keywords plot, severalGgPlots
#' @export
#' @examples
#' makeMapPlot()
# ---------------------------------------------------------------------------------------------
makeMapPlot <- function(data, z_lab = '', option = 'D', title = NULL,
                        summ = NULL, info = NULL, title_col = 'black',
                        guide_lim = c(min(data$val), max(data$val)), log = FALSE,
                        subtitle = NULL, ratio = 1.8, xlim = c(3, 35)){
  
  
  annotations <- data.frame(xpos = NA, ypos = NA, text = NA, hjustvar = NA, vjustvar = NA)
  if(!is.null(info)){
    annotations <- rbind(annotations, data.frame(xpos = Inf, ypos = Inf, text = info,
                                                 hjustvar = 1, vjustvar = 1))
  }
  if(!is.null(summ)){
    annotations <- rbind(annotations, data.frame(xpos = -Inf, ypos = Inf, text = summ,
                                                 hjustvar = 0, vjustvar = 1))
  }
  
  if(log && any(data$val == 0, na.rm = TRUE)){
    data$val[which(data$val == 0)] <- min(data$val[-which(data$val == 0)], na.rm = TRUE) # log transform makes 0 into -Inf
  }
  
  gg <- ggplot(data = data) +
    geom_point(aes(x = lon, y = lat, col = val), na.rm = TRUE) +
    geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
    coord_fixed(ratio = ratio, xlim = xlim, ylim = c(54.7, 72.6)) +
    labs(x = 'longitude', y = 'latitude', col = z_lab) +
    theme(plot.title = element_text(colour = title_col))
  
  if(dim(annotations)[1] > 1){
    gg <- gg + geom_label(data = as.data.frame(annotations[-1, ]),
                          mapping = aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = text))
  }
  
  if(log){
    guide_lim[which(guide_lim <= 0)] <- NA
    
    gg <- gg +
      scale_colour_viridis(na.value = NA, option = option, limits = guide_lim, trans = 'log',
                           breaks = exp(-10:10), labels = paste('exp(', -10:10, ')', sep = ''))
  } else{
    gg <- gg +
      scale_colour_viridis(na.value = NA, option = option, limits = guide_lim)
    # scale_colour_gradientn(na.value = NA, colours = rainbow(7), limits = guide_lim)
  }
  
  if(!is.null(subtitle)){
    gg <- gg + labs(subtitle = subtitle)
  }
  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }
  
  gg
}