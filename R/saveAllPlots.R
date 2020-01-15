#' Saves plots
#' 
#' Saves all plots created from 'severalGgPlots' in one pdf file per dataset
#' @param all_plots list of plots, created by sevaralGgPlots
#' @param quantile_vals 
#' @param out_path filepath for output
#' @param file_ending file ending for output pdf. Defaults to "all_plots
#' 
#' @keywords plot, iqd, plots
#' @export
#' @examples
#' severalGgPlots()

saveAllPlots <- function(all_plots, quantile_vals, out_path, file_ending = 'all_plots'){
  colsAndNames()
  
  for(season in seasons){
    quantile_vals[[season]] <- data.frame(lapply(quantile_vals[[season]], as.character), stringsAsFactors = FALSE)
    for(t in 1:length(type_names)){
      type <- type_names[t]
      
      quantile_dat <- filter(quantile_vals[[season]], type == type_names[t])
      quantile_dat <- quantile_dat[order(quantile_dat$mean), ]
      
      dir <- paste(out_path, season, '/', type, '/', sep = '')
      
      ifelse(dir.exists(dir), FALSE, dir.create(dir, TRUE, TRUE))
      pdf(paste(dir, file_ending, '.pdf', sep = ''))
      for(i in 1:dim(quantile_dat)[1]){
        dat <- data.frame((quantile_dat[i, ]))
        gg <- all_plots[[season]][[type]][[dat$model]][[dat$method]]
        print(gg)
      }
      dev.off()
    }
  }
}