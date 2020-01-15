#' Plot pattern plots
#'
#' This function uses ggplot to plot pattern plots
#' @param df Pattern data to plot.
#' @param xlText Text to xlabel. Defaults to "".
#' @param ylText Text to ylabel. Defaults to "".
#' @param xticks True to display xticks. Defaults to T.
#' @param yticks True to display yticks. Defaults to T.
#' @param titleText Title of plot. Defaults to "".
#' @param legend True to display legend. Defaults to T.
#' @param showPanel True to keep plot frame. Defaults to T.
#' @param legendOnly True to only plot legend. Defaults to F.
#' @param textSize Size of text in plot. Defaults to 4.
#' @param n Number of colors, should equal number of possible combinations
#' @param colScale What type of scico colorscale to use. Defaults to "grayC".
#' 
#' @keywords plot, pattern, patternplot
#' @export
#' @examples
#' plotPatternplot()

plotPatternplot <- function(df, xlText = "", ylText = "", xticks = T, yticks = T, titleText = "", legend = T, showPanel = T, legendOnly = F, textSize = 4, n = 8, colScale = "grayC") {
  d <- melt(df, id.vars="name")
  if (legendOnly) {
    p <- ggplot(d, plot = F, aes(x=name, fill = variable)) + geom_bar() + theme(legend.title = element_blank()) + scale_fill_manual(values = scico(n, palette = colScale, end = 0.6))
    g <- cowplot::get_legend(p)
  } else {
    g <- ggplot(data=d, plot = F, aes(x=name, y=value, fill = variable), axes = F) + xlab(NULL) + ylab(NULL) + geom_bar(position = "fill", stat = "identity", color='black') +
      theme_bw() + theme(plot.title = element_blank(), legend.title = element_blank(), axis.title.x = element_blank()) + scale_fill_manual(values = scico(n, palette = colScale, , end = 0.6)) +
      geom_text(aes(label = c(paste0(round(value[1], 3)*100,"%"), "", "", "", "", "", "", paste0(round(value[8], 3)*100,"%"))), position = position_stack(vjust = 0.5), size = textSize)
                #geom_text(aes(label = paste0(round(value, 3)*100,"%")), position = position_stack(vjust = 0.5), size = textSize) 
    if (!showPanel) {
      g <- g + theme(axis.line=element_blank(), panel.background=element_blank(), panel.border=element_blank(),panel.grid.major=element_blank(), 
                     panel.grid.minor=element_blank(), plot.background=element_blank())
    }
    if (!legend) {
      g <-  g + theme(legend.title = element_blank(), axis.line=element_blank(), legend.position="none")
    } 
    if (!xticks) {
      g <-  g + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    }
    if (!yticks) {
      g <- g + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
    }
    if (xlText != "") {
      g <-  g + theme(axis.title.x = element_text()) + labs(x=xlText) 
    }
    if (ylText != "") {
      g <- g + ylab(ylText)
    }
    if (titleText != "") {
      g <-  g + theme(plot.title = element_text()) + ggtitle(titleText)
    }
  }
  g
}
