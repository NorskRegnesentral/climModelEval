#' Plots a map with image.plot using scico colorscales and map coast boundary
#'
#' This function let you plot a map with drawn coast boundary, using a scico colorscale.
#' @param data Data to be ploted, 2d matrix or 
#' @param title Title of plot. Defaults to "". 
#' @param season Small text in upper corner of plot, for example to specify what season the data is from. Defaults to "". 
#' @param frac True to plot fraction of periods, and not number of periods. Defaults to TRUE. 
#' @param rowNames Names of rows, when plotting with matrix. Defaults to ". 
#' @param colList List of colors supplied to plotfunction. Defaults to c("#0000FF70", "#FF000070", "#00FF0070"). 
#' @param lines If true plot lines insted of barplot. Defaults to False
#' 
#' @keywords plot, barplot, histogram
#' @export
#' @examples
#' plotBarplot()

plotBarplot <- function(data, title = "", season = "", frac = TRUE, rowNames = "", colList = c("#0000FF70", "#FF000070", "#00FF0070"), lines = F) {
  if (!is.null(dim(data))) {
    if (frac) {
      for (i in 1:length(data[,1])) {
        data[i,] <- unlist(data[i,])/sum(unlist(data[i,]))
      }
    }
    colnames(data) <- 1:length(data[1,])
    rownames(data) <- rowNames
    if (lines) {
      plot(1:length(data[1,]), data[1,], type = "l", col = colList[1], plot = T)
      for (i in 2:(length(data[,1]))) {
        lines(1:length(data[1,]), data[i,], type = "l", col = colList[i], plot = T, add = T)
      }
    } else {
      barplot(data[1,], col = colList[1], plot = T)
      for (i in 2:(length(data[,1]))) {
        barplot(data[i,], col = colList[i], plot = T, add = T)
      }
    }
    legend(x = "right", legend = rowNames, fill=colList)
  } else {
    if (frac) {
      data <- unlist(data)/sum(unlist(data))
    }
    names(data) <- 1:length(data)
    colList = colList[1]
    if (lines) {
      plot(1:length(data), data, type = "l", col = colList)
    } else {
      barplot(data, col = colList)
    }
  }
  mtext(title, side = 3, line = -4, outer = TRUE)
  if (season != "") {
    legend("topright",paste("Season: ", season,sep=""),cex=1, bty = 'n')
  }
}