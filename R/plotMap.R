#' Plots a map with image.plot using scico colorscales and map coast boundary
#'
#' This function let you plot a map with drawn coast boundary, using a scico colorscale.
#' @param data Data to be ploted, 2d matrix
#' @param lon Longitude data, 2d matrix. Defaults to lonFenno. 
#' @param lat Latitude data, 2d matrix. Defaults to latFenno. 
#' @param title Title of plot. Defaults to "". 
#' @param season Small text in upper corner of plot, for example to specify what season the data is from. Defaults to "". 
#' @param seasonSize Text size of taxt passed to season argument. Defaults to 1.
#' @param m Supply mean to print in lower corner. Defaults to ""
#' @param xlim To limit map area in x direction. Defaults to [55, 32]. 
#' @param ylim To limit map area in y direction. Defaults to [5, 73]. 
#' @param z To set a scale for the colorbar. Defaults to ". 
#' @param colScale What type of scico colorscale to use. Defaults to "roma". 
#' @param legend If T plot legend using image.plot, else ploting using poly.image. Defaults to True 
#' @param lWidth Width of legend. Defaults to 4
#' 
#' @keywords plot, maps, map
#' @export
#' @examples
#' plotMap()

plotMap <- function(data, lon = lonFenno, lat = latFenno, title = "", season = "", seasonSize = 1, m = "", xlim = c(5, 32), ylim = c(55,73), z = NULL, colScale = "roma", legend = T, lWidth = 4) {
  if (is.null(z)) {
    if (!legend) {
      poly.image(lon, lat, data, main = title, axes = F, ann = F, col = scico(30, palette = colScale), xlim = xlim, ylim = ylim)
    } else {
      image.plot(lon, lat, data, main = title, axes = F, ann = F, col = scico(30, palette = colScale), xlim = xlim, ylim = ylim, legend.width = lWidth)
    }
  } else {
    if (!legend) {
      poly.image(lon, lat, data, main = title, axes = F, ann = F, col = scico(30, palette = colScale), xlim = xlim, ylim = ylim, zlim=z)
    } else {
      image.plot(lon, lat, data, main = title, axes = F, ann = F, col = scico(30, palette = colScale), xlim = xlim, ylim = ylim, zlim=z, legend.width = lWidth)
    }
  }
  title(title)
  legend("topleft",season,cex=seasonSize, bty = 'n')
  if (m != "") {
    m <- paste0("Mean: ", m)
  }
  legend("bottomright", m, cex=seasonSize, bty = 'n')
  map(coast, add = T)
}
