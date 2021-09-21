#' @title       plotMap
#' @description plot magpie object in selected projection using the raster package
#'
#' @param x             magpie object
#' @param filename      name of png file name
#' @param projection    world map projection to be displayed in map
#'                      (EqualEarth, Robinson, Mollweide, ...)
#' @param legendtitle   title of legend as string
#' @param legendcolor   vector of strings of color codes
#' @param colNA         color code of color for NA values as string
#' @param legendlimit   max and min value of legend as vector
#' @param legendbreaks  legend breaks as vector
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMap(x) }
#'
#' @importFrom magclass as.RasterBrick
#' @importFrom rgdal readOGR
#' @importFrom raster crs proj4string projectRaster
#' @importFrom sp spTransform
#' @importFrom rgeos gDifference
#' @importFrom sf st_difference
#'
#' @export
#'

plotMap <- function(x, filename = "test",
         projection   = "EqualEarth",
         legendtitle  = "Legend",
         #legendcolor = c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529"),
         #legendcolor = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
         legendcolor  = c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"),
         colNA        ="#d9d9d9",
         legendlimit  = c(0, 1),
         legendbreaks = seq(0, 1, 0.1)) {

  # Transform magpie object to raster object
  l <- toolMapTransform(x = x, projection = projection)
  x1             <- l$x1
  landMask       <- l$landMask
  worldCountries <- l$worldCountries

  ### Create and save plot ###
  png(paste0("outputs/", filename, ".png"), height = 2000, width = 4000)

  # Main plot
  plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)
  plot(x1, ylim = l$ylim, xlim = l$xlim,
       legend       = FALSE,
       col          = legendcolor,
       zlim         = legendlimit,
       breaks       = legendbreaks,
       colNA        = colNA,
       add          = T)
  plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
  plot(worldCountries, ylim = l$ylim, xlim = l$xlim, add = T)

  # Legend
  plot(x1,
       legend.only   = TRUE,
       horizontal    = TRUE,
       col           = legendcolor,
       zlim          = legendlimit,
       breaks        = legendbreaks,
       colNA         = colNA,
       legend.args   = list(text = legendtitle, side = 3, font = 1, line = 2, cex = 4),
       axis.args     = list(cex.axis = 4, at = legendbreaks, line = 0, tick = FALSE, hadj = 0.5, padj = 0.5),
       smallplot     = c(0.4, 0.75, 0.05, 0.1),
       add = T)

  dev.off()

  # Plotting help
  # https://www.rdocumentation.org/packages/graphics/versions/3.5.2/topics/axis
  # https://mmeredith.net/blog/2019/plotting_rasters.htm
  # https://colorbrewer2.org/#type=diverging&scheme=BrBG&n=10

}
