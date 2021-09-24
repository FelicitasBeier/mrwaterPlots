#' @title       plotMapEconOfIrrig
#' @description plot 3-layered map representing different irrigation gain thresholds
#'
#' @param version       subfolder of inputdata
#' @param multicropping multicropping activated (TRUE) or not (FALSE)
#' @param EFP           envrionmental flow protection activated ("on") or not ("off")
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapEconOfIrrig() }
#'
#' @importFrom magclass read.magpie collapseNames
#' @importFrom luplot plotmap2
#' @importFrom mrwater toolLPJarrayToMAgPIEmap
#' @importFrom ggplot2 theme element_blank element_rect element_text
#'
#' @export
#'

plotMapEconOfIrrig <- function(version       = "GT500",
                               multicropping = FALSE,
                               EFP           = "on") {

  ### Path ###
  inputdatapath   <- paste0(getwd(), "/inputdata/", version, "/")
  filename        <- "MapEconOfIrrig"

  ### Read in data ###
  irrigatableArea <- read.magpie(paste0(inputdatapath, "DemandCurve_pot_single.mz"))

  x0                <- irrigatableArea[, , "0"][, , EFP]
  x0[x0 == 0]       <- NA

  x500              <- irrigatableArea[, , "500"][, , EFP]
  x500[x500 == 0]   <- NA

  x1000             <- irrigatableArea[, , "1000"][, , EFP]
  x1000[x1000 == 0] <- NA

  potCropland1      <- collapseNames(read.magpie(paste0(inputdatapath, "DemandCurve_pot_single.mz"))[, , EFP])
  potCropland1      <- dimSums(potCropland1, dim = 1)
  getCells(potCropland1) <- "GLO"
  potCropland1           <- data.frame(y = as.numeric(as.character(as.data.frame(potCropland1)$Data1)),
                                       x = as.data.frame(potCropland1)$Value, stringsAsFactors = FALSE)
  # For curve plot
  x  <- potCropland1$x
  y  <- potCropland1$y
  a1 <- x[y >= 1000]
  b1 <- y[x == x[y >= 1000]]
  a2 <- x[y <= 1000 & y >= 500]
  b2 <- y[x == x[y <= 1000 & y >= 500]]
  a3 <- x[y <= 500]
  b3 <- y[x == x[y <= 500]]

  ### Cell size###
  cellsize <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type = "cell")
  cellsize <- (111e3 * 0.5) * (111e3 * 0.5) * cos(cellsize$lat / 180 * pi) / 1000000000 # Mha
  cellsize <- as.magpie(cellsize, spatial = 1)
  getCells(cellsize)  <- getCells(x0)

  # Area correction (only show areas where there is available irriagtion area > 1% of cellsize)
  z <- read.magpie(paste0(inputdatapath, "avlIrrigarea_pot.mz"))
  cellshare                   <- z / cellsize
  cellshare[cellshare < 0.01] <- 0

  x0[cellshare == 0]       <- NA
  x500[cellshare == 0]     <- NA
  x1000[cellshare == 0]    <- NA

  # Legend range adjustment
  x0    <- x0 / cellsize
  x500  <- x500 / cellsize
  x1000 <- x1000 / cellsize
  legendtitle  <- "Cellshare"
  legendrange  <- c(0, 0.1)
  legendbreaks <- seq(0, 0.1, 0.025)

  # Transform magpie object to raster object
  l <- toolMapTransform(x = x0, projection =  "EqualEarth")
  x0             <- l$x1
  l <- toolMapTransform(x = x500, projection =  "EqualEarth")
  x500           <- l$x1
  l <- toolMapTransform(x = x1000, projection =  "EqualEarth")
  x1000          <- l$x1
  landMask       <- l$landMask
  worldCountries <- l$worldCountries

  ### Create and save plot ###
  png(paste0("outputs/", filename, ".png"), height = 2000, width = 4000)

  # Plot physical potential
  par(bg = NA)
  plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)
  plot(x0, bg = "transparent", ylim = l$ylim, xlim = l$xlim,
       legend       = FALSE,
       col          = c("#e5f5e0", "#a1d99b", "#238b45", "#00441b"), # green color scale
       zlim         = legendrange,
       breaks       = legendbreaks,
       colNA        = NA,
       add          = T)
  plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
  plot(worldCountries, bg = "transparent", ylim = l$ylim, xlim = l$xlim, add = T)

  plot(x500, bg = "transparent", ylim = l$ylim, xlim = l$xlim,
       legend       = FALSE,
       col          = c("#deebf7", "#9ecae1", "#4292c6", "#08306b"), # blue color scale
       zlim         = legendrange,
       breaks       = legendbreaks,
       colNA        = NA,
       add          = T)
  plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
  plot(worldCountries, bg = "transparent", ylim = l$ylim, xlim = l$xlim, add = T)

  plot(x1000, bg = "transparent", ylim = l$ylim, xlim = l$xlim,
       legend       = FALSE,
       col          = c("#fee0d2", "#fc9272", "#cb181d", "#67000d"), # red color scale
       zlim         = legendrange,
       breaks       = legendbreaks,
       colNA        = NA,
       add          = T)
  plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
  plot(worldCountries, bg = "transparent", ylim = l$ylim, xlim = l$xlim, add = T)

  # Legend
  plot(x0, bg = "transparent",
       legend.only   = TRUE,
       legend.width  = 1,
       horizontal    = TRUE,
       col           = c("#e5f5e0", "#a1d99b", "#238b45", "#00441b"), # green color scale
       zlim          = legendrange,
       breaks        = legendbreaks,
       colNA         = "transparent",
       legend.args   = list(text = ">0", side = 2, font = 1, line = 2, cex = 4, las = 2),
       axis.args     = list(cex.axis = 4, at = legendbreaks, tick = FALSE, hadj = 0.5, padj = 0.5),
       smallplot     = c(0.4, 0.75, 0.03, 0.06),
       add = T)
  plot(x500, bg = "transparent",
       legend.only   = TRUE,
       legend.width  = 1,
       horizontal    = TRUE,
       col           = c("#deebf7", "#9ecae1", "#4292c6", "#08306b"), # blue color scale
       zlim          = legendrange,
       breaks        = legendbreaks,
       colNA         = "transparent",
       legend.args   = list(text = ">500", side = 2, font = 1, line = 2, cex = 4, las = 2),
       axis.args     = list(cex.axis = 4, at = legendbreaks, tick = FALSE, hadj = 0.5, padj = 50),
       smallplot     = c(0.4, 0.75, 0.06, 0.09),
       add = T)
  plot(x1000, bg = "transparent",
       legend.only   = TRUE,
       legend.width  = 1,
       horizontal    = TRUE,
       col           = c("#fee0d2", "#fc9272", "#cb181d", "#67000d"), # red color scale
       zlim          = legendrange,
       breaks        = legendbreaks,
       colNA         = "transparent",
       legend.args   = list(text = ">1000", side = 2, font = 1, line = 2, cex = 4, las = 2),
       axis.args     = list(cex.axis = 4, at = legendbreaks, tick = FALSE, hadj = 0.5, padj = 50),
       smallplot     = c(0.4, 0.75, 0.09, 0.12),
       add = T)
  title("Irrigated area cell share for different irrigation yield gain thresholds ",
        cex.main = 3.5, line = -115, adj = 0.6)
  # Curve Plot
  par(fig = c(0.02, 0.22, 0.11, 0.41), bg = "white", new = TRUE,
      mar = c(5.1, 6.5, 4.1, 2.1), xaxs = "i", yaxs = "i")
  plot(x, y, bg = "white",
       col = ifelse(y > 1000, "#cb181d", ifelse(y > 500, "#08306b", "#238b45")),
       main = "Global potentially irrigated area \non potential cropland", cex.main = 2.5,
       xlab = "Irrigated area (in Mha)",
       ylab = "",
       cex.lab = 2.5,
       type = "p", pch = 19, lty = 1, lwd = 2,
       bty = "l", yaxt = "n", xaxt = "n")
  s <- seq(length(a1)-1)
  segments(a1[s], b1[s], a1[s+1], b1[s+1], col= "#cb181d", lwd = 8)
  s <- seq(length(a2)-1)
  segments(a2[s], b2[s], a2[s+1], b2[s+1], col= "#08306b", lwd = 8)
  s <- seq(length(a3)-1)
  segments(a3[s], b3[s], a3[s+1], b3[s+1], col= "#238b45", lwd = 8)
  axis(side = 1, cex.axis = 2.5, hadj = 0.5, padj = 0,
       at = c(seq(from = 0, to = max(potCropland1$x),by = 200))) # x axis
  axis(side = 2, cex.axis = 2.5, hadj = 1, padj = 0.5,
       at = c(seq(from = 0, to = 3000, by = 500)), las = 1)      # y axis
  mtext("Irrigation yield gain (in USD/ha)", side = 2, line = 6, cex = 2.5)

  dev.off()

}
