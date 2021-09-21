#' @title       plotMapEconOfIrrig
#' @description plot 3-layered map representing different irrigation gain thresholds
#'
#' @param version   subfolder of inputdata
#' @param input     Object containing the share of water that is fulfilled
#' @param year      Year to be shown in plot
#' @param scenario  EFP scenario and non-agricultural water use scenario
#'                  separated by "."
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapEconOfIrrig() }
#'
#' @importFrom magclass read.magpie
#' @importFrom luplot plotmap2
#' @importFrom mrwater toolLPJarrayToMAgPIEmap
#' @importFrom ggplot2 theme element_blank element_rect element_text
#'
#' @export
#'

plotMapEconOfIrrig <- function(version       = "GT500",
                               multicropping = FALSE,
                               EFP           = "on",
                               legendScale   = "Mha") {

  ### Path ###
  inputdatapath   <- paste0(getwd(), "/inputdata/", version, "/")

  ### Read in data ###
  irrigatableArea <- read.magpie(paste0(inputdatapath, "DemandCurve_pot_single.mz"))

  x0                <- irrigatableArea[, , "0"][, , EFP]
  x0[x0 == 0]       <- NA

  x500              <- irrigatableArea[, , "500"][, , EFP]
  x500[x500 == 0]   <- NA

  x1000             <- irrigatableArea[, , "1000"][, , EFP]
  x1000[x1000 == 0] <- NA

  ### Cell size###
  y <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type = "cell")
  y <- (111e3 * 0.5) * (111e3 * 0.5) * cos(y$lat / 180 * pi) / 1000000000 # Mha
  y <- as.magpie(y, spatial = 1)
  getCells(y)  <- getCells(x0)

  # Area correction (only show areas where there is available irriagtion area > 1% of cellsize)
  z <- read.magpie(paste0(inputdatapath, "avlIrrigarea_pot.mz"))
  cellshare                   <- z / y
  cellshare[cellshare < 0.01] <- 0

  x0[cellshare == 0]       <- NA
  x500[cellshare == 0]     <- NA
  x1000[cellshare == 0]    <- NA

  # Legend range adjustment
  x0    <- x0 / y
  x500  <- x500 / y
  x1000 <- x1000 / y
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
       smallplot     = c(0.5, 0.85, 0.03, 0.06),
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
       smallplot     = c(0.5, 0.85, 0.06, 0.09),
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
       smallplot     = c(0.5, 0.85, 0.09, 0.12),
       add = T)
  title("Irrigation yield gain ", cex.main = 3.5, line = -112)
  # Curve Plot

  dev.off()

}
