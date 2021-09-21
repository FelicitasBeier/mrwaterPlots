#' @title       plotMapIrrigAreaOverview
#' @description plot maps of irrigated area and projected potentially irrigated
#'              areas
#'
#' @param version   subfolder of inputdata
#' @param year      Year to be shown in plot
#' @param scenario  EFP scenario and non-agricultural water use scenario
#'                  separated by "."
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapIrrigAreaOverview() }
#'
#' @importFrom magclass read.magpie
#' @importFrom luplot plotmap2
#' @importFrom mrwater toolLPJarrayToMAgPIEmap
#' @importFrom ggplot2 theme element_blank element_rect element_text
#' @importFrom ggpubr ggarrange
#'
#' @export
#'

plotMapIrrigAreaOverview <- function(version    = "GT500",
                                     year       = "y2010",
                                     scenario   = "off.ssp2",
                                     outputtype = "png") {

  projection <- "EqualEarth"
  filename   <- "IrrigAreaOverview"

  if (length(year) > 1) {
    stop("Please select one year only for Map depicting the share of current
         irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  #-#-# Path #-#-#
  inputdatapath <- paste0(getwd(), "/inputdata/", version, "/")

  #-#-# Read in data #-#-#
  # LUH irrigated areas
  LUH     <- setYears(dimSums(collapseNames(read.magpie(paste0(inputdatapath, "cropareaLUH", ".mz"))[, , "irrigated"]), dim = 3), year)
  # LUH sustainably irrigated areas
  LUHsus <- dimSums(read.magpie(paste0(inputdatapath, "LUHfulfilled_comag", ".mz"))[, year, "on"][, , "ssp2"][, , "irrigatable"], dim = 3)

  # Potentially Irrigated Areas (GT = 0) on current cropland
  #currCropland   <- collapseNames(read.magpie(paste0(inputdatapath, "DemandCurve_curr_single.mz"))[, , "0"][, , "on"])
  currCropland <- dimSums(read.magpie(paste0(inputdatapath, "irrigArea_currCropland_comag.mz"))[, year, "on"][, , "ssp2"][, , "irrigatable"], dim = 3)

  # Potentially Irrigated Areas (GT = 0) on potential cropland
  #potCropland    <- collapseNames(read.magpie(paste0(inputdatapath, "DemandCurve_pot_single.mz"))[, , "0"][, , "on"])
  potCropland  <- dimSums(read.magpie(paste0(inputdatapath, "irrigArea_potCropland_comag.mz"))[, year, "on"][, , "ssp2"][, , "irrigatable"], dim = 3)

  ### Cell size###
  y <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type = "cell")
  y <- (111e3 * 0.5) * (111e3 * 0.5) * cos(y$lat / 180 * pi) / 1000000000 # Mha
  y <- as.magpie(y, spatial = 1)
  getCells(y)  <- getCells(currCropland)

  # Area correction (only show areas where there is available irrigation area > 1% of cellsize)
  z <- read.magpie(paste0(inputdatapath, "avlIrrigarea_pot.mz"))
  cellshare                   <- z / y
  cellshare[cellshare < 0.01] <- 0

  LUH[cellshare == 0]          <- NA
  LUHsus[cellshare == 0]       <- NA
  currCropland[cellshare == 0] <- NA
  potCropland[cellshare == 0]  <- NA

  # Legend range adjustment
  LUH          <- LUH / y
  LUHsus       <- LUHsus / y
  currCropland <- currCropland / y
  potCropland  <- potCropland / y
  legendtitle  <- "Cellshare"
  legendlimit  <- c(0, 0.1)
  legendbreaks <- seq(0, 0.1, 0.025)
  legendcolor  <- c("#edf8e9", "#74c476", "#31a354", "#006d2c")
#  legendcolor  <- c("#edf8e9", "#c7e9c0", "#a1d99b", "#74c476", "#31a354", "#006d2c")

  # Transform magpie object to raster object
  l <- toolMapTransform(x = LUH, projection = projection)
  LUH            <- l$x1
  l <- toolMapTransform(x = LUHsus, projection = projection)
  LUHsus         <- l$x1
  l <- toolMapTransform(x = currCropland, projection = projection)
  currCropland   <- l$x1
  l <- toolMapTransform(x = potCropland, projection = projection)
  potCropland    <- l$x1
  landMask       <- l$landMask
  worldCountries <- l$worldCountries

  ### Create and save plot ###
  if (outputtype == "png") {

    png(paste0("outputs/", filename, ".png"), height = 2000, width = 4000)

    par(mfrow = c(2, 2), bg = "transparent")

    ### Graph 1: LUH irrigated area
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)
    plot(LUH, ylim = l$ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = l$ylim, xlim = l$xlim, add = T)
    title("(a) Currently irrigated areas (LUH)", outer = FALSE, cex.main = 4)

    ### Graph 2: LUH irrigated area that can be fulfilled
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)
    plot(LUHsus, ylim = l$ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = l$ylim, xlim = l$xlim, add = T)
    title("(b) Currently irrigated areas (our study)", outer = FALSE, cex.main = 4)

    ### Graph 3: Potentially irrigated areas on current cropland
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)
    plot(currCropland, ylim = l$ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = l$ylim, xlim = l$xlim, add = T)
    title("(c) Potentially irrigated areas on current cropland", outer = FALSE, cex.main = 4)

    ### Graph 4: Potentially irrigated areas on potential cropland
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)
    plot(potCropland, ylim = l$ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = l$ylim, xlim = l$xlim, add = T)
    title("(d) Potentially irrigated areas on potential cropland", outer = FALSE, cex.main = 4)

    # Legend
    par(mfrow = c(1, 1), fig = c(0, 0.5, 0, 1), bg = "transparent", new = TRUE)
    plot(potCropland,
         legend.only   = TRUE,
         legend.width  = 5,
         horizontal    = FALSE,
         col           = legendcolor,
         zlim          = legendlimit,
         breaks        = legendbreaks,
         colNA         = "white",
         legend.args   = list(text = legendtitle, side = 3, font = 1, line = 3, cex = 4),
         axis.args     = list(cex.axis = 4, at = legendbreaks, line = 0, tick = FALSE, hadj = 0, padj = 0.5),
         add = T)

    dev.off()

  } else if (outputtype == "pdf") {

    ylim <- l$ylim + c(2500000, 0)

    pdf(paste0("outputs/", filename, ".pdf"), paper = "a4r", width = 11.69, height = 8.27, pointsize = 9)

    par(mar = c(1, 0, 1, 0), mfrow = c(2, 2), bg = "transparent")

    ### Graph 1: LUH irrigated area
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim)
    plot(LUH, ylim = ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = ylim, xlim = l$xlim, add = T)
    title("(a) Currently irrigated areas (LUH)", outer = FALSE, cex.main = 1)

    ### Graph 2: LUH irrigated area that can be fulfilled
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim)
    plot(LUHsus, ylim = ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = ylim, xlim = l$xlim, add = T)
    title("(b) Currently irrigated areas (our study)", outer = FALSE, cex.main = 1)

    ### Graph 3: Potentially irrigated areas on current cropland
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim)
    plot(currCropland, ylim = ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = ylim, xlim = l$xlim, add = T)
    title("(c) Potentially irrigated areas on current cropland", outer = FALSE, cex.main = 1)

    ### Graph 4: Potentially irrigated areas on potential cropland
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim)
    plot(potCropland, ylim = ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = ylim, xlim = l$xlim, add = T)
    title("(d) Potentially irrigated areas on potential cropland", outer = FALSE, cex.main = 1)

    # Legend
    par(mar = c(0, 0, 0, 0), mfrow = c(1, 1), fig = c(0, 1, 0, 1), bg = "transparent", new = TRUE)
    plot(potCropland,
         legend.only   = TRUE,
         horizontal    = TRUE,
         col           = legendcolor,
         zlim          = legendlimit,
         breaks        = legendbreaks,
         colNA         = "white",
         legend.args   = list(text = legendtitle, side = 3, font = 1, line = 1, cex = 1),
         axis.args     = list(cex.axis = 1, at = legendbreaks, line = 0, tick = FALSE, hadj = 0.5, padj = 0.5),
         add = T)

    dev.off()

  } else (outputtype == "png_long") {

    l$ylim <-  c(-5600000, 8260000)

    png(paste0("outputs/", filename, ".png"), height = 7000, width = 4000)

    par(mar = c(2, 2, 2, 2), mfrow = c(3, 1), bg = "transparent")

    ### Graph 1: LUH irrigated area that can be fulfilled
    plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)
    plot(LUHsus, bg = "transparent", ylim = l$ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "transparent",
         add          = T)
    plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
    plot(worldCountries, bg = "transparent", ylim = l$ylim, xlim = l$xlim, add = T)
    title("(a) Currently irrigated areas", outer = FALSE, cex.main = 6, line = -15)

    ### Graph 3: Potentially irrigated areas on current cropland
    plot(landMask, border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, bg = "transparent")
    plot(currCropland, ylim = l$ylim, xlim = l$xlim, bg = "transparent",
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "transparent",
         add          = T)
    plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
    plot(worldCountries, bg = "transparent", ylim = l$ylim, xlim = l$xlim, add = T)
    title("(b) Potentially irrigated areas on current cropland", outer = FALSE, cex.main = 6, line = -15)

    ### Graph 4: Potentially irrigated areas on potential cropland
    plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)
    plot(potCropland, ylim = l$ylim, xlim = l$xlim, bg = "transparent",
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "transparent",
         add          = T)
    plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
    plot(worldCountries, bg = "transparent", ylim = l$ylim, xlim = l$xlim, add = T)
    title("(c) Potentially irrigated areas on potential cropland", outer = FALSE, cex.main = 6, line = -15)

    # Legend
    plot(potCropland, bg = "transparent",
         legend.only   = TRUE,
         horizontal    = TRUE,
         col           = legendcolor,
         zlim          = legendlimit,
         breaks        = legendbreaks,
         colNA         = "transparent",
         legend.args   = list(text = legendtitle, side = 3, font = 1, line = 2, cex = 4),
         axis.args     = list(cex.axis = 4, at = legendbreaks, line = 0, tick = FALSE, hadj = 0.5, padj = 0.5),
         smallplot     = c(0.4, 0.75, 0.05, 0.1),
         add = T)

    dev.off()

  } else if (outputtype == "pdf_long") {

    pdf(paste0("outputs/", filename, ".pdf"), paper = "a4", pointsize = 9)

    par(mar = c(0, 0, 1, 0), mfrow = c(3, 1), bg = "transparent")

    ### Graph 2: LUH irrigated area that can be fulfilled
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim)
    plot(LUHsus, ylim = ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = ylim, xlim = l$xlim, add = T)
    title("(a) Currently irrigated areas", outer = FALSE, cex.main = 1)

    ### Graph 3: Potentially irrigated areas on current cropland
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim)
    plot(currCropland, ylim = ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = ylim, xlim = l$xlim, add = T)
    title("(b) Potentially irrigated areas on current cropland", outer = FALSE, cex.main = 1)

    ### Graph 4: Potentially irrigated areas on potential cropland
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim)
    plot(potCropland, ylim = ylim, xlim = l$xlim,
         legend       = FALSE,
         col          = legendcolor,
         zlim         = legendlimit,
         breaks       = legendbreaks,
         colNA        = "white",
         add          = T)
    plot(landMask, border = NA, col = "white", ylim = ylim, xlim = l$xlim, add = T)
    plot(worldCountries, ylim = ylim, xlim = l$xlim, add = T)
    title("(c) Potentially irrigated areas on potential cropland", outer = FALSE, cex.main = 1)

    # Legend
    plot(potCropland,
         legend.only   = TRUE,
         horizontal    = TRUE,
         col           = legendcolor,
         zlim          = legendlimit,
         breaks        = legendbreaks,
         colNA         = "white",
         legend.args   = list(text = legendtitle, side = 3, font = 1, line = 1, cex = 1),
         axis.args     = list(cex.axis = 1, at = legendbreaks, line = 0, tick = FALSE, hadj = 0.5, padj = 0.5),
         add = T)

    dev.off()

  } else {
    stop("Please select outputtype pdf or png")
  }
}
