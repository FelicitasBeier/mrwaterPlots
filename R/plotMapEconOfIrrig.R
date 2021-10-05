#' @title       plotMapEconOfIrrig
#' @description plot 3-layered map representing different irrigation gain thresholds
#'
#' @param version       subfolder of inputdata
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

plotMapEconOfIrrig <- function(version       = "MCfalse",
                               EFP           = "on") {

  ### Path ###
  inputdatapath   <- paste0(getwd(), "/inputdata/", version, "/")
  filename        <- "MapEconOfIrrig"

  ### Read in data ###
  irrigatableArea <- read.magpie(paste0(inputdatapath, "DemandCurve_pot_single.mz"))

  x0                <- irrigatableArea[, , "0"][, , EFP]
  x500              <- irrigatableArea[, , "500"][, , EFP]
  x1000             <- irrigatableArea[, , "1000"][, , EFP]

  out <- paste0("With threshold of 0, ", round(dimSums(x0, dim = 1)), " Mha could be irrigated with EFP=", EFP, ".",
                "With threshold of 500, ", round(dimSums(x500, dim = 1)), " Mha could be irrigated with EFP=", EFP, ".",
                "With threshold of 1000, ", round(dimSums(x1000, dim = 1)), " Mha could be irrigated with EFP=", EFP, ".")

  x0[x0 == 0]       <- NA
  x500[x500 == 0]   <- NA
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
  cellsize <- (111e3 * 0.5) * (111e3 * 0.5) * cos(cellsize$lat / 180 * pi) / 10000000000 # square meter -> Mha (1ha = 10000m^2)
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
  x0Share    <- x0 / cellsize
  x500Share  <- x500 / cellsize
  x1000Share <- x1000 / cellsize
  legendtitle  <- "Cellshare"
  legendrange  <- c(0, 1)
  legendbreaks <- seq(0, 1, 0.25)

  ### Create and save Legend plot ###
  x0_000_025 <- sum(x0[x0Share<0.25 & is.na(x500) & is.na(x1000)], na.rm = TRUE)
  x0_025_050 <- sum(x0[x0Share>=0.25 & x0Share<0.5 & is.na(x500) & is.na(x1000)], na.rm = TRUE)
  x0_050_075 <- sum(x0[x0Share>=0.5 & x0Share<0.75 & is.na(x500) & is.na(x1000)], na.rm = TRUE)
  x0_075_010 <- sum(x0[x0Share>=0.75 & is.na(x500) & is.na(x1000)], na.rm = TRUE)

  x500_000_025 <- sum(x500[x500Share<0.25 & is.na(x1000)], na.rm = TRUE)
  x500_025_050 <- sum(x500[x500Share>=0.25 & x500Share<0.5 & is.na(x1000)], na.rm = TRUE)
  x500_050_075 <- sum(x500[x500Share>=0.5 & x500Share<0.75 & is.na(x1000)], na.rm = TRUE)
  x500_075_010 <- sum(x500[x500Share>=0.75 & is.na(x1000)], na.rm = TRUE)

  x1000_000_025 <- sum(x1000[x1000Share<0.25], na.rm = TRUE)
  x1000_025_050 <- sum(x1000[x1000Share>=0.25 & x1000Share<0.5], na.rm = TRUE)
  x1000_050_075 <- sum(x1000[x1000Share>=0.5 & x1000Share<0.75], na.rm = TRUE)
  x1000_075_010 <- sum(x1000[x1000Share>=0.75], na.rm = TRUE)

  colors <- c("#e5f5e0", "#a1d99b", "#238b45", "#00441b",
              "#deebf7", "#9ecae1", "#4292c6", "#08306b",
              "#fee0d2", "#fc9272", "#cb181d", "#67000d")
  values <- c(x0_000_025, x0_025_050, x0_050_075, x0_075_010,
              x500_000_025, x500_025_050, x500_050_075, x500_075_010,
              x1000_000_025, x1000_025_050, x1000_050_075, x1000_075_010)

  .boxes <- function(position = c(0, 0, 0.25, 1), color = "#e5f5e0", text = "some Mha") {

    rect(position[1], position[2], position[3], position[4], border = "black", col = color)
    text(mean(c(position[1], position[3])), mean(c(position[2], position[4])), text, cex = 3.5)

  }

  # Transform magpie object to raster object
  l <- toolMapTransform(x = x0Share, projection =  "EqualEarth")
  x0Share             <- l$x1
  l <- toolMapTransform(x = x500Share, projection =  "EqualEarth")
  x500Share           <- l$x1
  l <- toolMapTransform(x = x1000Share, projection =  "EqualEarth")
  x1000Share          <- l$x1
  landMask       <- l$landMask
  worldCountries <- l$worldCountries

  ### Create and save Map plot ###
  png(paste0("outputs/", filename, ".png"), height = 2000, width = 4000)

  l$ylim <- c(-6084272, 8260000)
  l$xlim <- c(-12577316, 15581284)

  # Plot Map
  par(mar = c(15, 6.5, 4.1, 2.1))
  plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim)

  plot(x0Share, bg = "transparent", ylim = l$ylim, xlim = l$xlim,
       legend       = FALSE,
       col          = c("#e5f5e0", "#a1d99b", "#238b45", "#00441b"), # green color scale
       zlim         = legendrange,
       breaks       = legendbreaks,
       colNA        = NA,
       add = T)

  plot(x500Share, bg = "transparent", ylim = l$ylim, xlim = l$xlim,
       legend       = FALSE,
       col          = c("#deebf7", "#9ecae1", "#4292c6", "#08306b"), # blue color scale
       zlim         = legendrange,
       breaks       = legendbreaks,
       colNA        = NA,
       add          = T)

  plot(x1000Share, bg = "transparent", ylim = l$ylim, xlim = l$xlim,
       legend       = FALSE,
       col          = c("#fee0d2", "#fc9272", "#cb181d", "#67000d"), # red color scale
       zlim         = legendrange,
       breaks       = legendbreaks,
       colNA        = NA,
       add          = T)

  plot(landMask, bg = "transparent", border = NA, col = "white", ylim = l$ylim, xlim = l$xlim, add = T)
  plot(worldCountries, bg = "transparent", ylim = l$ylim, xlim = l$xlim, add = T)

  # Curve Plot
  par(fig = c(0.03, 0.35, 0.01, 0.5), bg = "white", new = TRUE,
      mar = c(7.1, 8.5, 4.1, 2.1), xaxs = "i", yaxs = "i")
  plot(x, y, bg = "white",
       col = ifelse(y > 1000, "#cb181d", ifelse(y > 500, "#08306b", "#238b45")),
       main = "",
       xlab = "",
       ylab = "",
       cex.lab = 3.5,
       type = "p", pch = 19, lty = 1, lwd = 2,
       bty = "l", yaxt = "n", xaxt = "n")
  s <- seq(length(a1)-1)
  segments(a1[s], b1[s], a1[s+1], b1[s+1], col= "#cb181d", lwd = 8)
  s <- seq(length(a2)-1)
  segments(a2[s], b2[s], a2[s+1], b2[s+1], col= "#08306b", lwd = 8)
  s <- seq(length(a3)-1)
  segments(a3[s], b3[s], a3[s+1], b3[s+1], col= "#238b45", lwd = 8)
  axis(side = 1, cex.axis = 4, hadj = 0.5, padj = 0.5,
       at = c(seq(from = 0, to = max(potCropland1$x),by = 200))) # x axis
  axis(side = 2, cex.axis = 4, hadj = 1, padj = 0.5,
       at = c(seq(from = 0, to = 3000, by = 500)), las = 1)      # y axis
  mtext("Irrigation yield gain (in USD/ha)", side = 2, line = 10, cex = 4.5)
  mtext("Irrigated area (in Mha)", side = 1, line = 7, cex = 4.5)
  title("Global \npotentially irrigated area \non potential cropland", cex.main = 4.5, line = -15, adj = 0.25)


  # Legend
  par(mar = c(7.5, 15.1, 4.1, 1.1), fig = c(0.45, 0.95, 0, 0.18), new = TRUE)
  plot(c(0, 1), c(0, 3), type = "n", xlab = "", ylab = "", axes = FALSE)
  axis(side = 1, col = NA, col.ticks = NA, at = legendbreaks, cex.axis = 3.5, padj = 0.6)
  axis(side = 2, col = NA, col.ticks = NA, at = c(1, 2, 3), cex.axis = 3.5,
       hadj = 1.1, padj = 0.9, c("0-500 USD/ha", "500-1000 USD/ha", ">1000 USD/ha"), las = 2)
  mtext("Cell share", side = 1, line = 6, cex = 4)
  i <- 0
  for (threshold in c(1, 2, 3)) {
    for (share in c(0.25, 0.5, 0.75, 1)) {
      i <- i + 1
      .boxes(position = c(share-0.25, threshold-1, share, threshold),
             color = colors[i], text = paste0(round(values[i]), " Mha"))

    }
  }

  dev.off()

  return(out)

}
