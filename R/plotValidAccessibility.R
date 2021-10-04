#' @title       plotValidAccessibility
#' @description plot validation graph of EFR-Accessibility combinations
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
#' \dontrun{ plotValidAccessibility() }
#'
#' @importFrom magclass read.magpie collapseNames
#'
#' @export
#'

plotValidAccessibility <- function(version  = "MCfalse",
                                   input    = "shrCurrIrrigFulfilled",
                                   year     = "y2010",
                                   scenario = "off.ssp2") {

  if (length(year) > 1) {
    stop("Please select one year only for Map depicting the share of current
         irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  # Path
  inputdatapath <- paste0(getwd(), "/inputdata/", version, "/")

  # Read in data
  goodSmakhtinQ100 <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQ1.mz")), dim = 1)
  goodSmakhtinQ90  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQ0.9.mz")), dim = 1)
  goodSmakhtinQ75  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQ0.75.mz")), dim = 1)
  goodSmakhtinQ50  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQ0.50.mz")), dim = 1)
  goodSmakhtinCV2  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQCV2.mz")), dim = 1)

  fairSmakhtinQ100 <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQ1.mz")), dim = 1)
  fairSmakhtinQ90  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQ0.9.mz")), dim = 1)
  fairSmakhtinQ75  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQ0.75.mz")), dim = 1)
  fairSmakhtinQ50  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQ0.50.mz")), dim = 1)
  fairSmakhtinCV2  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQCV2.mz")), dim = 1)

  fairVMFQ100 <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ1.mz")), dim = 1)
  fairVMFQ90  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.9.mz")), dim = 1)
  fairVMFQ75  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.75.mz")), dim = 1)
  fairVMFQ50  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.50.mz")), dim = 1)
  fairVMFCV2  <- dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQCV2.mz")), dim = 1)



  # check if EFR = off is the same everywhere (i.e. for Smakthin, VMF)

# Note: for validation com_ag is turned off

# List of colors for accessibility: c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")

  # Read in data
  x <- collapseNames(read.magpie(paste0(inputdatapath, input, ".mz"))[, year, scenario])

  plotMap(x = x, filename = input, legendtitle = "Fulfilled Share",
          legendcolor = c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

}


potCropland1      <- collapseNames(read.magpie(paste0(inputdatapath, "DemandCurve_pot_single.mz"))[, , EFP])
potCropland1      <- dimSums(potCropland1, dim = 1)
getCells(potCropland1) <- "GLO"
potCropland1           <- data.frame(y = as.numeric(as.character(as.data.frame(potCropland1)$Data1)),
                                     x = as.data.frame(potCropland1)$Value, stringsAsFactors = FALSE)
# For curve plot
x  <- potCropland1$x
y  <- potCropland1$y


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
