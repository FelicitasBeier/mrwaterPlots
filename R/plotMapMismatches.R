#' @title       plotMapMismatches
#' @description plot bar plot to compare projected irrigated area with
#'              observed irrigated area from LUH
#'
#' @param version   subfolder of inputdata
#' @param observed  object with observed values
#'                  (irrigated area reported by LUH)
#' @param projected object with projected values
#'                  (irrigated area from mrwater analysis)
#' @param scenario  EFP scenario and non-agricultural water use scenario
#'                  separated by "."
#' @param year      Year for which scatter plot is displayed
#' @param output    Algo_noLUH (where we irrigate, but LUH not),
#'                  LUH_noAlgo (where LUH irrigates, but Algorithm not), LUH, Algorithm
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapMismatches() }
#'
#' @importFrom magclass read.magpie dimSums getCells collapseNames
#' @importFrom madrat toolGetMapping
#' @importFrom luplot plotmap2
#' @importFrom mrcommons toolGetMappingCoord2Country
#' @importFrom mrwater toolLPJarrayToMAgPIEmap
#' @importFrom ggplot2 ggplot theme element_blank element_rect element_text geom_point geom_abline coord_equal xlab ylab ggtitle aes
#' @importFrom ggpubr ggarrange
#'
#' @export

plotMapMismatches <- function(version = "GT500",
                             observed = "cropareaLUH",
                             projected = "irrigatableArea_potential",
                             scenario = "off.ssp2",
                             year = "y2010",
                             output = "Mismatch") {
  ### Path ###
  inputdatapath <- paste0(getwd(), "/inputdata/", version, "/")

  ### Read in data ###
  # observed data
  iaLUH       <- read.magpie(paste0(inputdatapath, observed, ".mz"))
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  map                              <- toolGetMappingCoord2Country()
  getCells(iaLUH)           <- paste(map$coords, map$iso, sep=".")
  names(dimnames(iaLUH))[1] <- "x.y.iso"
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

  # projected data
  iaProjected <- read.magpie(paste0(inputdatapath, projected, ".mz"))

  # current irrigation water that can be met by renewable water resources
  shrFulfilled       <- read.magpie(paste0(inputdatapath, "shrCurrIrrigFulfilled.mz"))

  # Manipulate data
  iaLUH        <- collapseNames(dimSums(iaLUH[, , "irrigated"],
                                        dim = 3))
  iaProjected  <- collapseNames(dimSums(iaProjected[, year, scenario][, , "irrigatable"],
                                        dim = "season"))
  shrFulfilled <- collapseNames(shrFulfilled[, year, scenario])

  # Where do we irrigate, but LUH not:
  iaMismatch <- iaProjected
  iaMismatch[iaLUH != 0] <- 0

  # Where does LUH irrigate, but we do not:
  iaMismatch2 <- iaLUH
  iaMismatch2[iaProjected != 0]  <- 0
  iaMismatch2[shrFulfilled != 1] <- 0

  if (output == "Algo_noLUH") {
    out <- plotmap2(toolLPJarrayToMAgPIEmap(iaMismatch),
                    title = "Areas where we project irrigation, but LUH does not irrigate", legendname = "Mha")
  } else if (output == "LUH_noAlgo") {
    out <- plotmap2(toolLPJarrayToMAgPIEmap(iaMismatch2),
                    title = "Areas where LUH projects irrigation, but we don't", legendname = "Mha")
  } else if (output == "LUH") {
    out <- plotmap2(toolLPJarrayToMAgPIEmap(iaLUH),
                    title = "Currently Irrigated Area (LUH)", legendname = "Mha")
  } else if (output == "Algorithm") {
    out <- plotmap2(toolLPJarrayToMAgPIEmap(iaProjected),
                    title = "Potentially Irrigated Area (our study)", legendname = "Mha")
  } else {
    stop("Choose Algo_noLUH, LUH_noAlgo, LUH or Algorithm for output argument")
  }

  return(out)
}
