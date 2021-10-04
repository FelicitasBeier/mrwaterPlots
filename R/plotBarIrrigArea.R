#' @title       plotBarIrrigArea
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
#' @param region    Region to be displayed
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotBarIrrigArea() }
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

plotBarIrrigArea <- function(version   = "MCfalse",
                             observed  = "cropareaLUH",
                             projected = "irrigatableArea_potential",
                             scenario  = "off.ssp2",
                             year      = "y2010",
                             region    = "GLO") {
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


  # How much % of LUH irrigated area is irrigated by our algorithm?
  # (in terms of Mha)


  # (in terms of number of cells)

  # no of cells
  noCells <- iaLUH
  noCells[,,] <- 1
  # no. of cells with irrigation in LUH
  boolLUH <- iaLUH
  boolLUH[iaLUH != 0] <- 1
  # no. of cells with irrigation in Algorithm
  boolAlgo <- iaProjected
  boolAlgo[iaProjected != 0] <- 1
  # no. of cells with irrigation in both
  boolBoth <- iaProjected
  boolBoth[,,] <- 0
  boolBoth[iaProjected != 0 & iaLUH != 0] <- 1

  # Select region to be displayed
  noCellsR <- toolRegionSums(x = noCells, region = region)
  luhR     <- toolRegionSums(x = boolLUH, region = region)
  algoR    <- toolRegionSums(x = boolAlgo, region = region)
  bothR    <- toolRegionSums(x = boolBoth, region = region)

  df <- data.frame(Variable = c("LUH", "Algo", "Both"),
                   Value = c(as.numeric(luhR / noCellsR),
                             as.numeric(algoR / noCellsR),
                             as.numeric(bothR / noCellsR)))

  out <- ggplot(data = df, aes(x = Variable, y = Value, fill = Variable)) +
         ggtitle(paste0("Share of number of cells of the region ", region, " that are irrigated")) +
         geom_bar(stat = "identity") +
         coord_cartesian(ylim = c(0, 1)) +
         scale_x_discrete(limits = df$Variable)
  out

  return(out)
}
