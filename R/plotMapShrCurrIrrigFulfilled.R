#' @title       plotMapShrCurrIrrigFulfilled
#' @description plot map of share of current irrigation that can be fulfilled
#'              given renewable water availability of the algorithm
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
#' \dontrun{ plotMapShrCurrIrrigFulfilled() }
#'
#' @importFrom magclass read.magpie collapseNames
#'
#' @export
#'

plotMapShrCurrIrrigFulfilled <- function(version  = "MCfalse",
                                         year     = "y2010",
                                         scenario = "off.ssp2") {

  input <- "shrCurrIrrigFulfilled"

  if (length(year) > 1) {
    stop("Please select one year only for Map depicting the share of current
         irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  # Path
  inputdatapath <- paste0(getwd(), "/inputdata/", version, "/")

  # Read in data
  x <- collapseNames(read.magpie(paste0(inputdatapath, input, ".mz"))[, year, scenario])

  plotMap(x = x, filename = input, legendtitle = "Fulfilled Share",
          legendcolor = c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090",
                          "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

  # Report data
  fulfilledComAg <- collapseNames(read.magpie(paste0(inputdatapath, "fulfilledComAg.mz"))[, year, scenario])
  ComAg          <- dimSums(read.magpie(paste0(inputdatapath, "ComAg.mz"))[, year, "consumption"], dim = c(3))

  out <- paste0("For irrigation of all by LUH reported irrigated areas, ",
                round(dimSums(ComAg, dim = 1) / 1000), "km^3/yr of consumptive water would be required. Of these ",
                round(dimSums(fulfilledComAg[,,"currHuman_wc"], dim = 1)/1000),
                "km^3/yr can be fulfilled by local water resources in the ", scenario, " scenario.")

  return(out)

}
