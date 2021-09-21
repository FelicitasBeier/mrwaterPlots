#' @title       plotMapYieldgainPotential
#' @description plot map of potential yield gains through irrigation
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
#' \dontrun{ plotMapYieldgainPotential() }
#'
#' @importFrom magclass read.magpie
#' @importFrom luplot plotmap2
#' @importFrom mrwater toolLPJarrayToMAgPIEmap
#' @importFrom ggplot2 theme element_blank element_rect element_text
#'
#' @export
#'

plotMapYieldgainPotential <- function(version = "GT500",
                                      input = "yieldgain_USDha",
                                      year  = "y2010",
                                      scenario = "off.ssp2") {

  if (length(year) > 1) {
    stop("Please select one year only for Map depicting the share of current
         irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  unit <- strsplit(input, "_")[[1]][2]
  unit <- paste0("USD per ", gsub("[A-Z]", "", unit))

  # Path
  inputdatapath <- paste0(getwd(), "/inputdata/", version, "/")

  # Read in data
  x <- dimSums(read.magpie(paste0(inputdatapath, input, ".mz")), dim = 3)[, year, ]

  # relevant map area
  area         <- read.magpie(paste0(inputdatapath, "avlIrrigarea_pot.mz"))[, year, ]
  x[area == 0] <- NA

  # plot output
  out <- plotmap2(toolLPJcell2MAgPIEcell(x), labs = FALSE, sea = FALSE,
                  land_colour = "transparent", title = "", legend_range = c(0, 3000)) +
    scale_fill_continuous(name = unit, limits = c(0, 3000),
                          labels = c(0, 500, 1000, 1500, 2000, 2500, 3000), breaks =  c(0, 500, 1000, 1500, 2000, 2500, 3000),
                          low = "white", high = "darkred", na.value = "grey") +
    theme(title            = element_blank(),
          legend.position  = c(0.1, 0.3),
          legend.title = element_text(size = 20), legend.text = element_text(size = 18),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y     = element_blank(),
          axis.title.x     = element_blank(),
          axis.ticks.y     = element_blank(),
          axis.ticks.x     = element_blank(),
          axis.text.y      = element_blank(),
          axis.text.x      = element_blank(),
          strip.background = element_rect(fill = "transparent", colour=NA),
          strip.text       = element_text(color = "white"))

  return(out)
}
