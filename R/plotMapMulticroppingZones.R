#' @title       plotMapMulticroppingZones
#' @description plot map of multiple cropping zones for rainfed, irrigated and
#'              the difference between the two
#'
#' @param version subfolder of inputdata
#' @param input   object containing the multiple cropping zone classification
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapMulticroppingZones() }
#'
#' @importFrom magclass read.magpie
#' @importFrom luplot plotmap2
#' @importFrom mrwater toolLPJarrayToMAgPIEmap
#' @importFrom ggplot2 theme element_blank element_rect element_text
#' @importFrom ggpubr ggarrange
#'
#' @export

plotMapMulticroppingZones <- function(version = "GT500",
                                      input = "multicroppingZones") {

  # Path
  inputdatapath <- paste0(getwd(), "/inputdata/", version, "/")

  # Read in data
  x             <- read.magpie(paste0(inputdatapath, input, ".mz"))

  # Manipulate data
  xIrr  <- collapseNames(x[, , "irrigated"])
  xRf   <- collapseNames(x[, , "rainfed"])
  xDiff <- xIrr - xRf

  # Create plot(s)
  pRf <- plotmap2(toolLPJarrayToMAgPIEmap(xRf),
                  title = "(A) Multiple Cropping Zones (rainfed)", legendname = "No. of seasons") +
    theme(title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(color = "white"))

  pIrr <- plotmap2(toolLPJarrayToMAgPIEmap(xIrr),
                  title = "(B) Multiple Cropping Zones (irrigated)", legendname = "No. of seasons") +
    theme(title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(color = "white"))

  pDiff <- plotmap2(toolLPJarrayToMAgPIEmap(xDiff),
                   title = "(C) Additional cropping seasons through irrigation", legendname = "No. of add. seasons") +
    theme(title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(color = "white"))

  out <- ggarrange(pRf, pIrr, pDiff, ncol = 1, nrow = 3,
                   common.legend = TRUE, legend = "right")

  return(out)
}
