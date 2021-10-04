#' @title       plotScatterIrrigArea
#' @description plot scatter of observed and projected Irrigated Area
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
#' \dontrun{ plotScatterIrrigArea() }
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

plotScatterIrrigArea <- function(version = "MCfalse",
                                 observed = "cropareaLUH",
                                 projected = "irrigatableArea_potential",
                                 scenario = "off.ssp2",
                                 year = "y2010",
                                 region = "GLO") {
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

  # Regionmapping
  mapping        <- toolGetMappingCoord2Country()
  mapping$coords <- paste(mapping$coords, mapping$iso, sep = ".")
  regmap         <- toolGetMapping("regionmappingH12.csv")
  names(regmap)  <- c("Country", "iso", "reg")
  mapping        <- merge(regmap, mapping)

  df <- data.frame(coord = mapping$coords,
                   iaLUH = as.data.frame(iaLUH[mapping$coords, , ])$Value,
                   iaProjected = as.data.frame(iaProjected[mapping$coords, , ])$Value,
                   shrFulfilled = as.data.frame(shrFulfilled[mapping$coords, , ])$Value,
                   region = mapping$reg,
                   country = mapping$iso)

  if (any(region != "GLO")) {

    if (all(region %in% mapping$reg)) {
      df <- df[df$region == region, ]
    } else if (all(region %in% mapping$iso)) {
      df <- df[df$country == region, ]
    } else {
      stop("Selected country or region not in countrylist or regionmapping")
    }

  }

  # Mark places where physically available (renewable) water is not sufficient for current uses
  df1 <- df2 <- df
  df1$iaProjected[df1$shrFulfilled == 1 | is.na(df1$shrFulfilled)] <- NA #!= 0 == 1??????
  df2$iaProjected[df2$shrFulfilled < 1  & !is.na(df2$shrFulfilled)] <- NA

  modelstat <- lm(iaProjected ~ iaLUH, data = df, na.action = na.omit)
  rsquared  <- round(summary(modelstat)$r.squared, digits = 3)

  modelstat <- lm(iaProjected ~ iaLUH, data = df2, na.action = na.omit)
  rsquared1 <- round(summary(modelstat)$r.squared, digits = 3)

  p1 <- ggplot(df, aes(x = iaLUH, y = iaProjected)) +
                geom_point(size = 0.1, color = "black", na.rm = TRUE) +
                geom_abline(intercept = c(0, 0), slope = 1, color = "grey") +
                coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
                xlab("Actually Irrigated Area according to LUH (in Mha)") +
                ylab("Projected Irrigated Area according to Algorithm") +
                theme_bw() +
                theme(panel.background = element_rect(fill = "transparent", colour = NA),
                      plot.background  = element_rect(fill = "transparent", colour = NA),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      strip.background = element_rect(fill = "transparent", colour = NA),
                      strip.text       = element_text(color = "white")) +
    ggtitle(paste0("Region: ", region, "; ", "Rsquared: ", rsquared, "; Rsquared (w/o unfulfilled): ", rsquared1))

  p2 <- ggplot(df1, aes(x = iaLUH, y = iaProjected)) +
    geom_point(size = 0.1, color = "red", na.rm = TRUE) +
    geom_abline(intercept = c(0, 0), slope = 1, color = "grey") +
    coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
    xlab("Actually Irrigated Area according to LUH (in Mha)") +
    ylab("Projected Irrigated Area according to Algorithm") +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text       = element_text(color = "white")) +
    ggtitle(paste0("Region: ", region, "; ", "Rsquared: ", rsquared, "; Rsquared (w/o unfulfilled): ", rsquared1))

  out <- cowplot::ggdraw() + cowplot::draw_plot(p1) + cowplot::draw_plot(p2)

  return(out)
}
