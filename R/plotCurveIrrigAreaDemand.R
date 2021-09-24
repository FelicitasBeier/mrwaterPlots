#' @title       plotCurveIrrigAreaDemand
#' @description plot minimum monetary yield gain per ha achieved
#'              on related potentially irrigated area
#'
#' @param version        subfolder of output results
#' @param EFP            environmental flow policy "on" or "off
#' @param aggregation    "countries" or "basins"
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @importFrom magclass dimSums collapseNames read.magpie
#' @importFrom ggplot2 ggplot facet_wrap geom_line geom_point aes ggtitle xlab ylab theme theme_bw geom_text xlim ylim ggsave scale_linetype_manual scale_color_manual scale_shape_manual
#' @importFrom mrwater toolSelectRiverBasin
#'
#' @export

plotCurveIrrigAreaDemand <- function(version = "GT500",
                                     EFP = "on",
                                     aggregation = "countries") {

  ### Path ###
  inputdatapath <- paste0(getwd(), "/inputdata/", version, "/")

  ### Read in data ###
  currCropland1   <- collapseNames(read.magpie(paste0(inputdatapath, "DemandCurve_curr_single.mz"))[, , EFP])
  potCropland1    <- collapseNames(read.magpie(paste0(inputdatapath, "DemandCurve_pot_single.mz"))[, , EFP])
  currYieldgain1  <- read.magpie(paste0(inputdatapath, "yieldgainarea_curr.mz"))
  potYieldgain1   <- read.magpie(paste0(inputdatapath, "yieldgainarea_pot.mz"))
  currentLUH      <- dimSums(read.magpie(paste0(inputdatapath, "cropareaLUH.mz"))[, , "irrigated"], dim = "MAG")

  ### Aggregate and rearrange data ###
  if (aggregation == "countries") {

    df   <- as.data.frame(dimSums(currCropland1, dim = c(1.1, 1.2)))
    df$IrrigArea <- "Potentially Irrigated Area \non current cropland \n(given local water constraints)"

    tmp    <- as.data.frame(dimSums(potCropland1, dim = c(1.1, 1.2)))
    tmp$IrrigArea <- "Potentially Irrigated Area \non potential cropland \n(given local water constraints)"
    df <- rbind(df, tmp)

    tmp    <- as.data.frame(dimSums(currYieldgain1, dim = c(1.1, 1.2)))
    tmp$IrrigArea <- "Areas with irrigation yield gain \non current cropland \n(without water constraints)"
    df <- rbind(df, tmp)

    tmp    <- as.data.frame(dimSums(potYieldgain1, dim = c(1.1, 1.2)))
    tmp$IrrigArea <- "Areas with irrigation yield gain \non potential cropland \n(without water constraints)"
    df <- rbind(df, tmp)

    df <- data.frame(Region = df$Region,
                     GT = as.numeric(as.character(df$Data1)),
                     Group = df$IrrigArea,
                     IrrigArea = df$Value,
                     stringsAsFactors = FALSE)
    df$LUH <- rep(as.array(dimSums(currentLUH, dim = c(1.1, 1.2))), length(df$Group) / length(as.array(dimSums(currentLUH, dim = c(1.1, 1.2)))))

    df <- df[df$Region %in% c("CHN", "IND", "AUS", "USA", "MEX", "BRA", "ITA", "TUR", "SDN"), ]
    df$Region <- factor(df$Region, levels = c("CHN", "IND", "AUS", "USA", "MEX", "BRA", "ITA", "TUR", "SDN"))

  } else if (aggregation == "basins") {

    basinList <- c("Mississippi", "Nile", "Chang Jiang", "Ganges", "Indus",
                   "Murray", "Huang He", "Colorado", "Danube", "Mekong",
                   "Parana", "Syr-Darya", "Amu-Darya", "Orange", "Guadalquivir")

    selectedBasins <- c("Chang Jiang", "Huang He", "Mekong",
                        "Mississippi", "Colorado", "Murray",
                        "Parana", "Ganges", "Indus")
    framestructure <- as.data.frame(new.magpie(cells_and_regions = selectedBasins,
                                               years = getYears(currCropland1),
                                               names = getNames(currCropland1)))
    df <- framestructure
    for (b in selectedBasins) {
      df$Value[df$Region == b] <- dimSums(currCropland1[toolSelectRiverBasin(basinname = b), , ], dim = 1)
    }
    df$Group <- "Potentially Irrigated Area \non current cropland \n(given local water constraints)"

    tmp <- framestructure
    for (b in selectedBasins) {
      tmp$Value[tmp$Region == b] <- dimSums(potCropland1[toolSelectRiverBasin(basinname = b), , ], dim = 1)
    }
    tmp$Group <- "Potentially Irrigated Area \non potential cropland \n(given local water constraints)"
    df <- rbind(df, tmp)

    tmp <- framestructure
    for (b in selectedBasins) {
      tmp$Value[tmp$Region == b] <- dimSums(currYieldgain1[toolSelectRiverBasin(basinname = b), , ], dim = 1)
    }
    tmp$Group <- "Areas with irrigation yield gain \non current cropland \n(without water constraints)"
    df <- rbind(df, tmp)

    tmp <- framestructure
    for (b in selectedBasins) {
      tmp$Value[tmp$Region == b] <- dimSums(potYieldgain1[toolSelectRiverBasin(basinname = b), , ], dim = 1)
    }
    tmp$Group <- "Areas with irrigation yield gain \non potential cropland \n(without water constraints)"
    df <- rbind(df, tmp)

    df <- data.frame(Region = df$Region,
                     GT = as.numeric(as.character(df$Data1)),
                     Group = df$Group,
                     IrrigArea = df$Value,
                     stringsAsFactors = FALSE)

    tmp <- framestructure
    for (b in selectedBasins) {
      tmp$Value[tmp$Region == b] <- dimSums(currentLUH[toolSelectRiverBasin(basinname = b), , ], dim = 1)
    }
    df$LUH <- tmp$Value
    df$Region <- factor(df$Region, levels = selectedBasins)

  } else {
    stop("Chosen aggregation is not yet implemented")
  }

  ### Plot ###
  out <- ggplot(df, aes(x = IrrigArea, y = GT, color = Group, group = Group, shape = Group)) +
                geom_point(size = 3) +
                geom_point(aes(x = LUH, y = 0), size = 5, color = "black", shape = 17) +
                geom_line(aes(linetype = Group), size = 1.5) +
                facet_wrap(~Region, scales = "free_x") +
                scale_color_manual(values = c("#1b9e77", "#7570b3", "#1b9e77", "#7570b3")) +
                scale_linetype_manual(values = c("dashed", "dashed", "solid", "solid")) +
                scale_shape_manual(values = c(19, 19, 15, 15)) +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0)) +
                xlab("Area (in Mha)") + ylab("Yield return to irrigation (in USD/ha)") +
                theme_bw() +
                theme(legend.title         = element_blank(),
                      legend.position      = "bottom",
                      legend.text          = element_text(size = 12),
                      legend.key.width     = unit(2.8, "line"),
                      legend.justification = c(0, 1),
                      legend.margin        = margin(t = 0, r = 0, b = 0, l = -77, unit = "pt"),
                      strip.text         = element_text(size = 18),
                      strip.background   = element_rect(fill = "#f6f6f6"),
                      plot.title         = element_text(size = 18),
                      axis.text.x        = element_text(size = 16),
                      axis.title.x       = element_text(size = 18),
                      axis.text.y        = element_text(size = 16),
                      axis.title.y       = element_text(size = 18),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank())

  ### Save Plot ###
  ggsave(paste0(aggregation, "_Curves.pdf"), plot = out, width = 297, height = 210, units = "mm")

  return(out)
}
