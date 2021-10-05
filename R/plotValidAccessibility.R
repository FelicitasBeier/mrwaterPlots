#' @title       plotValidAccessibility
#' @description plot validation graph of EFR-Accessibility combinations
#'
#' @param version   subfolder of inputdata
#' @param year      Year to be shown in plot
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
                                   year     = "y2010") {

  # Note: for validation com_ag is turned off!

  if (length(year) > 1) {
    stop("Please select one year only for Map depicting the share of current
         irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  # Path
  inputdatapath <- paste0(getwd(), "/inputdata/", version, "/")
  outputdatapath <- paste0(getwd(), "/outputs/")

  # Read in data
  goodSmakhtinQ100 <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQ1.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "goodSmakhtinQ100")
  goodSmakhtinQ90  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQ0.9.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "goodSmakhtinQ90")
  goodSmakhtinQ75  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQ0.75.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "goodSmakhtinQ75")
  goodSmakhtinQ50  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodQ0.5.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "goodSmakhtinQ50")
  goodSmakhtinCV2  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtingoodCV2.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "goodSmakhtinCV2")

  fairSmakhtinQ100 <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQ1.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairSmakhtinQ100")
  fairSmakhtinQ90  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQ0.9.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairSmakhtinQ90")
  fairSmakhtinQ75  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQ0.75.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairSmakhtinQ75")
  fairSmakhtinQ50  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairQ0.5.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairSmakhtinQ50")
  fairSmakhtinCV2  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandSmakhtinfairCV2.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairSmakhtinCV2")

  fairVMFQ100 <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ1.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairVMFQ100")
  fairVMFQ90  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.9.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairVMFQ90")
  fairVMFQ75  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.75.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairVMFQ75")
  fairVMFQ50  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.5.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairVMFQ50")
  fairVMFCV2  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairCV2.mz")), dim = 1)[,,"on"]), dim = 3.1, nm = "fairVMFCV2")

  OffQ100 <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ1.mz")), dim = 1)[,,"off"]), dim = 3.1, nm = "OffQ100")
  OffQ90  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.9.mz")), dim = 1)[,,"off"]), dim = 3.1, nm = "OffQ90")
  OffQ75  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.75.mz")), dim = 1)[,,"off"]), dim = 3.1, nm = "OffQ75")
  OffQ50  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairQ0.5.mz")), dim = 1)[,,"off"]), dim = 3.1, nm = "OffQ50")
  OffCV2  <- add_dimension(collapseNames(dimSums(read.magpie(paste0(inputdatapath, "ValidPotcroplandVMFfairCV2.mz")), dim = 1)[,,"off"]), dim = 3.1, nm = "OffCV2")

  data <- as.data.frame(mbind(goodSmakhtinQ100, goodSmakhtinQ90, goodSmakhtinQ75, goodSmakhtinQ50, goodSmakhtinCV2,
                              fairSmakhtinQ100, fairSmakhtinQ90, fairSmakhtinQ75, fairSmakhtinQ50, fairSmakhtinCV2,
                              OffQ100, OffQ90, OffQ75, OffQ50, OffCV2,
                              fairVMFQ100, fairVMFQ90, fairVMFQ75, fairVMFQ50, fairVMFCV2))
  data <- data.frame(EFR = data$Data1, Accessibility = data$Data1, GT = as.numeric(as.character(data$Data2)), IrrigArea = data$Value)
  data$Accessibility <- gsub("goodSmakhtin", "", data$Accessibility)
  data$Accessibility <- gsub("fairSmakhtin", "", data$Accessibility)
  data$Accessibility <- gsub("fairVMF", "", data$Accessibility)
  data$Accessibility <- gsub("Off", "", data$Accessibility)
  data$EFR <- gsub("Q100", "", data$EFR)
  data$EFR <- gsub("Q90", "", data$EFR)
  data$EFR <- gsub("Q75", "", data$EFR)
  data$EFR <- gsub("Q50", "", data$EFR)
  data$EFR <- gsub("CV2", "", data$EFR)

  # Physical potential comparison
  out <- ggplot(data[data$GT==0,], aes(y = IrrigArea, x = reorder(EFR, -IrrigArea), fill = reorder(Accessibility, -IrrigArea))) +
              geom_bar(position = "dodge", stat = "identity") +
              scale_fill_brewer(palette = "Dark2") +
              labs(x = "EFR Method", y = "Potentially Irrigated Area (in Mha)", fill = "Accessibility Rule:") +
              theme_minimal() +
              theme(legend.position    = "bottom",
                    legend.title       = element_text(size = 20),
                    legend.text        = element_text(size = 18),
                    axis.text.x        = element_text(size = 18),
                    axis.text.y        = element_text(size = 18),
                    axis.title.y       = element_text(size = 20),
                    axis.title.x       = element_text(size = 20))

  ### Save Plot ###
  ggsave(paste0(outputdatapath, "Validation_EFRAccessibility.pdf"), plot = out, width = 297, height = 210, units = "mm")

}
