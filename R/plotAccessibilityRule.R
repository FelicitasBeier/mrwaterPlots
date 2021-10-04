#' @title       plotAccessibilityRule
#' @description plot 3-layered map representing different irrigation gain thresholds
#'
#' @param version       subfolder of inputdata
#' @param multicropping multicropping activated (TRUE) or not (FALSE)
#' @param EFP           envrionmental flow protection activated ("on") or not ("off")
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotAccessibilityRule() }
#'
#' @importFrom magclass collapseNames
#'
#' @export
#'


plotAccessibilityRule <- function(lpjml, selectyears, climatetype, accessibilityrule) {

    # retrieve function arguments
    coeff  <- as.numeric(as.list(strsplit(accessibilityrule, split = ":"))[[1]][2])

    # Monthly Discharge from LPJmL (raw: including variation)
    monthly_discharge_lpjml <- calcOutput("LPJmL_new", subtype = "mdischarge",
                                          version = lpjml["natveg"], climatetype = climatetype,
                                          stage = "raw", aggregate = FALSE)

    # Extract years
    years         <- getYears(monthly_discharge_lpjml, as.integer = TRUE)
    if (class(selectyears) != "numeric") {
      selectyears <- as.numeric(gsub("y", "", selectyears))
    }

    for (y in selectyears) {

      # Long-term reference time frame for discharge variability calculation
      if ((y - 30) > years[1]) {
        longterm_period <- seq(y - 30, y, by = 1)
      } else {
        longterm_period <- seq(1980, 2010, by = 1)
      }
      monthly_discharge <- monthly_discharge_lpjml[, longterm_period, ]

      # Transform to array (faster calculation)
      monthly_discharge <- as.array(collapseNames(monthly_discharge))

      ### Variation Coefficient Method ###
      # Mean and standard deviation of discharge
      mean_discharge <- apply(monthly_discharge, MARGIN = 1, mean)
      std_discharge  <- apply(monthly_discharge, MARGIN = 1, sd)

      # Coefficient of Variation
      cv <- ifelse(mean_discharge > 0, std_discharge / mean_discharge, 0)
    }

  x <- seq(min(cv), max(cv), by = 1)

  ### Create and save plot ###
  pdf(paste0("outputs/AccessibilityRule.pdf"), height = 21.0, width = 29.7, paper = "a4r")
  par(mfrow = c(2, 1))

  boxplot(cv, horizontal = TRUE,
          ylim = c(0,19),
          #yaxs = "i",
          col = "lightblue")
  axis(side = 1, at = seq(min(cv), max(cv), by = 1)) # x axis
  title("(a)", adj = 0)

  plot(coeff^(-x),
       type = "l",
       xlab = "Coefficient of variation (std.deviation/mean)",
       ylab = "Accessible share of discharge",
       cex.lab = 1,
       xlim = c(min(x), max(x)), ylim = c(0, 1),
       #xaxs = "i",
       yaxs = "i",
       lwd = 3, col = "blue")
  axis(side = 1, at = seq(min(cv), max(cv), by = 1)) # x axis
  axis(side = 2, at = seq(0, 1, by = 0.1))  # y axis
  title("(b)", adj = 0)

  dev.off()

}



