#' @title       toolMapTransforms
#' @description transforms magpie object such that it can be plotted as raster
#'
#' @param x             magpie object
#' @param projection    world map projection to be displayed in map
#'                      (EqualEarth, Robinson, Mollweide, ...)
#'
#' @return map of magpie cells
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ plotMap(x) }
#'
#' @importFrom magclass as.RasterBrick
#' @importFrom rgdal readOGR
#' @importFrom raster crs proj4string projectRaster
#' @importFrom sp spTransform
#' @importFrom rgeos gDifference
#' @importFrom sf st_difference
#'
#' @export
#'

toolMapTransform <- function(x, projection = "EqualEarth") {

  # Transform magpie object to raster object
  x0 <- as.RasterBrick(x)

  # Standard projection of MAgPIE object
  LatLonProj     <- "+proj=longlat +datum=WGS84"
  #LatLonProj     <- "+proj=longlat +datum=WGS84 +no_defs"

  if (projection == "EqualEarth") {

    # Equal Earth projection
    NewProj <- "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

    # Select mapping extent
    ylim <- c(-6584272, 8260000)
    xlim <- c(-12577316, 15581284)

  } else if (projection == "Mollweide") {

    # Mollweide projection
    NewProj  <- "+proj=moll +lon_wrap=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84"

    #-#-# NOTE: There is still a warning showing up. Needs to be fixed #-#-#
    #-#-# 40689 projected point(s) not finite #-#-#

    # Select mapping extent
    ylim <- c(-6334552, 9329048)
    xlim <- c(-15338047, 18335153)

  } else if (projection == "Robinson") {

    # Robinson projection
    NewProj  <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

    #-#-# NOTE: There is still a warning showing up. Needs to be fixed #-#-#
    #-#-# 40689 projected point(s) not finite #-#-#

    # Select mapping extent
    ylim <- c(-8869345, 8892655)
    xlim <- c(-17287235, 17310365)

  } else {
    stop("Chosen projection is not yet implemented. Please go ahead and include
         it into this function.")
  }

  # Reprojected raster object to be plotted
  x1 <- projectRaster(x0, crs = crs(NewProj), over = TRUE)

  # Read in shapefile and transform to selected projection
  wdir      <- "C:\\Users\\beier\\Documents\\Modelle\\world_borders\\" #### Put shape file into mrwaterPlots extdata
  worldCountries              <- readOGR(paste0(wdir, "world_country_admin_boundary_shapefile_with_fips_codes.shp"))
  proj4string(worldCountries) <- crs(LatLonProj)
  worldCountries              <- as(worldCountries, "SpatialPolygons")
  worldCountries              <- spTransform(worldCountries, crs(NewProj))

  # Create land mask for pretty continent borders
  worldCountries        <- as(worldCountries, "SpatialPolygons")
  landMask              <- as(extent(worldCountries), "SpatialPolygons")
  proj4string(landMask) <- proj4string(worldCountries)
  landMask              <- gDifference(landMask, worldCountries)
  #### NOTE: Will deprecate soon! use alternative function for gDifference (e.g. st_difference)

  ### Save objects for plotting ###
  out <- list(x1 = x1,
              landMask = landMask, worldCountries = worldCountries,
              xlim = xlim, ylim = ylim)

  return(out)

}
