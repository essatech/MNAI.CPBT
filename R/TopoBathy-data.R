#' Example TopoBathy Raster Layer
#'
#' Sample TopoBathy (topographic and bathymetric) raster layer
#' used for function examples. 0 meters is referenced to the
#' Mean Lower Low Water (MLLW) level (Chart Datum).
#' Note that this dataset has been reduced and simplified from
#' the original version and should only be used for demonstration purposes.
#' See the source link below to download relevant data for your region of
#' interest.
#'
#' @docType data
#'
#' @usage data(TopoBathy)
#'
#' @format An object of class \code{"RasterLayer"};.
#'
#' @keywords datasets
#'
#' @references DFO - Canadian Hydrographic Service Non-Navigational (NONNA)
#' Bathymetric Data
#'
#' @source \href{https://www.open.canada.ca/en}{CHS Bathymetry Data Download}
#'
#' @examples
#' data(TopoBathy)
#' \donttest{
#' raster::plot(TopoBathy)
#' }
"TopoBathy"
