#' Coastline
#'
#' Coastline spatial line segment for modeling. It is recommended to keep this
#' line segment simple and short. Remember that perpendicular cross-shore
#' transects will need to be generated from this line. If the geometry is
#' too complex with sharp curves, points, peninsulas and bays, cross-shore
#' profiles will point in undesirable directions.
#'
#' @docType data
#'
#' @usage data(Coastline)
#'
#' @format A simple feature LINESTRING of class sf and data.frame.
#'
#' @keywords datasets
#'
#' @examples
#' data(Coastline)
#' head(Coastline)
#' \donttest{
#' # Plot the coastline
#' plot(sf::st_geometry(Coastline))
#' }
"Coastline"
