#' Example Building Footprint Layer
#'
#' Sample building footprint polygons. Note that building footprint represent
#' all unique structures and there may be more that one structure on a given
#' property. The building footprint layer should consist of individual
#' non-overlapping polygons for each structure. The mandatory columns include
#' the Depth Damage Curve ID (DDID) and the structure value (VAL) in dollars.
#' The depth damage curve ID should be referenced to the HAZUS depth damage
#' curves (see \link{HAZUS}). Note that this dataset contains sample
#' values and should be used for demonstration purposes only..
#'
#' @docType data
#'
#' @usage data(Bldgs)
#'
#' @format A simple features polygon layer of class \code{"sf"}; and
#'  \code{"data.frame"};.
#'
#' @keywords datasets
#'
#' @examples
#' data(Bldgs)
#' head(Bldgs)
#' \donttest{
#' # Structure Values
#' plot(Bldgs["VAL"], main="Structure Values ($)")
#'
#' # Depth Damage Curve IDs from HAZUS tables.
#' plot(Bldgs["DDID"], main="Depth-Damage Curve IDs (HAZUS)")
#'
#' # See HAZUS reference tables
#' # ?MNAI.CPBT::HAZUS
#' }
"Bldgs"
