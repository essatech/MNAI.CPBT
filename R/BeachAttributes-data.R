#' Example Beach Attribute Polygons
#'
#' Sample beach attribute polygons for package examples. The beach attribute
#' polygons highlight the foreshore area around each geomorphically distinct
#' beach section. It is up to the user to define what constitutes a unique beach
#' section. Each beach section should be characterized with field attributes
#' columns for the foreshore slope (slope) as rise over run, the berm width (W),
#' berm height (B) and dune height (D) in meters, the sediment grain size in
#' millimeters (sediment) and the beach value per meter squared in dollars (V).
#' Note that this dataset has been reduced and simplified from
#' the original version and should only be used for demonstration purposes.
#'
#' @docType data
#'
#' @usage data(BeachAttributes)
#'
#' @format A simple features polygon layer of class \code{"sf"}; and
#' \code{"data.frame"};.
#'
#' @keywords datasets
#'
#' @examples
#' data(BeachAttributes)
#' head(BeachAttributes)
#' \donttest{
#' # Dune Height
#' plot(BeachAttributes["D"], main="Dune Height in meters")
#' }
"BeachAttributes"
