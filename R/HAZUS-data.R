#' HAZUS Depth-Damage Curves
#'
#' HAZUS depth-damage reference curves showing the relative damage that a
#' structure receives based on the flood depth from a given storm event.
#' Damage estimates range from 0 to 1 and are referenced to the current
#' structure value (replacement cost). A value of 0 indicates no damage
#' whereas a value of 1 indicates complete damage (equivalent to the
#' replacement cost of the structure). Note that curves apply to structures
#' only (not content) and may represent an oversimplification for individual
#' units. Depth-damage curves in this dataset are limited to short,
#' salt-water exposure from the USACE - New Orleans curve database.
#'
#' @docType data
#'
#' @usage data(HAZUS)
#'
#' @format A data frame with 1500 rows and 6 variables:
#' \describe{
#'   \item{DDID}{Depth-damage curve ID.}
#'   \item{Description}{Structure types applicable to depth-damage curve.}
#'   \item{damage}{Portional damage (0-1) to the structure from flood water
#'   relative to the total structure value (replacement cost).}
#'   \item{depth_m}{Floodwater depth in meters for a given structure}
#'   \item{Floors}{Number of floors for a given structure.}
#' }
#'
#' @keywords datasets
#'
#' @references HAZUS-MH User Manual FEMA
#'
#' @examples
#' data(HAZUS)
#' head(HAZUS)
#' \donttest{
#' # Plot depth-damage curve for a single structure type
#'
#' targetCurve = HAZUS[HAZUS$DDID == "RES1_147",]
#'
#' plot(targetCurve$depth_m, targetCurve$damage*100, type="b",
#'  xlab="Flood Water Depth (m)",
#'  ylab="Structure Damage Percent (%)",
#'  main = "Curve ID: RES1_147")
#'
#' }
"HAZUS"
