#' Vegetation Polygons
#'
#' Vegetation polygons for submerged vegetation. Note that these vegetation
#' polygons were prepared for demonstration purposes only and do not reflect
#' actual eelgrass distributions.
#'
#' @docType data
#'
#' @usage data(Vegetation)
#'
#' @format A simple feature collection of eelgrass polygons.
#' \describe{
#'   \item{hc}{Blade height in meters}
#'   \item{N}{Shoot density as number of shoots per meter squared.}
#'   \item{d}{Blade width in meters.}
#'   \item{Type}{Either 'Eelgrass', 'Kelp' or 'Marsh'.}
#'   \item{Cd}{Drag coefficent as per Guannel et al 2015.}
#' }
#'
#' @keywords datasets
#'
#' @references Guannel et al (2015). Integrated modeling framework to quantify
#' the coastal protection services supplied by vegetation. Journal of
#' Geophysical Research: Oceans, 120(1), 324-345.
#' (\href{doi/10.1002/2014JC009821}{Article DOI})
#'
#' @examples
#' \donttest{
#' require(sf)
#' data(Vegetation)
#' head(Vegetation)
#' # Plot the eelgrass patch height
#' plot(Vegetation["hc"])
#' }
"Vegetation"
