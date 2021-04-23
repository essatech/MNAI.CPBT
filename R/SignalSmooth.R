#' @title Signal Smooth on profile
#'
#' @description Smooths a cross-shore elevation profile by averaging
#' values over a moving window.
#'
#' @param point_elev Cross-shore point elevations spatial points object
#' returned from ExtractElev.
#' @param SmoothParameter Numeric, smoothing window length as a percentage of 
#' the cross-shore profile length (0-100). A value of zero means no smoothing, but see
#' details below (recommended values are 5-20).
#'
#' @details A smoothed cross-shore profile is necessary for convergency of the
#' wave model. If the bottom surface topography extracted from the TopoBathy
#' DEM is overly rugged, the wave evolution model may produce unexpected
#' results as users may notice wave height increasing and decreasing along the
#' length of the profile. If this is observed try increasing the smoothing
#' parameter or omit that section of coastline from your analysis.
#'
#' @return An object of class sf and data.frame with the added column
#' elev_smooth of smoothed elevation values along the profile.
#' @examples
#' \dontrun{
#' library(MNAI.CPBT)
#' data(Coastline)
#' # Generate cross-shore profile lines along the coastline.
#' shoreline_points <- samplePoints(
#'   Coastline = Coastline,
#'   ShorelinePointDist = 150,
#'   BufferDist = 300.0,
#'   RadLineDist = 3.0
#' )
#'
#' # Extract elevation values along each profile
#' rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
#' TopoBathy <- raster::raster(rpath)
#' cross_shore_profiles <- shoreline_points[[2]]
#' pt_elevs <- ExtractElev(cross_shore_profiles, TopoBathy)
#'
#' # Run SignalSmooth function to smooth elevation profiles for
#' # wave model convergence
#' pt_elevs <- SignalSmooth(point_elev = pt_elevs,
#' SmoothParameter = 5)
#'
#' # Filter to just the first cross-shore profile
#' cs1 <- pt_elevs[pt_elevs$line_id == 2, ]
#' # Filter out any NA values beyond extent of TopoBathy DEM
#' cs1 <- cs1[!(is.na(cs1$elev)), ]
#' # Plot original and smoothed values
#' plot(1:nrow(cs1), cs1$elev, type = 'l', xlab = 'Distance (m)',
#' ylab = 'Elevation (m) [CD]', main = 'Cross-Shore Profile - 2')
#'
#' # Compare original elevations to smoothed values
#' points(1:nrow(cs1), cs1$elev_smooth, type = 'l', col='red')
#' }
#' @export
SignalSmooth <- function(
  point_elev = NA,
  SmoothParameter = 5
) {


  # If no smoothing return original values
  if(SmoothParameter == 0){
    print('The smoothing parameter should be between 5 to 20.')
    print('This will avoid complications with wave evolution model.')
    point_elev$elev_smooth <- point_elev$elev
    return(point_elev)
  }


  #place holder
  point_elev$elev_smooth <- NA

  uids <- unique(point_elev$line_id)

  for(i in 1:length(uids)) {

    this_id <- uids[i]

    # Select index and excluda all NA points
    #this_index <- which(point_elev$line_id == this_id)
    this_index <- which(point_elev$line_id == this_id
                        & !(is.na(point_elev$elev)))

    shoreline_pts <- point_elev[this_index, ]

    # Window size
    SmoothValue <- (SmoothParameter / 100.0) * nrow(shoreline_pts)

    Dorig <- shoreline_pts$elev

    # smooth function
    Tempy1 <- SignalSmooth_smooth(x = Dorig,
                                  window_len = round(SmoothValue, 2),
                                  mm='flat')

    # plot(Dorig, type = 'l')
    # points(Tempy1, type = 'l', col = 'red')

    shoreline_pts$elev_smooth <- Tempy1

    point_elev$elev_smooth[this_index] <- shoreline_pts$elev_smooth

  }

  return(point_elev)

}
