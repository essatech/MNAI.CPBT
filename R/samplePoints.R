#' @title sample Points along the shoreline
#'
#' @description Sample points along the coastline at specified intervals.
#'
#' @param Coastline Coastline LINESTRING of class sf and data.frame.
#' @param ShorelinePointDist Numeric. Spacing between cross-shore profile lines (meters).
#'  Note that it is recommended to keep this number as large as possible for
#'  your area of interest to speed up processing time.
#' @param BufferDist Numeric. Buffer search distance (meters) used to identify
#' the perpendicular angle of each cross-shore profile line from the coastline.
#' @param RadLineDist Numeric. Radial line distance (kilometers) of the cross-shore profiles. This value determines how for offshore (and onshore) the cross-shore profiles should extend (recommended values are 1 - 3 km).
#'
#' @return A list object of shoreline points and cross-shore profile lines
#' along the coastline.
#' @examples
#' \dontrun{
#' data(Coastline)
#' shoreline_points <- samplePoints(
#'   Coastline = Coastline,
#'   ShorelinePointDist = 150, # where to sample points on shoreline
#'   BufferDist = 300.0, # Buffer distance in meters for vertical line
#'   RadLineDist = 3.0 # Line dist in km
#' )
#' # Coastline
#' plot(sf::st_geometry(Coastline))
#' # Shoreline points
#' plot(sf::st_geometry(shoreline_points[[1]]), add=TRUE)
#' # Cross-shore profiles
#' plot(sf::st_geometry(shoreline_points[[2]]), add=TRUE)
#' }
#' @export
samplePoints <- function(
  Coastline = NA,
  ShorelinePointDist = 200,
  BufferDist = 300.0,
  RadLineDist = 3.0
) {

  # Generate points along shoreline
  shoreline <- Coastline
  shoreline <- sf::st_cast(shoreline, "LINESTRING")[1]

  # Number of points to sample
  npts <- as.numeric(round(sf::st_length(shoreline) / ShorelinePointDist, 0))

  shoreline_pts <- sf::st_line_sample(x = shoreline,
                                  n = npts,
                                  type = "regular")

  # Get perpendicular line angle
  shoreline_pts <- sf::st_sf(shoreline_pts)
  shoreline_pts <- sf::st_cast(shoreline_pts, "POINT")

  # Perpendicular angle
  shoreline_pts$bearing <- NA

  # Draw point buffers
  bbuf <- sf::st_buffer(shoreline_pts, ShorelinePointDist)
  bbuf <- sf::st_cast(bbuf, "LINESTRING", warn = FALSE)

  # List of perp lines
  all_lines <- list()

  # Loop through points
  for (i in seq_len(nrow(shoreline_pts))) {

    this_point <- shoreline_pts[i, ]
    this_buff <- bbuf[i, ]

    # buffer on shoreline
    sli <- suppressWarnings(sf::st_intersection(this_buff, shoreline))

    # Get point coordinates
    tp4326 <- sf::st_transform(this_point, 4326)
    sli4326 <- sf::st_transform(sli, 4326)

    #----------------------------------------------------
    # Get perpendicular bearing from points
    #----------------------------------------------------

    c1 <- sf::st_coordinates(tp4326)
    c2 <- sf::st_coordinates(sli4326)

    if (nrow(c2) == 1) {
      # edge
      b <- geosphere::bearing(c1[, c("X", "Y")], c2[, c("X", "Y")])
      next
    } else {
      # center
      b1 <- geosphere::bearing(c1[, c("X", "Y")], c2[1, c("X", "Y")])
      b2 <- geosphere::bearing(c1[, c("X", "Y")], c2[2, c("X", "Y")])
      b <- mean(c(b1, b2), na.rm = TRUE)
    }

    shoreline_pts$bearing[i] <- b


    #----------------------------------------------------
    # Draw lines from shoreline points
    #----------------------------------------------------

    # direction 1
    dest1 <- geosphere::destPoint(c1[, c("X", "Y")], b, d = RadLineDist * 1000)

    # reciprocal bearig
    if (b < 180) {
      br <- b + 180
    } else {
      if (b == 180) {
        br <- 360
      } else {
        br <- 180 - (360 - b)
      }
    }

    # direction 2 - reciprocal bearing
    dest2 <- geosphere::destPoint(c1[, c("X", "Y")], br, d = RadLineDist * 1000)

    # Make into line
    new_line <- sf::st_linestring(x = rbind(dest1, dest2), dim = "XY")
    new_line <- sf::st_sfc(new_line)
    new_line <- sf::st_sf(new_line)
    sf::st_crs(new_line) <- 4326
    new_line$id <- i

    # add to list
    all_lines[[i]] <- new_line



  } # end of get point bearing


  all_line_out <- do.call("rbind", all_lines)
  # match projection
  all_line_out <- sf::st_transform(all_line_out, sf::st_crs(shoreline_pts))


  # build return objects

  ret_obj <- list()

  # filter out edge points
  shoreline_pts <- shoreline_pts[which(!(is.na(shoreline_pts$bearing))), ]

  ret_obj[[1]] <- shoreline_pts
  ret_obj[[2]] <- all_line_out

  return(ret_obj)

}
