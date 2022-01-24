#' @title Extract Elevation
#'
#' @description Extract elevation values along cross-shore profiles lines.
#'
#' @param cross_shore_profiles Cross-shore profile lines from samplePoints.
#' @param TopoBathy TopoBathy digital elevation model of class RasterLayer.
#'
#' @return Spatial points of class sf with extracted elevation values along
#' each cross-shore profile.
#'
#' @examples
#' \dontrun{
#' library(MNAI.CPBT)
#' data(Coastline)
#' shoreline_points <- samplePoints(
#'   Coastline = Coastline,
#'   ShorelinePointDist = 150, # where to sample points on shoreline
#'   BufferDist = 300.0, # Buffer distance in meters for vertical line
#'   RadLineDist = 3.0 # Line dist in km
#' )
#'
#' # Cross-shore profile lines
#' plot(sf::st_geometry(Coastline))
#' cross_shore_profiles <- shoreline_points[[2]]
#' plot(sf::st_geometry(cross_shore_profiles), add=TRUE)
#'
#' # Extract elevation along each profile
#' rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
#' TopoBathy <- raster::raster(rpath)
#' pt_elevs <- ExtractElev(cross_shore_profiles, TopoBathy)
#' plot(pt_elevs['elev'])
#' # plot(TopoBathy)
#' }
#' @export
ExtractElev <- function(
  cross_shore_profiles = NA,
  TopoBathy = NA
) {

  # Rename to match
  perp_lines <- cross_shore_profiles
  r <- TopoBathy
  sample_resolution <- 1
  PointResolution <- sample_resolution

  # to extract
  perp_lines_sp <- sf::as_Spatial(perp_lines)

  p1 <- sp::proj4string(r)
  p2 <- suppressWarnings(sp::proj4string(perp_lines_sp))

  print("setup complete (1/2)...")

  if(is.na(p1)) {
    print("Projection missing for raster...")
    print("Assume CRS is the same as perp lines...")
    raster::crs(r) <- p2
  }


  # match spatial projection of raster
  if (p1 != p2) {
    # assume raster is more costly to transform
    perp_lines_sp <- sp::spTransform(perp_lines_sp, p1)
  }

  print("setup complete (2/2)...")


  # list of all points and elevs
  all_pts <- list()

  # Loop through lines to extract values
  for (i in seq_len(nrow(perp_lines))) {

    this_line <- perp_lines[i, ]

    # sample points along line at desired resolution
    this_line_sf <- sf::st_as_sf(this_line)
    npts <- as.numeric(round(sf::st_length(this_line_sf) / PointResolution, 0))

    shoreline_pts <- sf::st_line_sample(x = this_line_sf,
                                    n = npts,
                                    type = "regular")


    # Get points
    shoreline_pts <- sf::st_sf(shoreline_pts)
    shoreline_pts <- sf::st_cast(shoreline_pts, "POINT")
    shoreline_pts <- sf::as_Spatial(shoreline_pts)

    # extract values for unique raster cells first - faster

    vals <- raster::extract(r, shoreline_pts)


    shoreline_pts$line_id <- this_line$id
    shoreline_pts$elev <- vals

    shoreline_pts <- sf::st_as_sf(shoreline_pts)


    all_pts[[i]] <- shoreline_pts

    print(paste0("Running transect ... ", this_line$id))

  } # end of loop through lines

  ptelev <- do.call("rbind", all_pts)

  return(ptelev)

}
