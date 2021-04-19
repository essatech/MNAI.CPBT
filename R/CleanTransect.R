#' @title CleanTransect
#'
#' @description Utility function to clean transects and remove anomalies that
#' may break the wave evolution model. This function also ensures that
#' transects are pointing in the correction direction and fixes them if
#' they are not.
#'
#' @param point_elev Cross-shore point elevations spatial points object
#' returned from ExtractElev.
#' @param RadLineDist Maximum radial line distance (onshore and offshore) in
#' kilometers of the cross-shore profiles. Note that this value must be the
#' same value used in samplePoints().
#' @param MaxOnshoreDist Maximum radial line distance onshore in kilometers.
#' Note that in some instances the onshore extent should be truncated
#' substaintly for the wave evolution model. It is reccomended to keep this
#' value below 1.
#' @param trimline Optional back shore trim line used to restrict the on-shore
#' extent of the cross-shore profiles. The default value is NA (not used). If
#' used, the back shore trim line should run parallel to the coastline but be
#' setback onto land. A back shore trim line may be required in cases where
#' cross-shore profiles are generated along the coastline of a narrow peninsula
#' or in instances where there is a back shore lagoon. A back shore trimline
#' should be provided as a simple features line object.
#'
#' @details Running this function is required to ensure that
#' there are no artifacts along the cross-shore profile that may cause the
#' subsequent wave and erosion models to fail. This function also adds a
#' cross-shore distance column Xpos to each profile.
#'
#' @return An object of class sf and data.frame cleaned and orientated in the
#' proper direction with a cross-shore distance column Xpos added to the data.
#' @examples
#' \dontrun{
#' library(MNAI.CPBT)
#' data(Coastline)
#' # Generate cross-shore profile lines along the coastline.
#' shoreline_points <- samplePoints(
#'   Coastline = Coastline,
#'   ShorelinePointDist = 100,
#'   BufferDist = 50,
#'   RadLineDist = 1.5
#' )
#'
#' # Extract elevation values along each profile
#' rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
#' TopoBathy <- raster::raster(rpath)
#' pt_elevs <- ExtractElev(shoreline_points[[2]], TopoBathy)
#'
#' # Run SignalSmooth function to smooth elevation profiles
#' pt_elevs <- SignalSmooth(point_elev = pt_elevs,
#' SmoothParameter = 5)
#'
#' # Clean the cross-shore profiles with CleanTransect
#' cleantransect <- CleanTransect(
#'   point_elev = pt_elevs,
#'   RadLineDist = 1.5,
#'   MaxOnshoreDist = 1,
#'   trimline = NA
#' )
#'
#' }
#' @export
CleanTransect <- function(
  point_elev = NA,
  RadLineDist = 3,
  MaxOnshoreDist = 3,
  trimline = NA
) {

  rebuild_pts <- list()

  uid <- unique(point_elev$line_id)

  # loop through point sets
  for(i in 1:length(uid)) {

    this_id <- uid[i]
    this_dat <- point_elev[which(point_elev$line_id == this_id),]

    # plot(this_dat$elev, type='l')

    # determine if flipped
    # look at mean on each side
    mid <- (RadLineDist * 1000)

    # first and second half
    p1 <- this_dat[1:mid, ]
    p2 <- this_dat[(mid + 1):nrow(this_dat), ]

    m1 <- mean(p1$elev, na.rm=TRUE)
    m2 <- mean(p2$elev, na.rm=TRUE)

    # if flipped - reverse
    if(m2 > m1) {
      this_dat <- this_dat[rev(seq_len(nrow(this_dat))),]
    }

    # set pos and neg cross shore dist vals
    this_dat$Xpos <- NA
    this_dat$Xpos[1:mid] <- seq(-RadLineDist * 1000, -1)
    this_dat$Xpos[(mid + 0):(nrow(this_dat))] <- seq(1, RadLineDist * 1000)

    # plot(this_dat$Xpos, this_dat$elev, type='l', xlim = c(-50, 50))
    # abline(h = 0); abline(v = 0)

    # remove portions offshore that are shallower than average depth above
    # deepest point

    deepest <- min(this_dat$elev_smooth, na.rm = TRUE)
    deep_pt <- which(this_dat$elev_smooth == deepest)

    this_dat <- suppressWarnings(this_dat[1:deep_pt, ])

    # exclude areas too far on shore
    mosd <- -MaxOnshoreDist * 1000 # km to m
    this_dat <- this_dat[which(this_dat$Xpos > mosd), ]


    # If an onshore trimline is provided remove any points on shore behind it
    if(!(length(trimline) == 1)) {
      if(class(trimline)[1] == "sf") {
        trim <- trimline
        dist <- as.numeric(sf::st_distance(this_dat, trim))
        closest <- this_dat[which(dist == min(dist)), ]
        X_limit <- closest$Xpos
        # trim any points behind trimline
        this_dat <- this_dat[which(this_dat$Xpos > X_limit),]
      }
    }



    # Look for large break in data and cut transect if so
    big_break <- this_dat$Xpos[which(is.na(this_dat$elev))]

    if(length(big_break) > 0) {
      start <- seq(min(big_break), min(big_break) + 100, by = 1)
      #if(all(big_break[1:100] == start)) {
      #}
    }

    # rebuild list object
    rebuild_pts[[i]] <- this_dat

  }

  pt_exp <- do.call("rbind", rebuild_pts)

  return(pt_exp)


}
