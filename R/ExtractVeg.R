#' @title ExtractVeg
#'
#' @description Merges the cleaned cross-shore profile dataset with vegetation
#' data. Note that this function must be run before the wave evolution and
#' erosion models can be run. Even if your project has no vegetation, running
#' this function is still necessary to format the data frame with appropriate
#' columns.
#'
#' @param pt_exp Cross-shore point elevations spatial points object
#' returned from CleanTransect.
#' @param Vegetation Maximum radial line distance (onshore and offshore) in
#' kilometers of the cross-shore profiles. Note that this value must be the
#' same value used in samplePoints().
#'
#' @return An object of class sf and data.frame updated with vegetation data
#' attributes and ready for the wave evolution and erosion model.
#' @examples
#' \dontrun{
#' library(MNAI.CPBT)
#' data(Coastline)
#' # Generate cross-shore profile lines along the coastline.
#' crossshore_profiles <- samplePoints(
#'   Coastline = Coastline,
#'   ShorelinePointDist = 150,
#'   BufferDist = 50,
#'   RadLineDist = 1.5
#' )
#' crossshore_lines <- crossshore_profiles[[2]]
#'
#' # Extract elevation values along each profile
#' rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
#' TopoBathy <- raster::raster(rpath)
#' pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)
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
#' # Merge vegetation onto lines
#' data(Vegetation)
#' dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)
#'
#' # Cross-shore profiles ready for modeling
#' }
#' @export
ExtractVeg <- function(
  pt_exp = NA,
  Vegetation = NA
) {


if(length(Vegetation) != 1) {

  if(class(Vegetation)[1] == "sf") {

    # Fix veg column names
    Vegetation$StemHeight <- Vegetation$hc
    Vegetation$StemDensty <- Vegetation$N
    Vegetation$StemDiam <- Vegetation$d

    # Clip veg layers to project extent
    bbox <- sf::st_as_sfc(sf::st_bbox(pt_exp))

    # Identify veg classes
    veg_classes <- unique(Vegetation$Type)


    for(i in seq_len(length(veg_classes))){

      this_type <- veg_classes[i]

      tveg <- Vegetation[which(Vegetation$Type == this_type), ]

      # Convert all from multipolygon to single polygon
      tveg <- sf::st_cast(tveg, 'POLYGON')


      # Set Default if User did not define
      # veg attributes

      # KELP DEFAULTS...........................
      if(tveg$Type[1] == 'Kelp'){ # KELP
        if(!("StemHeight" %in% colnames(tveg))){
          tveg$StemHeight <- 4
        }
        if(!("StemDiam" %in% colnames(tveg))){
          tveg$StemDiam <- 0.05
        }
        if(!("StemDensty" %in% colnames(tveg))){
          tveg$StemDensty <- 10
        }
        if(!("Cd" %in% colnames(tveg))){
          tveg$Cd <- 0.1
        }
      }


      # EELGRASS DEFAULTS...........................
      if(tveg$Type[1] == 'Eelgrass'){ #
        if(!("StemHeight" %in% colnames(tveg))){
          tveg$StemHeight <- 1.5
        }
        if(!("StemDiam" %in% colnames(tveg))){
          tveg$StemDiam <- 0.015
        }
        if(!("StemDensty" %in% colnames(tveg))){
          tveg$StemDensty <- 200
        }
        if(!("Cd" %in% colnames(tveg))){
          tveg$Cd <- 0.1
        }
      }


      # Marsh DEFAULTS...........................
      if(tveg$Type[1] == 'Marsh'){ #
        if(!("StemHeight" %in% colnames(tveg))){
          tveg$StemHeight <- 0.3
        }
        if(!("StemDiam" %in% colnames(tveg))){
          tveg$StemDiam <- 0.01
        }
        if(!("StemDensty" %in% colnames(tveg))){
          tveg$StemDensty <- 500
        }
        if(!("Cd" %in% colnames(tveg))){
          tveg$Cd <- 0.1
        }
      }


      int <- sf::st_intersects(tveg, bbox)
      int2 <- unlist(lapply(int, "length"))
      keep <- tveg[which(int2==1),]

      # filter for proper columns
      keep <- keep[,c("Type", "StemHeight", "StemDiam", "StemDensty", "Cd")]

      keep <- sf::st_cast(keep, "POLYGON")
      keep <- sf::st_zm(keep)

      if(i > 1) {
        all <- rbind(all, keep)
      } else {
        all <- keep
      }
    }


    # Need to remove overlapping portions
    # reverse order
    all <- all[nrow(all):1, ]

    all2 <- sf::st_intersection(all)
    merg_Veg <- all2[which(sf::st_geometry_type(all2) %in% c('POLYGON', 'MULTIPOLYGON')), ]


    # Extract (join) vegetation and attributes under profiles
    elev_veg <- sf::st_join(pt_exp, merg_Veg)

    elev_veg$n.overlaps <- NULL

    elev_veg$origins <- NULL

    elev_veg$Type <- ifelse(is.na(elev_veg$Type), NA, as.character(elev_veg$Type))

    dat <- elev_veg

  }
} else {

  # Build empty object with no veg
  pt_exp$Type <- NA
  pt_exp$StemHeight <- NA
  pt_exp$StemDiam <- NA
  pt_exp$StemDensty <- NA
  pt_exp$Cd <- NA

  dat <- pt_exp

}

  return(dat)

}

