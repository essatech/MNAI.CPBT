#' @title Flood Contour Generation
#'
#' @description Generates flood contours of the storm event.
#'
#' @param TopoBathy TopoBathy digital elevation model of class RasterLayer.
#' @param mean_high_water Mean high water tidal level above the chart datum.
#' @param total_wsl_adj Total water surface level above the chart datum. Recall
#' that the chart datum and TopoBathy DEM are referenced to have 0 at low
#' water. It is therefore suggested to set this value at the mean sea level
#' above chart datum or a specific tidal elevation of interest.
#' @param erosion_totals Erosion summaries across the area of interest.

#' @return A list object of flood contours and water surface elevation rasters.
#'
#' @examples
#' \dontrun{
#' library(MNAI.CPBT)
#' data(Coastline)
#' # Generate cross-shore profile lines along the coastline.
#' ShorelinePointDist = 150
#' crossshore_profiles <- samplePoints(
#'   Coastline = Coastline,
#'   ShorelinePointDist = ShorelinePointDist,
#'   BufferDist = 50, RadLineDist = 1.5)
#' crossshore_lines <- crossshore_profiles[[2]]
#'
#'
#' # Extract elevation values along each profile
#' rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
#' TopoBathy <- raster::raster(rpath)
#' pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)
#'
#'
#' # Run SignalSmooth function to smooth elevation profiles
#' pt_elevs <- SignalSmooth(point_elev = pt_elevs,
#' SmoothParameter = 5)
#'
#'
#' # Clean the cross-shore profiles with CleanTransect
#' cleantransect <- CleanTransect(
#'   point_elev = pt_elevs,
#'   RadLineDist = 1.5, MaxOnshoreDist = 0.01, trimline = NA
#' )
#'
#' # Merge vegetation onto lines
#' data(Vegetation)
#' dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)
#'
#' # Run the wave evolution model
#' wave_data <- WaveModel(dat = dat_veg,
#'   total_wsl_adj = 1.2,
#'   Ho = 1.5, To = 5)
#'
#' # Link data to foreshore beach attributes
#' linkbeach <- LinkProfilesToBeaches(BeachAttributes = BeachAttributes,
#' dat = wave_data)
#'
#' # Run the erosion model
#' erosion <- ErosionTransectsUtil(
#'     Ho = 2, To = 8, total_wsl_adj = 1.2,
#'     linkbeach = linkbeach, wave_data = wave_data,
#'     storm_duration = 3, Tr = 10,
#'     Longshore = ShorelinePointDist, PropValue = 200,
#'     disc = 0.05, TimeHoriz = 50)
#'
#' # Get the erosion damage totals across the study area
#' erosion_totals <- ErosionTotals(wave_data = wave_data,
#'   erosion = erosion, Longshore = ShorelinePointDist)
#'
#' # Build flood contours (and rasters of water surface elevation)
#' # over the study area.
#'  flood_contours <- FloodContours(TopoBathy = TopoBathy, mean_high_water = 1,
#'   total_wsl_adj = 1.2, erosion_totals = erosion_totals)
#'
#'  # Inspect object
#'  names(flood_contours)
#'  fc <- flood_contours$contours
#'  head(fc)
#'
#'  # View the flood contours - use mapview if installed
#'  # library(mapview)
#'  # mapview(fc)
#'  plot(sf::st_geometry(fc))
#'  plot(fc['name'])
#'
#'
#' }
#' @export
FloodContours <- function(
  TopoBathy = NA,
  mean_high_water = NA,
  total_wsl_adj = NA,
  erosion_totals = NA
) {


  # erosion_totals = ero_tot2
  erosion_points = erosion_totals$erosion_points

  # TopoBathy
  r <- TopoBathy


  # Level vector
  lv <- c(0, mean_high_water, total_wsl_adj)
  names(lv) <- c("LowTide", "HighTide", "StaticWaterLevel")

  # Make contours for static water level
  countour_static <- raster::rasterToContour(r, maxpixels=5000000, level=lv)
  cs <- sf::st_as_sf(countour_static)

  cs$name[which(cs$level == "0")] <- "LowTide"
  cs$name[which(as.numeric(cs$level) == mean_high_water)] <- "HighTide"
  cs$name[which(as.numeric(cs$level) == total_wsl_adj)] <- "StaticWL"


  cs$level <- NULL

  #=======================================================
  # Interpolate runup heights
  #=======================================================

  erosion <- erosion_points
  epp <- sf::as_Spatial(erosion)
  # mapview(epp['runup_NoVeg'])


  suppressWarnings(sp::proj4string(epp) <- sp::proj4string(r))
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(sp::spsample(epp, "regular", n=50000))
  names(grd)       <- c("X", "Y")
  sp::coordinates(grd) <- c("X", "Y")
  sp::gridded(grd)     <- TRUE  # Create SpatialPixel object
  sp::fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  suppressWarnings(sp::proj4string(grd) <- sp::proj4string(epp))


  #=======================================================
  # Build empty raster layer for interpolation
  r_blank <- raster::raster(ext = raster::extent(r), resolution=10)
  r_blank[] <- NA
  suppressWarnings(sp::proj4string(r_blank) <- sp::proj4string(epp))
  grd_r <- methods::as(r_blank, 'SpatialGrid')
  # sf::as_Spatial(erosion)

  #======================================================
  # Run IDW
  midw <- gstat::idw(runup_NoVeg ~ 1, epp, newdata=grd_r, idp=5)
  # midw <- gstat::idw(runup_NoVeg ~ 1, epp, newdata=grd, idp=5)

  rout <- raster::raster(midw)
  #mapview(rout)

  #plot(rout)
  res2 <- raster::resample(rout, r)

  #------------------------------------------------
  # Note when adding we are bringing raster down
  # confusing to think about making deeper and subtraction
  # Add runup to original
  r_runup <- r - res2
  # Add static water level to original
  r_runup <- r_runup - total_wsl_adj
  # save raster for later
  depth_noveg_raster <- r_runup

  mmin <- raster::minValue(r_runup)
  mmax <- raster::maxValue(r_runup)

  if(mmax > 0 & mmin < 0){
    countour_runup_noveg <- raster::rasterToContour(r_runup, maxpixels=5000000, level=0)
    csnv <- sf::st_as_sf(countour_runup_noveg)
    csnv$name <- "RunUpNoVeg"
    csnv$level <- NULL
    cs <- rbind(cs, csnv) #csnv

  } else {
    #diff <- (mmax - mmin)/2 + mmin
    print("No contour non-veg")
  }



  #======================================================
  #======================================================
  # Run IDW - VEGETATION
  midw <- gstat::idw(runup_Veg ~ 1, epp, newdata=grd_r, idp=5)
  rout <- raster::raster(midw)
  #plot(rout)
  res2 <- raster::resample(rout, r)

  # Add runup to original
  r_runup <- r - res2
  r_runup <- r_runup - total_wsl_adj
  # save raster for later
  depth_veg_raster <- r_runup
  # Calc contour
  # countour_runup_veg <- raster::rasterToContour(r_runup, maxpixels=5000000, level=0)

  mmin <- raster::minValue(r_runup)
  mmax <- raster::maxValue(r_runup)

  if(mmax > 0 & mmin < 0){
    countour_runup_veg <- raster::rasterToContour(r_runup, maxpixels=5000000, level=0)

    csv <- sf::st_as_sf(countour_runup_veg)
    csv$name <- "RunUpVeg"
    csv$level <- NULL
    cs <- rbind(cs, csv)

  } else {
    # diff <- (mmax - mmin)/2 + mmin
    print("No Veg Contour")
  }


  # mapview(countour_runup_noveg) + mapview(countour_runup_veg)

  #=======================================================
  # Add all contours togehter
  #=======================================================


  cs2 <- cs



  #=======================================================
  # Clean up final layer
  #=======================================================

  nn <- unique(cs2$name)
  merge_out <- list()

  for(i in 1:length(nn)){

    this_name <- nn[i]
    cthis <- cs2[which(cs2$name == this_name),]

    tl <- suppressWarnings(sf::st_cast(cthis, "LINESTRING"))
    tl$length <- as.numeric(sf::st_length(tl))
    tl <- tl[which(tl$length > 100),]
    tl$length <- NULL
    merge_out[[i]] <- tl

  }
  cs_final  <- do.call("rbind", merge_out)


  # cs_final_clip <- st_crop(cs_final, st_bbox(erosion))
  # mapview(cs_final)



  #====================================
  # Return flood rasters
  #====================================

  # Build return object
  ret_obj <- list()
  ret_obj[["contours"]]   <- cs_final
  ret_obj[["r_d_veg"]]    <- depth_veg_raster
  ret_obj[["r_d_noveg"]]  <- depth_noveg_raster



  return(ret_obj)


}
