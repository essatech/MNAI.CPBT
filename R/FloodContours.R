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


  #=======================================================
  # Interpolate runup heights
  #=======================================================

  erosion <- erosion_points
  epp <- sf::as_Spatial(erosion)
  #mapview(epp['runup_NoVeg'])


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

  countour_runup_noveg <- raster::rasterToContour(r_runup, maxpixels=5000000, level=0)


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
  countour_runup_veg <- raster::rasterToContour(r_runup, maxpixels=5000000, level=0)

  # mapview(countour_runup_noveg) + mapview(countour_runup_veg)






  #=======================================================
  # Add all contours togehter
  #=======================================================

  cs$name <- c("LowTide", "HighTide", "StaticWL")
  cs$level <- NULL


  csnv <- sf::st_as_sf(countour_runup_noveg)
  csv <- sf::st_as_sf(countour_runup_veg)
  csnv$name <- "RunUpNoVeg"
  csv$name <- "RunUpVeg"
  csnv$level <- NULL
  csv$level <- NULL


  cs1 <- rbind(cs, csv) # csv
  cs2 <- rbind(cs1, csnv) #csnv



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


  #cs_final_clip <- st_crop(cs_final, st_bbox(erosion))
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
