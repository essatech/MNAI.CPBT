#' @title Export Profiles
#'
#' @description Exports cross shore profiles as csv files to a specified local
#' directory.
#'
#' @param dat A sf and data frame of cross-profiles returned from ExtractVeg.
#' @param wave_dat A sf and data frame of cross-profiles returned from
#' WaveModel.
#' @param erosion Erosion summaries returned from ErosionTransectsUtil.
#' @param mean_high_water Mean high water elevation above the chart datum.
#' @param total_wsl_adj Total water surface level above the chart datum. Recall
#' that the chart datum and TopoBathy DEM are referenced to have 0 at low
#' water. It is therefore suggested to set this value at the mean sea level
#' above chart datum or a specific tidal elevation of interest.
#' @param TrimOnshoreDist Numeric, option distance onshore to trim profile in
#' meters.
#' @param path_output Output directory to save cross-shore profiles.
#' @param export_csv Should cross shore profiles be exported.
#'
#' @return Cross-shore profiles are exported and save as csv to a directory.
#'
#' @export
ExportProfiles <- function(
  dat = NA,
  wave_dat = NA,
  erosion = NA,
  mean_high_water = NA,
  total_wsl_adj = NA,
  TrimOnshoreDist = NA,
  path_output = NA,
  export_csv = TRUE
) {

  uids <- unique(dat$line_id)
  eids <- unique(erosion$transect_id)

  exp_data <- list()
  exp_flood <- list()
  exp_flood_marks <- list()


  for(i in 1:length(uids)) {

    this_id <- uids[i]
    # this_id = 17

    # Current profile
    dats <- dat[which(dat$line_id == this_id),]

    # If a ridge filter out past max elev
    max_elev <- max(dats$elev, na.rm=TRUE)

    # Find ridge crest
    dd1 <- dplyr::filter(dats, dats$elev > 0)
    dd2 <- dplyr::filter(dd1, abs(dd1$elev - max_elev) == min(abs(dd1$elev - max_elev)))
    ds1 <- dd2[, 'Xpos']

    # ds1 <- dats %>% filter(elev > 0) %>%
    #   filter(abs(elev - max_elev) == min(abs(elev - max_elev))) %>%
    #   dplyr::select(Xpos)

    x_peak <- ds1$Xpos[1]


    if(!(is.na(TrimOnshoreDist))){
      # Filter past peak
      dats <- dats[which(dats$Xpos >= x_peak), ] #adj

      # Filter anything beyond 200m Xpos horizontal distance
      dats <- dats[which(dats$Xpos >= (-1 * TrimOnshoreDist)), ] #adj
    }

    # Filter out any land areas that are over a elevation of 30m
    elev_30 <- dats$Xpos[which(dats$elev > 30)]

    if(length(elev_30 > 0)){
      # Get closest shoreward distance
      ref <- max(elev_30)
      dats <- dats[which(dats$Xpos >= ref),] #adj
    }


    # Interpolate over any NA values
    interp_elev <- stats::approx(x=dats$Xpos, y=dats$elev, xout=dats$Xpos)

    # replace with interpolated values
    dats$elev <- interp_elev$y



    #=========================================
    # Current runup
    erosion_t <- erosion[which(erosion$transect_id == this_id),]

    if(nrow(erosion_t) == 0) {
      ru_veg <- 0
      ru_noveg <- 0
      print("no erosion data")
      next
    } else {
      ru_veg <- erosion_t$runup_Veg
      ru_noveg <- erosion_t$runup_NoVeg
    }


    # total_wsl_adj
    total_wsl_adj_t <- total_wsl_adj + ru_noveg
    total_wsl_adj_t_veg <- total_wsl_adj + ru_veg


    myylim <- ceiling(total_wsl_adj_t) + 2

    # Get xlim for plot boundaries
    dd <- dplyr::filter(dats, dats$elev < 0)
    dd <- dplyr::mutate(dd, b = abs(dd$elev))
    dd <- dplyr::filter(dd, abs(dd$b - 2) == min(abs(dd$b - 2)))
    ds1 <- dd[, 'Xpos']


    x1 <- ds1$Xpos[1]

    if(x1 >= 200){
      x1 <- 200
    }



    # Get xlim for plot boundaries
    dd <- dplyr::filter(dats, dats$elev > 0)
    dd <- dplyr::filter(dd, abs(dd$elev - myylim) == min(abs(dd$elev - myylim)))
    ds1 <- dd[, 'Xpos']

    x2 <- ds1$Xpos[1]






    #====================================================
    # Check flooding without vegetation
    #====================================================

    # Get flooded land
    # fl <- dats %>% filter(elev >= mean_high_water & elev <= total_wsl_adj_t)
    fl <- dplyr::filter(dats, dats$elev >= mean_high_water & dats$elev <= total_wsl_adj_t)

    if(nrow(fl)>0){

      # Identify breakpoints
      xx <- fl$Xpos

      bps <- unname(tapply(xx, cumsum(c(1, diff(xx)) != 1), range))

      bp <- data.frame()
      for(b in 1:length(bps)){
        row <- bps[b][[1]]
        dist = abs(row[1] - row[2])
        addr <- data.frame(sec=b,from=row[1], to=row[2], dist=dist)
        bp <- rbind(addr, bp)
      }

      flood_dist <- sum(bp$dist, na.rm=TRUE)
      msub_title_noveg <- paste0(flood_dist, "m of shoreline flooded above high water (without Veg).")

    } else {
      flood_dist <- 0
      msub_title_noveg <- paste0(flood_dist, "m of shoreline flooded above high water (without Veg).")
      bp <- data.frame()

    }







    #====================================================
    # Check flooding with vegetation
    #====================================================

    # Get flooded land
    # fl <- dats %>% filter(elev >= mean_high_water & elev <= total_wsl_adj_t_veg)
    fl <- dplyr::filter(dats, dats$elev >= mean_high_water &
                          dats$elev <= total_wsl_adj_t_veg)

    #plot(fl$Xpos, fl$elev)

    if(nrow(fl)>0){

      # Identify breakpoints
      xx <- fl$Xpos

      bps <- unname(tapply(xx, cumsum(c(1, diff(xx)) != 1), range))

      bpv <- data.frame()
      for(b in 1:length(bps)){
        row <- bps[b][[1]]
        dist = abs(row[1] - row[2])
        addr <- data.frame(sec=b,from=row[1], to=row[2], dist=dist)
        bpv <- rbind(addr, bpv)
      }

      flood_dist_veg <- sum(bpv$dist, na.rm=TRUE)
      msub_title_veg <- paste0(flood_dist_veg, "m of shoreline flooded above high water (with Veg).")
    } else {
      flood_dist_veg <- 0
      msub_title_veg <- paste0(flood_dist_veg, "m of shoreline flooded above high water (with Veg).")
      bpv <- data.frame()

    }



    msub_title <- paste0(msub_title_noveg, "\r\n", msub_title_veg)



    #=============================================
    #=============================================


    exp_dat <- dats[,c("line_id", "Xpos", "elev", "elev_smooth",
                       "Type", "StemHeight", "StemDiam", "StemDensty", "Cd")]

    longs <- as.data.frame(sf::st_coordinates(sf::st_transform(dats, 4326)))[,1]
    lats <- as.data.frame(sf::st_coordinates(sf::st_transform(dats, 4326)))[,2]

    exp_dat$latitude <- lats
    exp_dat$longitude <- longs
    sf::st_geometry(exp_dat) <- NULL

    # add flood data to other object

    flood_dat <- data.frame(line_id= this_id,
                            flood_dist=flood_dist,
                            flood_dist_veg=flood_dist_veg)

    flood_marks <- bp

    exp_data[[i]] <- exp_dat
    exp_flood[[i]] <- flood_dat
    exp_flood_marks[[i]] <- flood_marks

    #print(this_id)

  }




  # print("Merging Object")

  exp_data_out <- do.call("rbind", exp_data)
  exp_flood_out <- do.call("rbind", exp_flood)
  exp_flood_marks_out <- do.call("rbind", exp_flood_marks)

  ret_obj                <- list()
  ret_obj$transect_data  <- exp_data_out
  ret_obj$flood_data     <- exp_flood_out
  ret_obj$flood_marks    <- exp_flood_marks_out






































  tdat_all <- ret_obj$transect_data
  uids <- unique(wave_dat$line_id)



  for(i in 1:length(uids)) {

    this_id <- uids[i]

    twave <- wave_dat[which(wave_dat$line_id == this_id), ]
    tdat <- tdat_all[which(tdat_all$line_id == this_id), ]

    if(nrow(twave) == 0) {
      next
    }

    sf::st_geometry(twave) <- NULL
    twave <- twave[, c("Xpos", "Eta","Etas","Ubot","H_veg","H_noveg")]
    twave[is.na(twave)] <- 0


    mdat <- merge(tdat, twave, by.x="Xpos", by.y="Xpos", all.x=TRUE, all.y=FALSE)
    mdat$Eta[is.na(mdat$Eta)] <- 0
    mdat$Etas[is.na(mdat$Etas)] <- 0
    mdat$Ubot[is.na(mdat$Ubot)] <- 0
    mdat$H_veg[is.na(mdat$H_veg)] <- 0
    mdat$H_noveg[is.na(mdat$H_noveg)] <- 0

    if(export_csv){
      utils::write.csv(mdat,
                file=paste0(path_output,
                            '',
                            'profile_', this_id, '.csv'),
                row.names = FALSE)

    }

  } # end of loop


}
