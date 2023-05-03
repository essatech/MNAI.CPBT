#' @title Erosion Transects Utility Mangrove
#'
#' @description Mudflat erosion model originally developed by Dr. Greg Guannel
#' for the Coastal Natural Capital InVEST project. This function estimates
#' beach retreat and wave runup for mangrove mudflats.
#'
#' @param Ho Initial offshore wave height in meters.
#' @param To Initial offshore wave period in seconds.
#' @param total_wsl_adj Total water surface level above the chart datum. Recall
#' that the chart datum and TopoBathy DEM are referenced to have 0 at low
#' water. It is recommended that the erosion model is run with mean sea level.
#' @param linkbeach Dataframe returned from LinkProfilesToBerms. Should contain.
#' columns for forshore slope `fore_slp`, Erosion Constant `me` in kg/Ns and
#' density `Cm` in km/m3.
#' @param wave_data sf and dataframe spatial points returned from WaveModel.
#' @param storm_duration Numeric. Storm duration in hours.
#' @param Longshore Longshore distance in meters should match
#' ShorelinePointDist used in samplePoints.
#' @param PropValue Generally land value in dollars per square meter of beach.
#' (not used if beach polygon property values are provided).
#' @param Tr Numeric. Return period (frequency) of the simulated storm (in years).
#' @param disc Annual valuation discount rate over the time horizon (0 - 1).
#' @param TimeHoriz Time horizon (in years) for long term cumulative
#' valuation given a storm return frequency.
#' Typically 100-year horizons are used.
#' @param mean_sea_level Mean sea level elevation in meters above chart datum.
#' Note that the chart datum should be adjusted to LLW for these estimates
#' @param mean_high_water Mean high water level elevation in meters above
#' chart datum.
#'
#' @details The coastal erosion model was originally developed by Dr. Greg
#' Guannel for the Coastal Natural Capital InVEST project. This function
#' estimates lateral beach erosion and wave runup for each cross-shore profile.
#' Foreshore parameters for berm width, height etc. are provided as spatial
#' polygons for each beach section. These are then linked to underlying
#' cross-shore profiles for erosion estimates. The return object is a data frame
#' showing erosion estimates for each cross-shore profile.
#'
#' @return A data frame with erosion estimates for each cross-shore profile.
#' _NoVeg is estimated without submerged vegetation and _Veg is an estimate with
#' submerged vegetation
#'
#' @references
#' InVEST: Wave Attenuation & Erosion Reduction: Coastal Protection (G. Guannel)
#'
#' Guannel et al. (2014) Integrated modeling framework to quantify the coastal
#' protection services supplied by vegetation. Journal of
#' Geophysical Research: Oceans. DOI: 10.1002/2014JC009821.
#'
#' @export
ErosionTransectsUtilMangrove <- function(
    Ho = 2.5,
    To = 7,
    total_wsl_adj = NA,
    linkbeach = NA,
    wave_data = NA,
    storm_duration = 3,
    Longshore = 100,
    PropValue = 200,
    Tr = 10,
    disc = 0.05,
    TimeHoriz = 50,
    mean_sea_level = 0.01,
    mean_high_water = 0.7
) {


  fs_dat <- linkbeach
  wave_dat <- wave_data

  math.pi = pi

  # Calculate erosion for each transect
  uids <- unique(fs_dat$line_id)

  all_erosion <- list()

  for(i in 1:length(uids)){

    # Determine if should run or set to zero

    run_erosion <- TRUE

    this_id <- uids[i]

    # Get current transect
    this_param <- fs_dat[which(fs_dat$line_id == this_id),]
    wave_transect <- wave_dat[which(wave_dat$line_id == this_id),]

    if(nrow(wave_transect)<10){
      next
    }

    me <- this_param$me
    Cm <- this_param$Cm
    m <- this_param$fore_slp


    if(is.null(me)) {
      print("setting me to 0.001")
      me <- 0.001
    }
    if(is.na(me)) {
      me <- 0.001
    }

    if(is.null(Cm)) {
      print("setting Cm to 70")
      Cm <- 70
    }
    if(is.na(Cm)) {
      Cm <- 70
    }


    if(is.null(m)) {
      print("setting fore_slp to 0.001")
      m <- 0.001
    }
    if(is.na(m)) {
      m <- 0.001
    }


    # Run Erosion function
    erosion <- ErosionTransectMangrove(
      wave_transect = wave_transect,
      Ho = Ho,
      To = To,
      total_wsl_adj = total_wsl_adj,
      m = m,
      me = me,
      Cm = Cm,
      storm_duration = storm_duration
    )



    #------------------------------------------------
    # Estimate volume of material lost from erosion
    # Assume berm and foreshore slope shift backwards

    # Volume Loss
    vol_loss_NoVeg <- round(erosion$erosion_m3_NoVeg, 1)

    # Volume Loss
    vol_loss_Veg <- round(erosion$erosion_m3_Veg, 1)


    # Beach Area Loss for Veg and NoVeg
    area_loss_NoVeg <- round(erosion$retreat_NoVeg*Longshore,1)
    area_loss_Veg <- round(erosion$retreat_Veg*Longshore,1)


    if(!(run_erosion)){
      # erosion model failed - set erosion to zero
      print("Sed size too great")
      erosion$retreat_NoVeg <- 0
      erosion$retreat_Veg <- 0
    }


    # Run VALUATION Function - for single transect
    # Inputs: Longshore, PropValue, Tr, disc, TimeHoriz

    # Calculate erosion area
    E1 = erosion$retreat_NoVeg * Longshore
    E2 = erosion$retreat_Veg * Longshore

    # Calculate erosion value per square meter - from the current storm
    Dav_NoVeg = E1 * PropValue
    Dav_Veg = E2 * PropValue

    # Calculate cost over time horizon with return period
    p = 1.0 / Tr  # return frequency
    temp1 = 1.0 + disc # Discount rate
    temp2 = (1/temp1)**(seq(1:TimeHoriz))
    # Total erosion area
    EPV_NoVeg = p * Dav_NoVeg * sum(temp2)
    EPV_Veg = p * Dav_Veg * sum(temp2)


    erosion$damage_NoVeg <- EPV_NoVeg
    erosion$damage_Veg <- EPV_Veg


    erosion$transect_id = this_id


    # Add on area loss
    erosion$area_loss_Veg <- area_loss_Veg
    erosion$area_loss_NoVeg <- area_loss_NoVeg


    # Add on volume loss
    erosion$vol_loss_Veg <- vol_loss_Veg
    erosion$vol_loss_NoVeg <- vol_loss_NoVeg


    # Calculate erosion score
    # Calculate erosion score as percent of beach berm length
    erosion$retreat_pct_NoVeg <- NA
    erosion$retreat_pct_Veg <- NA


    erosion$retreat_index_NoVeg <- NA

    erosion$retreat_index_Veg <- NA

    all_erosion[[i]] <- erosion
    #print(i)

  } # end of loop


  all_out <- do.call("rbind", all_erosion)





  return(all_out)


} # end of function




