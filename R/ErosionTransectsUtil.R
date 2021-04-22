#' @title Erosion Transects Utility
#'
#' @description Beach erosion model originally developed by Dr. Greg Guannel
#' for the Coastal Natural Capital InVEST project. This function estimates
#' beach retreat and wave runup based on foreshore attributes.
#'
#' @param Ho Initial offshore wave height in meters.
#' @param To Initial offshore wave period in seconds.
#' @param total_wsl_adj Total water surface level above the chart datum. Recall
#' that the chart datum and TopoBathy DEM are referenced to have 0 at low
#' water. It is recommended that the erosion model is run with mean sea level.
#' @param linkbeach Dataframe returned from LinkProfilesToBerms.
#' @param wave_data sf and dataframe spatial points returned from WaveModel.
#' @param storm_duration Storm duration in hours.
#' @param Longshore Longshore distance in meters should match
#' ShorelinePointDist used in samplePoints.
#' @param PropValue Property value in dollars per square meter of beach.
#' (not used if fs_dat is provided).
#' @param Tr Return period of the simulated storm (in years).
#' @param disc Annual valuation discount rate over the time horizon (0 - 1).
#' @param TimeHoriz Time horizon for valuation (in years).
#' @param mean_sea_level Mean sea level elevation in meters above chart datum.
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
#' #' \describe{
#'   \item{retreat_}{Lateral beach retreat distance in meters from the storm.
#'   (single storm event)}
#'   \item{runup_}{Vertical wave runup elevation at a profile.}
#'   \item{damage_}{Total erosion damage at a cross-shore profile section from
#'   beach loss due to retreat and storm return period over the time horizon.}
#'   \item{transect_id}{Cross-shore profile transect ID (to link back to
#'   previous data sets.}
#'   \item{area_loss_}{Beach loss area m2 from the storm (single storm event)
#'   .}
#'   \item{vol_loss_}{Beach loss volume m3 from the storm (single storm event).}
#'   \item{retreat_pct_}{Beach retreat percentage (lateral retreat distance
#'   divided by berm width).}
#'   \item{retreat_index_}{Beach retreat index score 1-5.}
#' }
#'
#' @references
#' InVEST: Wave Attenuation & Erosion Reduction: Coastal Protection (G. Guannel)
#'
#' Guannel et al. (2014) Integrated modeling framework to quantify the coastal
#' protection services supplied by vegetation. Journal of
#' Geophysical Research: Oceans. DOI: 10.1002/2014JC009821.
#'
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
#' data(Trimline)
#' cleantransect <- CleanTransect(
#'   point_elev = pt_elevs,
#'   RadLineDist = 1.5,
#'   MaxOnshoreDist = 0.01,
#'   trimline = Trimline
#' )
#'
#' # Merge vegetation onto lines
#' data(Vegetation)
#' dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)
#'
#' # Run the wave evolution model
#' wave_data <- WaveModel(dat = dat_veg,
#'   total_wsl_adj = 0.5,
#'   Ho = 2,
#'   To = 8
#' )
#'
#' data(BeachAttributes)
#' linkbeach <- LinkProfilesToBeaches(BeachAttributes = BeachAttributes,
#' dat = wave_data)
#'
#' erosion <- ErosionTransectsUtil(
#'     Ho = 2,
#'     To = 8,
#'     total_wsl_adj = 0.5,
#'     linkbeach = linkbeach,
#'     wave_data = wave_data,
#'     storm_duration = 3,
#'     Tr = 10,
#'     Longshore = 150,
#'     PropValue = 200,
#'     disc = 0.05,
#'     TimeHoriz = 50
#'   )
#'
#'  # erosion summaries by cross-shore profile sections
#'  print(wave_data)
#'
#' }
#' @export
ErosionTransectsUtil <- function(
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

  ssf_lookup = getSSF()
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

    # Foreshore paramters
    sediment_size_mm = this_param$sed_size
    ssf_lookup = ssf_lookup
    total_wsl_adj = total_wsl_adj
    m = this_param$fore_slp
    B = this_param$berm_heigh
    D = this_param$dune_heigh
    W = this_param$berm_lengt

    if(m < 0.02){
      m = 0.02
    }

    if(m > 0.2){
      m = 0.2
    }

    # Need to correct for bad sediment sizes
    if(sediment_size_mm > 50){
      run_erosion <- FALSE
      sediment_size_mm <- 0.15
    }
    if(sediment_size_mm < 0.12){
      sediment_size_mm <- 0.12
    }

    # Run Erosion function
    erosion <- ErosionTransect(wave_transect = wave_transect,
                               Ho=Ho,
                               To=To,
                               sediment_size_mm = sediment_size_mm,
                               ssf_lookup = ssf_lookup,
                               total_wsl_adj = total_wsl_adj,
                               m = m,
                               B = B,
                               D = D,
                               W = W,
                               storm_duration = storm_duration
    )



    #------------------------------------------------
    # Estimate volume of material lost from erosion
    # Assume berm and foreshore slope shift backwards

    # Calculate Area 1 (berm only)
    area1 <- B*erosion$retreat_NoVeg
    # Calculate Area 2 (slope between high tide and MSL)
    p_opposite <- mean_high_water - mean_sea_level
    area2 <- p_opposite*erosion$retreat_NoVeg
    # Cross section area loss
    cs_area_loss <- area1 + area2
    # Volume Loss
    vol_loss_NoVeg <- round(cs_area_loss*Longshore,1)

    # Calculate Area 1 (berm only)
    area1 <- B*erosion$retreat_Veg
    # Calculate Area 2 (slope between high tide and MSL)
    p_opposite <- mean_high_water - mean_sea_level
    area2 <- p_opposite*erosion$retreat_Veg
    # Cross section area loss
    cs_area_loss <- area1 + area2
    # Volume Loss
    vol_loss_Veg <- round(cs_area_loss*Longshore,1)


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
    erosion$retreat_pct_NoVeg <- round((erosion$retreat_NoVeg/W)*100,1)
    erosion$retreat_pct_Veg <- round((erosion$retreat_Veg/W)*100,1)


    erosion$retreat_index_NoVeg <-cut(erosion$retreat_pct_NoVeg,
                                      breaks=c(-Inf,1,25,50,75,100,Inf),
                                      labels=c(0,1,2,3,4,5))

    erosion$retreat_index_Veg <-cut(erosion$retreat_pct_Veg,
                                    breaks=c(-Inf,1,25,50,75,100,Inf),
                                    labels=c(0,1,2,3,4,5))




    all_erosion[[i]] <- erosion
    #print(i)

  } # end of loop


  all_out <- do.call("rbind", all_erosion)





  return(all_out)


} # end of function




