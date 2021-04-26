#' @title Depth Damage Flood Summaries
#'
#' @description Summarizes flood damage values from a storm event using depth
#' damage curves for each structure with a given flood water depth.
#'
#' @param flood_contours Flood contours object returned from FloodContours.
#' @param Bldgs sf and dataframe polygon object of building structure foot
#' prints. This dataset must have HAZUS depth-damge curves IDs populated for
#' each structure (DDID) as well as estimates of the replacement value cost
#' for each structure in dollars (VAL). See data(Bldgs) for an example input.
#' @param HAZUS HAZUS depth-damage reference curves. Note that these are
#' included in the package see data(HAZUS). Users may modify these curves,
#' but it is expected that most users will use default values.
#'
#' @return Flood damage cost summaries for the storm event with the number of
#' structures flooded, the mean flood depth and the total damage cost. Values
#' are provided for scenarios with submerged vegetation and without submerged
#' vegetation.
#' #' \describe{
#'   \item{nStructure}{Number of structures flooded.}
#'   \item{MedianDepth}{Median depth of flooded structure.}
#'   \item{MaxDepth}{Maximum flood depth across all structures.}
#'   \item{Damage}{Total flood structural damage cost in dollars.}
#' }
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
#' # Calculate the total flood damage cost using depth damage curves
#'
#'  data(Bldgs)
#'  data(HAZUS)
#'  dd_flood <- DepthDamageFlood(Bldgs = Bldgs,
#'    flood_contours = flood_contours,
#'    HAZUS = HAZUS)
#'
#'  # Total damage cost of flooding (dollars)
#'  dd_flood$VegDamage
#'
#'
#' }
#' @export
DepthDamageFlood <- function(
  Bldgs = NA,
  flood_contours = NA,
  HAZUS = NA
) {


  bld <- Bldgs
  r_d_veg    <- flood_contours[["r_d_veg"]]
  r_d_noveg  <- flood_contours[["r_d_noveg"]]

  bld$Sec_Val <- bld$VAL

  # Load buildings
  bld <- sf::st_zm(bld, drop=TRUE)

  bld$Sec_Val <- ifelse(is.na(bld$Sec_Val), 0, bld$Sec_Val)


  if(sum(is.na(bld$Sec_Val))>0){
    print("ERROR - missing Building value info")
    damage_exp <- data.frame(NoVeg_nStructure = 0,
                             Veg_nStructure   = 0,
                             NoVeg_MedianDepth=0,
                             Veg_MedianDepth  = 0,
                             NoVeg_MaxDepth   = 0,
                             Veg_MaxDepth     = 0,
                             NoVegDamage      = 0,
                             VegDamage        = 0
    )
    return(damage_exp)
  }

  # Get the median depth of each building for each flood scenario

  # Resample to 5m res
  r_d_noveg_rs <- raster::aggregate(r_d_noveg, fact=5, fun=mean, expand=FALSE)
  r_d_veg_rs <- raster::aggregate(r_d_veg, fact=5, fun=mean, expand=FALSE)


  # noVeg
  ex_NoVeg <- raster::extract(r_d_noveg_rs, bld, fun=mean, na.rm=TRUE, df=TRUE)

  # noVeg
  ex_Veg <- raster::extract(r_d_veg_rs, bld, fun=mean, na.rm=TRUE, df=TRUE)


  bld$depth_NoVeg <- ex_NoVeg[,2]
  bld$depth_Veg <- ex_Veg[,2]


  # uid for depth damage field
  # bld$uid <- paste0(bld$Occupancy, "_", bld$DmgFnId)
  bld$DmgFnId <- bld$DDID
  bld$uid <- bld$DDID

  # Drop any with NA
  bld_nv <- bld[which(!(is.na(bld$depth_NoVeg))),]
  bld_v <- bld[which(!(is.na(bld$depth_Veg))),]

  # Drop structures above water
  bld_nv <- bld_nv[which(bld_nv$depth_NoVeg < 0),]
  bld_v <- bld_v[which(bld_v$depth_Veg < 0),]

  # Fix sign to make positive
  bld_nv$depth_NoVeg <- bld_nv$depth_NoVeg*-1
  bld_v$depth_Veg <- bld_v$depth_Veg*-1


  #plot(bld_v['depth_Veg'], border=NA)
  #plot(bld_nv['depth_NoVeg'], border=NA)


  # Load depth damage curves
  # ddc <- read.csv("./www/dat/HAZUS_depth_damage_functions_long.csv")
  ddc <- HAZUS
  # ddc$uid <- paste0(ddc$Occupancy, "_", ddc$DmgFnId)
  ddc$uid <- ddc$DDID


  # depth damage codes to loop through
  codes <- unique(bld$uid)



  # Return 0 if no buildings were covered
  if(nrow(bld_nv)==0){
    damage_exp <- data.frame(NoVeg_nStructure = 0,
                             Veg_nStructure   = 0,
                             NoVeg_MedianDepth=0,
                             Veg_MedianDepth  = 0,
                             NoVeg_MaxDepth   = 0,
                             Veg_MaxDepth     = 0,
                             NoVegDamage      = 0,
                             VegDamage        = 0
    )
    return(damage_exp)
  }


  #============================================

  veg_all <- list()
  noveg_all <- list()


  for(c in 1:length(codes)){

    tcode <- codes[c]

    # subset
    cnv <- bld_nv[which(bld_nv$uid == tcode),]
    cv <- bld_v[which(bld_v$uid == tcode),]
    c_ddc <- ddc[which(ddc$uid == tcode),]


    # Veg-Damage
    dmg_est_v <- stats::approx(x=c_ddc$depth_m,
                        y=c_ddc$damage,
                        xout=cv$depth_Veg)
    cv$damage <- dmg_est_v$y

    # Non-Veg Damage
    dmg_est_nv <- stats::approx(x=c_ddc$depth_m,
                         y=c_ddc$damage,
                         xout=cnv$depth_NoVeg)
    cnv$damage <- dmg_est_nv$y

    # Fix extreme values
    cnv$damage <- ifelse(cnv$depth_NoVeg < min(c_ddc$depth_m), min(min(c_ddc$damage)), cnv$damage)
    cnv$damage <- ifelse(cnv$depth_NoVeg > max(c_ddc$depth_m), max(min(c_ddc$damage)), cnv$damage)

    cv$damage <- ifelse(cv$depth_Veg < min(c_ddc$depth_m), min(min(c_ddc$damage)), cv$damage)
    cv$damage <- ifelse(cv$depth_Veg > max(c_ddc$depth_m), max(min(c_ddc$damage)), cv$damage)



    veg_all[[c]]    <- cv
    noveg_all[[c]]  <- cnv


  }


  #==================================


  v_all <- do.call("rbind", veg_all)
  noveg_all <- do.call("rbind", noveg_all)



  #==================================
  # Calculate Value
  v_all$damage_cost   <- v_all$Sec_Val*v_all$damage
  noveg_all$damage_cost   <- noveg_all$Sec_Val*noveg_all$damage


  NoVegDamage <- sum(noveg_all$damage_cost, na.rm=TRUE)
  VegDamage <- sum(v_all$damage_cost, na.rm=TRUE)

  NoVeg_MaxDepth <- max(noveg_all$depth_NoVeg, na.rm=TRUE)
  Veg_MaxDepth <- max(v_all$depth_Veg, na.rm=TRUE)

  NoVeg_MedianDepth <- stats::median(noveg_all$depth_NoVeg, na.rm=TRUE)
  Veg_MedianDepth <- stats::median(v_all$depth_Veg, na.rm=TRUE)

  NoVeg_nStructure <- nrow(noveg_all)
  Veg_nStructure <- nrow(v_all)


  damage_exp <- data.frame(NoVeg_nStructure = NoVeg_nStructure,
                           Veg_nStructure   = Veg_nStructure,
                           NoVeg_MedianDepth=NoVeg_MedianDepth,
                           Veg_MedianDepth  = Veg_MedianDepth,
                           NoVeg_MaxDepth   = NoVeg_MaxDepth,
                           Veg_MaxDepth     = Veg_MaxDepth,
                           NoVegDamage      = NoVegDamage,
                           VegDamage        = VegDamage
  )

  return(damage_exp)


}
