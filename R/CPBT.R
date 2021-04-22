#' @title Coastal Protection Benefit Tool
#'
#' @description Runs the complete CPBT analysis along a coastline for a given
#' storm simulation.
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
#'   \item{nStructure}{Nummber of structures flooded.}
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
#'   ShorelinePointDist = 150
#'   crossshore_profiles <- samplePoints(
#'     Coastline = Coastline,
#'     ShorelinePointDist = ShorelinePointDist,
#'     BufferDist = 50, RadLineDist = 1.5)
#'
#' crossshore_lines <- crossshore_profiles[[2]]
#'
#'
#' # Extract elevation values along each profile
#'   rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
#'   TopoBathy <- raster::raster(rpath)
#'   pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)
#'
#'
#' # Run SignalSmooth function to smooth elevation profiles
#'   pt_elevs <- SignalSmooth(point_elev = pt_elevs,
#'   SmoothParameter = 5)
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
CPBT <- function(
  Bldgs = NA,
  flood_contours = NA,
  HAZUS = NA
) {

  # Output directory
  dir_output <- 'C:/Users/mbayly/Desktop/Projects/EN2591/CPBT/sims/'
  simulation_name <- 'Test Sim 1'


  # samplePoints ....
  data(Coastline)
  Coastline = Coastline
  ShorelinePointDist = 150
  BufferDist = 200
  RadLineDist = 1.5
  data(TopoBathy)
  rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
  TopoBathy <- raster::raster(rpath)


  # SignalSmooth
  SmoothParameter = 5

  #CleanTransect(
  MaxOnshoreDist = 0.01
  data(Trimline)
  trimline = Trimline

  # ExtractVeg
  data(Vegetation)
  Vegetation <- Vegetation

  # WaveModeld
  total_wsl_adj <- 1.2
  tide_during_storm <- 0.5
  surge_elevation <- 0.2
  sea_level_rise <- 0.1
  Ho = 1.5
  To = 5

  # linbeach
  data(BeachAttributes)
  BeachAttributes <- BeachAttributes

  # ErosionTransectsUtil
  storm_duration = 3
  Tr = 10
  Longshore = ShorelinePointDist
  PropValue = 200
  disc = 0.05
  TimeHoriz = 50

  mean_high_water = 1.5
  mean_sea_level = 0.5


  data(Bldgs)
  Bldgs <- Bldgs

  data(HAZUS)
  HAZUS <- HAZUS






  #=====================================================
  # Prep OUTPUT FOLDER
  #=====================================================

  # Create Output Directory
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  rR  <- substrRight(dir_output, 1)
  iR <- ifelse(rR == "/", "", "/")

  path_output <- paste0(dir_output, iR, simulation_name, "/")

  dir.create(path_output)


  file.copy("C:/Users/mbayly/Desktop/Projects/EN2591/CPBT/template/www", path_output, recursive=TRUE)
  file.copy("C:/Users/mbayly/Desktop/Projects/EN2591/CPBT/template/SimulationResults.html", path_output, recursive=TRUE)



  #=====================================================
  # RUN SUB MODELS
  #=====================================================

  # Generate cross-shore profile lines along the coastline.
  crossshore_profiles <- samplePoints(
     Coastline = Coastline,
     ShorelinePointDist = ShorelinePointDist,
     BufferDist = 50, RadLineDist = 1.5
  )

  # Get the lines
  crossshore_lines <- crossshore_profiles[[2]]

  # Extract elevation values along each profile
  pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)


  # Run SignalSmooth function to smooth elevation profiles
  pt_elevs <- SignalSmooth(point_elev = pt_elevs,
                           SmoothParameter = 5)


  # Clean the cross-shore profiles with CleanTransect
  cleantransect <- CleanTransect(
    point_elev = pt_elevs,
    RadLineDist = RadLineDist,
    MaxOnshoreDist = MaxOnshoreDist,
    trimline = trimline
    )

  # Merge vegetation onto lines
  dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)


  # Run the wave evolution model
  wave_data <- WaveModel(
    dat = dat_veg,
    total_wsl_adj = total_wsl_adj,
    Ho = Ho, To = To
    )

  # Link data to foreshore beach attributes
  linkbeach <- LinkProfilesToBeaches(BeachAttributes = BeachAttributes,
                                     dat = wave_data)

  # Run the erosion model
  erosion <- ErosionTransectsUtil(
     Ho = Ho, To = To, total_wsl_adj = total_wsl_adj,
     linkbeach = linkbeach, wave_data = wave_data,
     storm_duration = storm_duration,
     Tr = Tr,
     Longshore = ShorelinePointDist,
     PropValue = PropValue,
     disc = disc,
     TimeHoriz = TimeHoriz
     )


  erosion$damage_t_Veg <- erosion$area_loss_Veg*PropValue
  erosion$damage_t_NoVeg <- erosion$area_loss_NoVeg*PropValue


  # Get the erosion damage totals across the study area
    erosion_totals <- ErosionTotals(wave_data = wave_data,
                                    erosion = erosion,
                                    Longshore = ShorelinePointDist)

  # Damage from a single sotrm
    erosion_totals[["s_storm_damage_Veg"]] <- erosion_totals$total_erosion_Veg_m2*PropValue
    erosion_totals[["s_storm_damage_NoVeg"]] <- erosion_totals$total_erosion_NoVeg_m2*PropValue

  # Build flood contours (and rasters of water surface elevation)
  # over the study area.
  flood_contours <- FloodContours(TopoBathy = TopoBathy,
                                  mean_high_water = mean_high_water,
                                  total_wsl_adj = total_wsl_adj,
                                  erosion_totals = erosion_totals)

  # Calculate the total flood damage cost using depth damage curves
  dd_flood <- DepthDamageFlood(Bldgs = Bldgs,
                               flood_contours = flood_contours,
                               HAZUS = HAZUS)


  # Calculate long term flood damage cost
  LongTermDamage <- function(single_storm_damage = NA, Tr = NA, disc = NA, TimeHoriz = NA){
    p = 1.0 / Tr  # return frequency
    temp1 = 1.0 + disc # Discount rate
    temp2 = (1/temp1)**(seq(1:TimeHoriz))
    # Total erosion area
    long_term_damage = p * single_storm_damage * sum(temp2)
    return(long_term_damage)
  }

  dd_flood$NoVegDamage_longterm <- LongTermDamage(
    single_storm_damage = dd_flood$NoVegDamage,
    Tr = Tr, disc = disc, TimeHoriz = TimeHoriz)

  dd_flood$VegDamage_longterm <- LongTermDamage(
    single_storm_damage = dd_flood$VegDamage,
    Tr = Tr, disc = disc, TimeHoriz = TimeHoriz)




  #=====================================================
  # GENERATE SUMMARY OBJECT
  #=====================================================

  # Add values to erosion totals
  ero_tot <- erosion_totals

  # Add features to export variable
  ero_tot[["NoVeg_nStructure"]] <- dd_flood$NoVeg_nStructure
  ero_tot[["Veg_nStructure"]] <- dd_flood$Veg_nStructure
  ero_tot[["NoVeg_MedianDepth"]] <- round(dd_flood$NoVeg_MedianDepth, 2)
  ero_tot[["Veg_MedianDepth"]] <- round(dd_flood$Veg_MedianDepth, 2)
  ero_tot[["NoVeg_MaxDepth"]] <- round(dd_flood$NoVeg_MaxDepth, 2)
  ero_tot[["Veg_MaxDepth"]] <- round(dd_flood$Veg_MaxDepth, 2)
  ero_tot[["NoVegDamage"]] <- round(dd_flood$NoVegDamage, 0)
  ero_tot[["VegDamage"]] <- round(dd_flood$VegDamage, 0)

  ero_tot[["FloodLTDamageVeg"]] <- round(dd_flood$VegDamage_longterm , 0)
  ero_tot[["FloodLTDamageNoVeg"]] <- round(dd_flood$NoVegDamage_longterm, 0)



  # Add storm simulation conditions
  ero_tot[["Ho"]] <- Ho
  ero_tot[["To"]] <- To
  ero_tot[["storm_duration"]] <- storm_duration
  ero_tot[["surge_elevation"]] <- surge_elevation
  ero_tot[["PropValue"]] <- PropValue
  ero_tot[["Tr"]] <- Tr
  ero_tot[["disc"]] <- disc
  ero_tot[["TimeHoriz"]] <- TimeHoriz
  ero_tot[["mean_sea_level"]] <- mean_sea_level
  ero_tot[["mean_high_water"]] <- mean_high_water
  ero_tot[["sea_level_rise"]] <- sea_level_rise

  # Add on transect dune parameters
  ero_tot$erosion_points <- merge(ero_tot$erosion_points, linkbeach,
                                  by.x="line_id",
                                  by.y="line_id",
                                  all.x=TRUE,
                                  all.y=FALSE)


  #=========================================
  # Prep objects
  flood_contour <- flood_contours[["contours"]]

  merg_Veg <- Vegetation
  merg_Veg$StemHeight <- merg_Veg$hc
  merg_Veg$StemDiam <- merg_Veg$d
  merg_Veg$StemDensty <- merg_Veg$N

  wave_dat <- wave_data

  dat <- dat_veg

  #=========================================
  # Run JSON export

  ExportJSONContent(
    path_output = path_output,
    flood_contour = flood_contour,
    merg_Veg = merg_Veg,
    wave_dat = wave_dat,
    dat = dat
  )




}
