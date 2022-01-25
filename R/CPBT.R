#' @title Coastal Protection Benefit Tool
#'
#' @description Runs the complete CPBT analysis along a coastline for a given
#' storm simulation.
#'
#' @param simulation_name Simulation name for current storm and conditions.
#' @param dir_output Local output directory for CPBT simulation results.
#' @param Scenario_Description A qualitative description of the scenario.
#' @param Coastline Linestring of class sf and dataframe. Coastline spatial
#' line segment for modeling. The coastline should roughly trace the water
#' edge along the beach at the mean sea level. It is recommended to keep the coastline geometry simple and avoid sharp corners.
#' @param ShorelinePointDist Numeric. Spacing between cross-shore profile lines (meters).
#'  Note that it is recommended to keep this number as large as possible for
#'  your area of interest to speed up processing time.
#' @param BufferDist Numeric. Buffer search distance (meters) used to identify
#' the perpendicular angle of each cross-shore profile line from the coastline
#' (recommended values are 100-300).
#' @param RadLineDist Numeric. Radial line distance (kilometers) of the cross-shore profiles. This value determines how for offshore (and onshore) the cross-shore profiles should extend (recommended values are 1 - 3 km).
#' @param TopoBathy TopoBathy digital elevation model of class RasterLayer.
#' @param SmoothParameter Numeric, smoothing window length as a percentage of
#' the cross-shore profile length (0-100). A value of zero means no smoothing, but see
#' details below (recommended values are 5-20).
#' @param MaxOnshoreDist Numeric. Maximum radial line distance onshore in kilometers.
#' Note that in some instances the onshore extent should be truncated
#' significantly for the wave evolution model. It is recommended to keep this
#' value below 1km.
#' @param trimline Optional back shore trim line used to restrict the on-shore
#' extent of the cross-shore profiles. The default value is NA (not used). If
#' used, the back shore trim line should run parallel to the coastline but be
#' set back onto land. A back shore trim line may be required in cases where
#' cross-shore profiles are generated along the coastline of a narrow peninsula
#' or in instances where there is a back shore lagoon. A back shore trimline
#' should be provided as a simple features line object.
#' @param Vegetation A simple feature polygon of class sf and dataframe.
#' @format Patch attributes must include.
#' \describe{
#'   \item{hc}{Numeric, Blade height in meters}
#'   \item{N}{Numeric, Shoot density as number of shoots per meter squared.}
#'   \item{d}{Numeric, Blade width in meters.}
#'   \item{Type}{Either 'Eelgrass', 'Kelp' or 'Marsh'.}
#'   \item{Cd}{Numeric, Drag coefficent as per Guannel et al 2015.
#'   If unsure use a value of 0.1 for Eelgrass}
#' }
#' @param mean_high_water Mean high tidal water elevation above the chart datum.
#' @param mean_sea_level Mean average tidal water elevation above the chart datum.
#' @param tide_during_storm Numeric, Tidal elevation in meters during the storm referenced to the Chart Datum.
#' @param surge_elevation Numeric, Additional elevation from storm surge (in meters) during the storm.
#' @param sea_level_rise Numeric, Additional elevation from any local sea-level rise (in meters) during the storm.
#' @param Ho Initial offshore wave height in meters.
#' @param To Initial offshore wave period in seconds.
#' @param storm_duration Numeric. Storm duration in hours.
#' @param BeachAttributes Spatial polygons of class sf and dataframe for
#' @format foreshore beach attributes. Each beach section should be characterized with field attributes:
#' \describe{
#'   \item{slope}{the foreshore slope (slope) as rise over run.}
#'   \item{W}{Numeric, the berm width in meters (W).}
#'   \item{B}{Numeric, berm height in meters (B).}
#'   \item{D}{Numeric, dune height (D) in meters.}
#'   \item{sediment}{Numeric, the sediment grain size in
#' millimeters (sediment).}
#'   \item{V}{Numeric, the beach value per meter squared in dollars (V).}
#' }
#' @param Tr Numeric. Return period (frequency) of the simulated storm (in years).
#' @param PropValue Generaly land value in dollars per square meter of beach.
#' (not used if beach polygon property values are provided).
#' @param disc Annual valuation discount rate over the time horizon (0 - 1).
#' @param TimeHoriz Time horizon (in years) for long term cumulative valuation given a storm return frequency.
#' Typically 100-year horizons are used.
#' @param Bldgs Spatial polygon of building footprints of class sf and dataframe.
#'  See data(Bldgs) for an example input format.
#' @format Building footprint attribute fields must include columns:
#' \describe{
#'   \item{DDID}{Numeric, the HAZUS depth-damage curves IDs populated for
#' each structure (column name: DDID).}
#'   \item{VAL}{Numeric, estimates of the replacement value cost
#' for each structure in dollars (VAL).}
#' }
#' @param export_report Boolean for debugging. Keep set at TRUE.
#' @param export_spatial_dat Boolean (TRUE FALSE) should spatial data be exported for report.
#'
#'
#' @return Export a html output report folder to a local directory with the
#' complete CPBT results summary.
#'
#' @examples
#' \dontrun{
#'
#' library(MNAI.CPBT)
#'
#' data(Coastline)
#' data(BeachAttributes)
#' rpath <- system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
#' TopoBathy <- raster::raster(rpath)
#' data(Vegetation)
#' data(Bldgs)
#'
#' CPBT(
#'   simulation_name = "My Simulation",
#'   dir_output = getwd(),
#'   Coastline = Coastline,
#'   ShorelinePointDist = 50,
#'   BufferDist = 25,
#'   RadLineDist = 1,
#'   TopoBathy = TopoBathy,
#'   SmoothParameter = 5,
#'   MaxOnshoreDist = 0.1,
#'   trimline = NA,
#'   Vegetation = Vegetation,
#'   mean_high_water = 1.5,
#'   mean_sea_level = 0.5,
#'   tide_during_storm = 0.9,
#'   surge_elevation = 0.2,
#'   sea_level_rise = 0.15,
#'   Ho = 3,
#'   To = 9,
#'   storm_duration = 6,
#'   BeachAttributes = BeachAttributes,
#'   Tr = 50,
#'   PropValue = 200,
#'   disc = 0.03,
#'   TimeHoriz = 200,
#'   Bldgs = Bldgs,
#'   export_report = FALSE
#' )
#'
#' }
#' @export
CPBT <- function(
  simulation_name = "My Simulation",
  Scenario_Description = NA,
  dir_output = getwd(),
  Coastline = NA,
  ShorelinePointDist = 200,
  BufferDist = 50,
  RadLineDist = 1,
  TopoBathy = NA,
  SmoothParameter = 5,
  MaxOnshoreDist = 0.1,
  trimline = NA,
  Vegetation = NA,
  mean_high_water = 1.5,
  mean_sea_level = 0.5,
  tide_during_storm = 0.9,
  surge_elevation = 0.5,
  sea_level_rise = 0.15,
  Ho = 2.5,
  To = 7,
  storm_duration = 3,
  BeachAttributes = NA,
  Tr = 10,
  PropValue = 200,
  disc = 0.05,
  TimeHoriz = 100,
  Bldgs = NA,
  export_report = FALSE,
  export_spatial_dat = FALSE
) {


  pdg <- function(msg){
    if(TRUE){
      print(msg)
    }
  }

  pdg("Start function....")



  #=====================================================
  # Assign projection to raster if missing
  #=====================================================
  p1 <- sp::proj4string(TopoBathy)
  # Set crs for raster if missing
  if(is.na(p1)) {
    print("Projection missing for raster...")
    print("Assume CRS is the same as perp lines...")
    temp_pt <- sf::as_Spatial(Coastline)
    p2 <- suppressWarnings(sp::proj4string(temp_pt))
    raster::crs(TopoBathy) <- p2
    p1 <- sp::proj4string(TopoBathy)
  }


  # Also check and assign projection of other objects (if missing)
  temp_pt <- sf::as_Spatial(Coastline)
  temp_pt <- sf::st_as_sf(temp_pt)

  test_pj <- sf::st_crs(Bldgs)
  if(is.na(test_pj$epsg)) {
    sf::st_crs(Bldgs) <-  sf::st_crs(temp_pt)
  }
  test_pj <- sf::st_crs(Coastline)
  if(is.na(test_pj$epsg)) {
    sf::st_crs(Coastline) <-  sf::st_crs(temp_pt)
  }
  test_pj <- sf::st_crs(Vegetation)
  if(is.na(test_pj$epsg)) {
    sf::st_crs(Vegetation) <-  sf::st_crs(temp_pt)
  }
  if(!(is.na(trimline))) {
    test_pj <- sf::st_crs(trimline)
    if(is.na(test_pj$epsg)) {
      sf::st_crs(trimline) <-  sf::st_crs(temp_pt)
    }
    print(sf::st_crs(trimline)$epsg)
  }

  print("Check projections of all sf objects...")
  print(sf::st_crs(Coastline)$epsg)
  print(sf::st_crs(Bldgs)$epsg)
  print(sf::st_crs(Vegetation)$epsg)
  print(sf::st_crs(Coastline)$epsg)



  #=====================================================
  # Prep OUTPUT FOLDER
  #=====================================================

  # If no description is supplied use name

  if(is.na(Scenario_Description)) {
    Scenario_Description <- simulation_name
  }


  # Create Output Directory
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  rR  <- substrRight(dir_output, 1)
  iR <- ifelse(rR == "/", "", "/")

  path_output <- paste0(dir_output, iR, simulation_name, "/")


  pdg("Look up system file....")

  path_www <- system.file("extdata", "www", package = "MNAI.CPBT")
  path_html <- system.file("extdata", "SimulationResults.html", package = "MNAI.CPBT")

  pdg("Create directory for writting and copy content....")

  if(export_report){
    dir.create(path_output)
    file.copy(path_www, path_output, recursive=TRUE)
    file.copy(path_html, path_output, recursive=TRUE)
  }



  #=====================================================
  # RUN SUB MODELS
  #=====================================================
  pdg("Start submodels....")

  # Set the total still water level
  total_wsl_adj <- tide_during_storm + surge_elevation + sea_level_rise
  total_water_level_erosion <- mean_sea_level + surge_elevation + sea_level_rise

  # Keep these the same
  Longshore = ShorelinePointDist

  pdg("Get HAZUS data....")
  #data(HAZUS, envir = environment())
  HAZUS <- HAZUS


  # Generate cross-shore profile lines along the coastline.
  pdg("Generate profiles...")
  pdg(nrow(Coastline))
  pdg(class(Coastline))
  print(sf::st_length(Coastline))
  print(sf::st_geometry_type(Coastline))
  print(ShorelinePointDist)
  print(BufferDist)
  print(RadLineDist)


  crossshore_profiles <- samplePoints(
     Coastline = Coastline,
     ShorelinePointDist = ShorelinePointDist,
     BufferDist = BufferDist, RadLineDist = RadLineDist
  )

  # Get the lines
  pdg("Get lines...")
  crossshore_lines <- crossshore_profiles[[2]]

  # Extract elevation values along each profile
  pdg("ExtractElev...")
  print(TopoBathy)
  print(head(crossshore_lines))

  pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)


  # Run SignalSmooth function to smooth elevation profiles
  pdg("SignalSmooth....")
  pt_elevs <- SignalSmooth(point_elev = pt_elevs,
                           SmoothParameter = SmoothParameter)


  # Clean the cross-shore profiles with CleanTransect
  pdg("CleanTransect....")
  pdg(RadLineDist)
  pdg(MaxOnshoreDist)

  cleantransect <- suppressWarnings(CleanTransect(
    point_elev = pt_elevs,
    RadLineDist = RadLineDist,
    MaxOnshoreDist = MaxOnshoreDist,
    trimline = trimline
    ))

  # Merge vegetation onto lines
  pdg("ExtractVeg....")
  dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)


  # Run the wave evolution model
  pdg("WaveModel....")
  wave_data <- WaveModel(
    dat = dat_veg,
    total_wsl_adj = total_water_level_erosion,
    Ho = Ho, To = To
    )

  pdg(paste0("Ok transects: ", length(unique(wave_data$line_id))))

  # Link data to foreshore beach attributes
  pdg("LinkProfilesToBeaches....")
  linkbeach <- LinkProfilesToBeaches(BeachAttributes = BeachAttributes,
                                     dat = wave_data)

  # Run the erosion model
  pdg("ErosionTransectsUtil....")
  print(surge_elevation)
  print(total_water_level_erosion)

  erosion <- ErosionTransectsUtil(
     Ho = Ho, To = To,
     total_wsl_adj = total_water_level_erosion,
     linkbeach = linkbeach,
     wave_data = wave_data,
     storm_duration = storm_duration,
     Tr = Tr,
     Longshore = ShorelinePointDist,
     PropValue = PropValue,
     disc = disc,
     TimeHoriz = TimeHoriz
     )


  pdg("Build export objects....")


  erosion$damage_t_Veg_ss <- erosion$area_loss_Veg*PropValue
  erosion$damage_t_NoVeg_ss <- erosion$area_loss_NoVeg*PropValue
  erosion$LTEdamage_NoVeg <- round(erosion$damage_NoVeg, 0)
  erosion$LTEdamage_Veg <- round(erosion$damage_Veg, 0)


  # Get the erosion damage totals across the study area
    erosion_totals <- ErosionTotals(wave_data = wave_data,
                                    erosion = erosion,
                                    Longshore = ShorelinePointDist)

    erosion_totals$total_damage_Veg <- round(erosion_totals$total_damage_Veg,0)
    erosion_totals$total_damage_NoVeg <- round(erosion_totals$total_damage_NoVeg,0)
    erosion_totals$total_erosion_Veg_m2 <- round(erosion_totals$total_erosion_Veg_m2,0)
    erosion_totals$total_erosion_NoVeg_m2 <- round(erosion_totals$total_erosion_NoVeg_m2,0)



  # Damage from a single sotrm
    erosion_totals[["s_storm_damage_Veg"]] <- erosion_totals$total_erosion_Veg_m2*PropValue
    erosion_totals[["s_storm_damage_NoVeg"]] <- erosion_totals$total_erosion_NoVeg_m2*PropValue

  # Build flood contours (and rasters of water surface elevation)
  # over the study area.
  pdg("Run flood contours....")
  flood_contours <- FloodContours(TopoBathy = TopoBathy,
                                  mean_high_water = mean_high_water,
                                  total_wsl_adj = total_wsl_adj,
                                  erosion_totals = erosion_totals)

  # Calculate the total flood damage cost using depth damage curves
  pdg("Run DepthDamageFlood....")
  dd_flood <- DepthDamageFlood(Bldgs = Bldgs,
                               flood_contours = flood_contours,
                               HAZUS = HAZUS)


  # Calculate long term flood damage cost
  pdg("Calc long term damage....")
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
  pdg("Build summary objects for json...")

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
  ero_tot[["Scenario_Description"]] <- Scenario_Description

  # Single storm damage totals erosion




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
  ero_tot[["tide_during_storm"]] <- tide_during_storm
  ero_tot[["total_wsl_adj"]] <- total_wsl_adj
  ero_tot[["total_wsl_adj"]] <- total_wsl_adj

  # Add on transect dune parameters
  pdg("Merge exports...")
  ero_tot$erosion_points <- merge(ero_tot$erosion_points, linkbeach,
                                  by.x="line_id",
                                  by.y="line_id",
                                  all.x=TRUE,
                                  all.y=FALSE)


  #=========================================
  # Prep objects
  flood_contour <- flood_contours[["contours"]]


  if(length(Vegetation) != 1) {

    if(class(Vegetation)[1] == "sf") {

      merg_Veg <- Vegetation
      merg_Veg$StemHeight <- merg_Veg$hc
      merg_Veg$StemDiam <- merg_Veg$d
      merg_Veg$StemDensty <- merg_Veg$N

    }

  } else {
    merg_Veg <- NA
  }


  wave_dat <- wave_data

  dat <- dat_veg

  #=========================================
  # Run JSON export

  pdg("ExportJSONContent...")

  if(export_report){

    ExportJSONContent(
      path_output = path_output,
      flood_contour = flood_contour,
      merg_Veg = merg_Veg,
      wave_dat = wave_dat,
      dat = dat,
      ero_tot = ero_tot,
      total_wsl_adj = total_wsl_adj,
      export_spatial_dat = export_spatial_dat
    )

    # Fix has
    pdg("Set the hash to a valid transect.... ")
    tselect <- unique(wave_dat$line_id)[1]
    new <- paste0("        location.hash = '#profile=", tselect, "';")
    html_file <- paste0(path_output, "SimulationResults.html")
    htmltxt = readLines(html_file,-1)
    htmltxt[48] = new
    writeLines(htmltxt,html_file,-1, sep="\r\n")


  }








}
