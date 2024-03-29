testthat::test_that("Test full CPBT", {

  data(Coastline)
  data(BeachAttributes)
  suppressWarnings(rm(TopoBathy))
  rpath <-
    system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
  TopoBathy <- raster::raster(rpath)
  expect_type(sp::proj4string(TopoBathy), 'character')

  #plot(TopoBathy)
  data(Vegetation)
  data(Bldgs)

  ret <- CPBT(
    simulation_name = "My Simulation",
    dir_output = getwd(),
    Coastline = Coastline,
    ShorelinePointDist = 50,
    BufferDist = 25,
    RadLineDist = 1,
    TopoBathy = TopoBathy,
    SmoothParameter = 5,
    MaxOnshoreDist = 0.1,
    trimline = NA,
    Vegetation = Vegetation,
    mean_high_water = 1.5,
    mean_sea_level = 0.5,
    tide_during_storm = 0.9,
    surge_elevation = 0.2,
    sea_level_rise = 0.15,
    Ho = 3,
    To = 9,
    storm_duration = 6,
    BeachAttributes = BeachAttributes,
    Tr = 50,
    PropValue = 200,
    disc = 0.03,
    TimeHoriz = 200,
    Bldgs = Bldgs,
    export_report = FALSE
  )

  # func finish
  expect_type(ret, 'NULL')




  # ============================================
  # Test Full CPBT Mangrove extension
  # ============================================

  BeachAttributes_Mangrove <- BeachAttributes
  BeachAttributes_Mangrove$slope <- 0.001

  mangrove <-
    c(
      "NRoots" = 15,
      "dRoots" = 0.1,
      "hRoots" = 1,
      "NCanop" = 10,
      "dCanop" = 0.5,
      "hCanop" = 3,
      "NTrunk" = 1.7,
      "dTrunk" = 0.4,
      "hTrunk" = 5
    )

  retm <- CPBT(
    simulation_name = "My Simulation Mangrove",
    dir_output = getwd(),
    Coastline = Coastline,
    ShorelinePointDist = 50,
    BufferDist = 25,
    RadLineDist = 1,
    TopoBathy = TopoBathy,
    SmoothParameter = 5,
    MaxOnshoreDist = 0.1,
    trimline = NA,
    Vegetation = Vegetation,
    mangrove = mangrove,
    mean_high_water = 3,
    mean_sea_level = 0.5,
    tide_during_storm = 0.9,
    surge_elevation = 0.2,
    sea_level_rise = 0.15,
    Ho = 2,
    To = 4,
    storm_duration = 6,
    BeachAttributes = BeachAttributes_Mangrove,
    Tr = 50,
    PropValue = 200,
    disc = 0.03,
    TimeHoriz = 200,
    Bldgs = Bldgs,
    export_report = FALSE
  )




})
