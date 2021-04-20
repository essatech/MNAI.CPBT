testthat::test_that("Wave model and veg test", {


  ###########################################################
  # Generate cross-shore profile lines along the coastline.
  ###########################################################

  data(Coastline)

  crossshore_profiles <- samplePoints(
    Coastline = Coastline,
    ShorelinePointDist = 150,
    BufferDist = 50,
    RadLineDist = 1.5
  )
  crossshore_lines <- crossshore_profiles[[2]]


  ###########################################################
  # Extract elevation values along each profile
  ###########################################################

  rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
  TopoBathy <- raster::raster(rpath)
  pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)

  # Run SignalSmooth function to smooth elevation profiles
  pt_elevs <- SignalSmooth(point_elev = pt_elevs,
  SmoothParameter = 5)


  ###########################################################
  # Clean the cross-shore profiles with CleanTransect
  ###########################################################

  data(Trimline)
  cleantransect <- CleanTransect(
    point_elev = pt_elevs,
    RadLineDist = 1.5,
    MaxOnshoreDist = 0.01,
    trimline = Trimline
  )


  ###########################################################
  # Merge vegetation onto lines
  ###########################################################

  data(Vegetation)

  # test no veg
  dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = NA)

  expect_true(all(is.na(dat_veg$StemHeight)))

  dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)

  # Run the wave evolution model - high water
  wm1 <- WaveModel(dat = dat_veg,
    total_wsl_adj = 0.7,
    Ho = 5,
    To = 8
  )

  # Run the wave evolution model - lower low water
  wm2 <- WaveModel(dat = dat_veg,
                         total_wsl_adj = -0.5,
                         Ho = 1,
                         To = 4
  )

  m1 <- min(wm1$H_noveg, na.rm=TRUE)
  m2 <- min(wm2$H_noveg, na.rm=TRUE)

  expect_true(m1 > m2)

  m1 <- max(wm1$H_noveg, na.rm=TRUE)
  m2 <- max(wm2$H_noveg, na.rm=TRUE)

  expect_true(m1 > m2)


  m1 <- mean(wm1$H_noveg, na.rm=TRUE)
  m2 <- mean(wm2$H_noveg, na.rm=TRUE)

  expect_true(m1 > m2)


  ###########################################################
  # Test profile link
  ###########################################################

  plink1 <- LinkProfilesToBeaches(BeachAttributes = BeachAttributes, dat = wm1)
  plink2 <- LinkProfilesToBeaches(BeachAttributes = BeachAttributes, dat = wm2)

  expect_type(plink1, "list")
  expect_type(plink2, "list")
  expect_true(nrow(plink1) > 0)
  expect_true(nrow(plink2) > 0)



  ###########################################################
  # Test erosion model
  ###########################################################

  erosion <- ErosionTransectsUtil(Ho=2,
                                  To=6,
                                  total_wsl_adj = 0.5,
                                  fs_dat = plink1,
                                  wave_dat = wm1,
                                  storm_duration = 3,
                                  Tr = 10,
                                  Longshore = NA,
                                  PropValue = 200,
                                  disc = 0.05,
                                  TimeHoriz = 50)

  expect_true(nrow(erosion) > 0)

  erosion2 <- ErosionTransectsUtil(Ho=5,
                                  To=9,
                                  total_wsl_adj = 1,
                                  fs_dat = plink1,
                                  wave_dat = wm1,
                                  storm_duration = 9,
                                  Tr = 86,
                                  Longshore = 150,
                                  PropValue = 200,
                                  disc = 0.05,
                                  TimeHoriz = 200)

  expect_true(nrow(erosion2) > 0)

  expect_true(sum(erosion2$damage_NoVeg)  > 10000)
  expect_true(sum(erosion2$damage_Veg) < sum(erosion2$damage_NoVeg))

  erosion3 <- ErosionTransectsUtil(Ho = 5,
                                  To = 9,
                                  total_wsl_adj = 1,
                                  fs_dat = plink1,
                                  wave_dat = wm1,
                                  storm_duration = 9,
                                  Tr = 86,
                                  Longshore = 150,
                                  PropValue = 200,
                                  disc = 1,
                                  TimeHoriz = 200)


  expect_true(sum(erosion2$damage_NoVeg) > sum(erosion3$damage_NoVeg))
  expect_true(sum(erosion3$damage_Veg) < sum(erosion3$damage_NoVeg))
  expect_true(sum(erosion2$vol_loss_Veg) == sum(erosion3$vol_loss_Veg))
  expect_true(sum(erosion2$vol_loss_NoVeg) == sum(erosion3$vol_loss_NoVeg))
  expect_true(sum(erosion2$retreat_pct_NoVeg) == sum(erosion3$retreat_pct_NoVeg))



  ###########################################################
  # Erosion summaries total across coastline
  ###########################################################

  # Summarize shoreline erosion across whole shoreline
  ero_tot1 <- ErosionTotals(dat = wm1,
                                erosion = erosion2,
                                Longshore = 150)

  ero_tot2 <- ErosionTotals(dat = wm2,
                           erosion = erosion3,
                           Longshore = 150)


  expect_type(ero_tot1, "list")
  expect_type(ero_tot2, "list")

  expect_type(ero_tot1$erosion_points, "list")
  expect_type(ero_tot2$erosion_points, "list")

  expect_type(ero_tot1$volume_NoVeg, "double")
  expect_type(ero_tot2$volume_NoVeg, "double")



  ###########################################################
  # Test export function
  ###########################################################

  ret1 <- ExportProfiles(
    dat = dat_veg,
    wave_dat = wm2,
    erosion = erosion,
    mean_high_water = 1,
    total_wsl_adj = 1.2,
    TrimOnshoreDist = NA,
    path_output = NA,
    export_csv = FALSE
  )
  expect_type(ret1, "NULL")

  if(FALSE){
    ret1 <- ExportProfiles(dat = dat_veg, wave_dat = wm2, erosion = erosion,
                           mean_high_water = 1, total_wsl_adj = 1.2,
                           TrimOnshoreDist = NA,
                           path_output = 'C:/Users/mbayly/Desktop/delete/',
                           export_csv = TRUE)
  }


  ###########################################################
  # Test contour generator
  ###########################################################

  fc <- FloodContours(TopoBathy = TopoBathy, mean_high_water = 1, total_wsl_adj = 0.8, erosion_totals = ero_tot2)

  expect_type(fc, "list")
  expect_type(fc$r_d_noveg, "S4")
  expect_type(fc$r_d_veg, "S4")
  expect_type(fc$contours, "list")
  expect_true(nrow(fc$contours) > 2)
  expect_true('HighTide' %in% c(fc$contours$name))
  ctrs <- fc$contours
  expect_true(as.numeric(sum(sf::st_length(ctrs))) > 100)
  # mapview(ctrs)


})
