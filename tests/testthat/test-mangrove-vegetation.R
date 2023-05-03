testthat::test_that("mangrove vegetation", {
  # Adjust Inputs

  RadLineDist = 1.5
  MaxOnshoreDist = 0.01
  ShorelinePointDist = 150
  BufferDist = 50

  SmoothParameter = 5

  storm_duration = 3

  total_wsl_adj = 5
  Ho = 2
  To = 4
  me = 0.001
  Cm = 70

  # Foreshore slope
  m <- 0.001

  tran_force = TRUE
  print_debug = TRUE

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



  ###########################################################
  # Generate cross-shore profile lines along the coastline.
  ###########################################################
  # library(MNAI.CPBT)
  data(Coastline)

  crossshore_profiles <- samplePoints(
    Coastline = Coastline,
    ShorelinePointDist = ShorelinePointDist,
    BufferDist = BufferDist,
    RadLineDist = RadLineDist
  )
  crossshore_lines <- crossshore_profiles[[2]]


  ###########################################################
  # Extract elevation values along each profile
  ###########################################################

  rpath <-
    system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
  TopoBathy <- raster::raster(rpath)
  pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)

  # Run SignalSmooth function to smooth elevation profiles
  pt_elevs <- SignalSmooth(point_elev = pt_elevs,
                           SmoothParameter = SmoothParameter)


  ###########################################################
  # Clean the cross-shore profiles with CleanTransect
  ###########################################################

  data(Trimline)
  cleantransect <- CleanTransect(
    point_elev = pt_elevs,
    RadLineDist = RadLineDist,
    MaxOnshoreDist = MaxOnshoreDist,
    trimline = Trimline
  )



  ###########################################################
  # Merge vegetation onto lines
  ###########################################################

  data(Vegetation)

  # test no veg
  dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = NA)

  expect_true(all(is.na(dat_veg$StemHeight)))

  dat_veg <-
    ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)

  # unique(dat_veg$StemHeight)



  # # Match Python Exact
  # if (FALSE) {
  #   df <-
  #     read.table(
  #       "C:/Users/mattj/Dropbox/Projects/EN2775 Panama/invest/demo/SmoothProfileCutGIS.txt"
  #     )
  #   dat_veg <-
  #     data.frame(
  #       line_id = 2,
  #       elev = df$V2,
  #       elev_smooth = df$V2,
  #       Xpos = df$V1,
  #       Type = NA,
  #       StemHeight = NA,
  #       StemDiam = NA,
  #       StemDensty = NA,
  #       Cd = NA
  #     )
  #
  #   dat_veg$StemHeight <-
  #     ifelse(dat_veg$Xpos <= 9999 & dat_veg$Xpos >= -9999, 1, NA)
  #   dat_veg$Type <-
  #     ifelse(dat_veg$Xpos <= 9999 & dat_veg$Xpos >= -9999, "mangrove", NA)
  #   dat_veg$StemDiam <-
  #     ifelse(dat_veg$Xpos <= 9999 & dat_veg$Xpos >= -9999, 0.1, NA)
  #   dat_veg$StemDensty <-
  #     ifelse(dat_veg$Xpos <= 9999 & dat_veg$Xpos >= -9999, 0.1, NA)
  #   dat_veg$Cd <-
  #     ifelse(dat_veg$Xpos <= 9999 & dat_veg$Xpos >= -9999, 1, NA)
  # }




  # Run the wave evolution model - high water
  wm1 <- WaveModel(
    dat = dat_veg,
    total_wsl_adj = total_wsl_adj,
    Ho = Ho,
    To = To,
    mangrove = mangrove,
    # will override veg attributes
    tran_force = tran_force,
    print_debug = print_debug
  )

  # Preview individual transect
  table(wm1$line_id)
  dsub <- wm1[wm1$line_id == 2, ]

  # Plot cross-shore profile
  par(mfrow = c(2, 1))
  plot(
    dsub$Xpos,
    dsub$elev,
    type = "l",
    xlab = "Cross-shore Distance (m)",
    ylab = "Elevation (m) Chart Datum",
    main = "ELEVATION PROFILE"
  )
  points(dsub$Xpos,
         dsub$elev_smooth,
         col = "red",
         type = "l")

  # Add MLLW water line
  abline(h = 0,
         lty = 2,
         col = "blue")

  # Look at the wave height (without vegetation)
  plot(
    dsub$Xpos,
    dsub$H_noveg,
    type = 'l',
    xlab = "Cross-shore Distance (m)",
    ylab = "Wave Height (m)",
    main = "WAVE ATTENUATION"
  )
  # Add on wave height with vegetation
  points(dsub$Xpos,
         dsub$H_veg,
         col = "green",
         type = "l")


  # Look at the wave height (without vegetation)
  plot(
    dsub$Xpos,
    dsub$Ubots,
    type = 'l',
    xlab = "Cross-shore Distance (m)",
    ylab = "B Velocity (m/s)",
    main = "BOTTOM VELOCITY"
  )
  # Add on wave height with vegetation
  points(dsub$Xpos, dsub$Ubot, col = "green", type = "l")




  # ================================================================
  # Test the MudErosion function

  # Run mud erosion function without vegetation
  ero <-
    MudErosion(
      Uc = dsub$Ubots * 0,
      Uw = dsub$Ubots,
      h = dsub$elev_smooth,
      To = To,
      me = me,
      Cm = Cm
    )
  Retreat1 <- ero$Erosion
  Trms1 <- ero$Trms
  Tc1 <- ero$Tc
  Tw1 <- ero$Tw
  Te <- ero$Te

  # Run mud erosion function with vegetation
  ero2 <-
    MudErosion(
      Uc = dsub$Ubot * 0,
      Uw = dsub$Ubot,
      h = dsub$elev_smooth,
      To = To,
      me = me,
      Cm = Cm
    )

  Retreat2 <- ero2$Erosion
  Trms2 <- ero2$Trms
  Tc2 <- ero2$Tc
  Tw2 <- ero2$Tw
  Te <- ero2$Te
  # ero$Te



  # Look at erosion rates pre-management and post-management
  # see lines 2477 in python...

  # Erosion (without vegetation)
  ErodeLoc = which(Trms1 > ero$Te[1])

  # Indices where erosion rate greater than Threshold
  # Zero must be the location of the water at lower low water
  Zero = max(dsub$Xpos) # 507

  ErodeLoc = ErodeLoc[ErodeLoc >= Zero]
  dx <- 1
  # Erosion rate greater than Threshold at each location
  # shoreward of the shoreline (pre managament)

  # Erosion Length m (without vegetation):
  MErodeLen1 = len(ErodeLoc) * dx

  # Erosion volume
  MErodeVol1 <-
    pracma::trapz(x = abs(dsub$Xpos[ErodeLoc]), y = Retreat1[ErodeLoc] / 100.0) * storm_duration
  # Volume of mud eroded shoreward of the shoreline (m^3/m)


  # ---------------------------------------------------------
  # Erosion (with vegetation)
  ErodeLoc = which(Trms2 > ero2$Te[1])

  # Indices where erosion rate greater than Threshold
  # Zero must be the location of the water at lower low water
  Zero = max(dsub$Xpos) # 507

  ErodeLoc = ErodeLoc[ErodeLoc >= Zero]
  dx <- 1
  # Erosion rate greater than Threshold at each location
  # shoreward of the shoreline (pre managament)

  # Erosion Length m (without vegetation):
  MErodeLen2 = len(ErodeLoc) * dx

  # Erosion volume
  MErodeVol2 <-
    pracma::trapz(x = abs(dsub$Xpos[ErodeLoc]), y = Retreat2[ErodeLoc] / 100.0) * storm_duration
  # Volume of mud eroded shoreward of the shoreline (m^3/m)



  print(MErodeLen1)
  print(MErodeLen2)




  #----------------------------------------------------------
  # Calculate Wave Runup for mudflats
  #----------------------------------------------------------

  math.pi = pi

  g = 9.81
  Hshortwave = Ho
  #HshortwaveMA = Ho
  Values = 100
  Etasimple1 = dsub$Etas # Wave setup without vegetation - to modify runup

  Setup = dsub$Eta       # Wave setup with vegetation
  #print(summary(Setup))

  #======================================
  # Calculate Wave Runup
  #======================================

  # estimate runup amount
  Lo = g * To ** 2.0 / (2.0 * math.pi)

  # before management action
  # run-up - short and long waves (run-up w/o vegetation effect)
  Rnp1 = 1.1 * (0.35 * m * math.sqrt(Hshortwave * Lo) + math.sqrt(Lo * (Hshortwave * 0.563 * m **
                                                                          2.0 + Ho * 0.004)) / 2.0)

  # setup at the beach
  Emax = 0.35 * m * math.sqrt(Hshortwave * Lo)

  # correction factor for MWL
  coef0 = max(Etasimple1[2:(length(Etasimple1)-1)], na.rm=TRUE) / Emax

  # corrected MWL at shoreline in presence of habitat
  Etap = max(Setup, na.rm=TRUE) / coef0

  # MJB Fix in ESRI
  if(is.na(Etap)){
    Etap = 0
  }
  # if Eta with vegetation is negative, take as zero
  if(Etap < 0){
    Etap = 0
  }

  # Hprime to estimate run-up with vegetation
  Hp = (Etap / (0.35 * m)) ** 2 / Lo

  # run-up with vegetation
  Rnpveg1 = 1.1 * (Etap + math.sqrt(Lo * (Hp * 0.563 * m ** 2 + Ho * 0.004)) / 2.0)



  # run-up without vegetation
  Rnp1

  # run-up with vegetation
  Rnpveg1








})
