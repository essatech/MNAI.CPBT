#' Custom ErosionTransectMangrove
#' @keywords internal
ErosionTransectMangrove <- function(wave_transect = wave_transect,
                                    Ho = Ho,
                                    To = To,
                                    total_wsl_adj = total_wsl_adj,
                                    m = m,
                                    me = me,
                                    Cm = Cm,
                                    storm_duration = storm_duration) {
  # ================================================================
  # Mangrove Mud Erosion function

  dsub <- wave_transect

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
  MErodeLen1 <- len(ErodeLoc) * dx

  # Erosion volume
  MErodeVol1 <-
    pracma::trapz(x = abs(dsub$Xpos[ErodeLoc]), y = Retreat1[ErodeLoc] / 100.0) * storm_duration
  # Volume of mud eroded shoreward of the shoreline (m^3/m)


  # ---------------------------------------------------------
  # Erosion (with vegetation)
  ErodeLoc <- which(Trms2 > ero2$Te[1])

  # Indices where erosion rate greater than Threshold
  # Zero must be the location of the water at lower low water
  Zero <- max(dsub$Xpos) # 507

  ErodeLoc <- ErodeLoc[ErodeLoc >= Zero]
  dx <- 1
  # Erosion rate greater than Threshold at each location
  # shoreward of the shoreline (pre managament)

  # Erosion Length m (with vegetation):
  MErodeLen2 = len(ErodeLoc) * dx

  # Erosion volume
  MErodeVol2 <-
    pracma::trapz(x = abs(dsub$Xpos[ErodeLoc]), y = Retreat2[ErodeLoc] / 100.0) * storm_duration
  # Volume of mud eroded shoreward of the shoreline (m^3/m)




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
  coef0 = max(Etasimple1[2:(length(Etasimple1) - 1)], na.rm = TRUE) / Emax

  # MJB prevent extreme values
  m_median <- head(sort(Setup, decreasing = TRUE), 5)
  m_median <- median(m_median, na.rm = TRUE)
  m_max <- max(Setup, na.rm = TRUE)

  if(m_max > (m_median * 5)) {
    numerator <- m_median
  } else {
    numerator <- m_max
  }

  # corrected MWL at shoreline in presence of habitat
  Etap = numerator / coef0


  # MJB Fix in ESRI
  if (is.na(Etap)) {
    Etap = 0
  }
  # if Eta with vegetation is negative, take as zero
  if (Etap < 0) {
    Etap = 0
  }

  # Hprime to estimate run-up with vegetation
  Hp = (Etap / (0.35 * m)) ** 2 / Lo

  # run-up with vegetation
  Rnpveg1 = 1.1 * (Etap + math.sqrt(Lo * (Hp * 0.563 * m ** 2 + Ho * 0.004)) / 2.0)

  # run-up without vegetation
  # Rnp1

  # run-up with vegetation
  # Rnpveg1

  # Return output object
  ret_obj <- data.frame(
    retreat_NoVeg = MErodeLen1,
    retreat_Veg = MErodeLen2,
    runup_NoVeg = Rnp1,
    runup_Veg = Rnpveg1,
    erosion_m3_NoVeg = MErodeVol1,
    erosion_m3_Veg = MErodeVol2
  )

  return(ret_obj)

}
