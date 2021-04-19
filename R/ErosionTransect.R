#' Custom ErosionTransect
#' @keywords internal
ErosionTransect <- function(
  wave_transect = NA,
  Ho=NA,
  To=NA,
  sediment_size_mm = NA,
  ssf_lookup = NA,
  total_wsl_adj = NA,
  m = 0.02,
  B = 3,
  D = 3,
  W = 20,
  storm_duration = NA
) {


  #======================================
  # Setup variables
  #======================================
  math.pi = pi

  g = 9.81
  Hshortwave = Ho
  #HshortwaveMA = Ho
  Values = 100
  Etasimple1 = wave_transect$Etas # Wave setup without vegetation - to modify runup
  Setup = wave_transect$Eta       # Wave setup with vegetation
  #print(summary(Setup))

  # Sediment scale factor
  mdiff = abs(sediment_size_mm - ssf_lookup$diam)
  A = ssf_lookup$ssf[which(mdiff == min(mdiff))]

  #======================================
  # Run Erosion Script
  #======================================

  # estimate runup amount
  Lo = g * To ** 2.0 / (2.0 * math.pi)
  # before management action
  # run-up - short and long waves (run-up w/o vegetation effect)
  Rnp1 = 1.1 * (0.35 * m * math.sqrt(Hshortwave * Lo) + math.sqrt(Lo * (
    Hshortwave * 0.563 * m**2.0 + Ho * 0.004)) / 2.0)



  # last portion of the profile
  #TrigRange = range(BegTrig[-1], EndTrig[-1] + 1);

  # last portion of the profile is not reef
  if(Values == 100) {



    Emax = 0.35 * m * math.sqrt(Hshortwave * Lo)  # setup at the beach

    coef0 = max(Etasimple1[2:(length(Etasimple1)-1)], na.rm=TRUE) / Emax    # correction factor for MWL

    Etap = max(Setup, na.rm=TRUE) / coef0         # corrected MWL at shoreline in presence of habitat

    # MJB Fix in ESRI
    if(is.na(Etap)){
      Etap = 0
    }


    if(Etap < 0){
      Etap = 0  # if Eta with vegetation is negative, take as zero
    }


    Hp = (Etap / (0.35 * m)) ** 2 / Lo  # Hprime to estimate run-up with vegetation



    # run-up with vegetation
    Rnpveg1 = 1.1 * (Etap + math.sqrt(Lo * (Hp * 0.563 * m ** 2 + Ho * 0.004)) / 2.0)

  }


  # estimate erosion
  #if sand == 1:

  #log.info("...estimating erosion amount for sandy beach")
  # check if foreshore slope adequate (use worst wave height)
  hb = (((Ho ** 2.) * g * To / (2 * math.pi)) / 2.) ** (2. / 5.) / (
    g ** (1. / 5.) * 0.73 ** (4. / 5.))  # breaking depth
  xb = (hb / A) ** 1.5  # surf zone width
  ErosionTerm = xb - hb / m  # erosion term in Kriebel and Dean; has to be > 0!

  if(ErosionTerm <= 0){
    mo = math.floor(xb / hb)
    print("Your foreshore slope is too flat or sediment size is too high.")
    print(paste0("We'll increase it to 1/", mo, " to estimate a minimum erosion value."))
    mo = 1/mo # MJB added fix Dec 2020
  } else {
    mo = m
  }

  # before management action
  inundationcount = 0  # tracks whether or not the profile was inundation pre MA to avoid redundant messages.
  count = inundationcount

  # erosion of beach - without vegetation
  #TWL = total_wsl_adj + Rnp1
  #print("TWL for erosion is Rnp1 + surge_elevation")
  TWL = Rnp1 + total_wsl_adj

  out <- ErosionKD(A, Ho, TWL, B, D, W, mo, count, To, storm_duration)
  R1 <- out$R0


  if(Rnpveg1 > Rnp1){
    Rnpveg1 = Rnp1
    R_rnp = R1
  } else {
    # scale beach retreat by runup change due to vegetation
    R_rnp = R1 * (Rnpveg1 / Rnp1)
  }


  DissBF = mean(wave_transect$Dis1/wave_transect$DisSimple1, na.rm=TRUE)
  if(DissBF != -1){ # wave model did not crash
    R_dissip = R1 * DissBF  # scale beach retreat by dissipation due to vegetation
  } else {
    R_dissip = R_rnp  # scale beach retreat by dissipation due to vegetation
  }



  temp1 = mean(c(R_rnp, R_dissip))  # take the mean of both approaches
  temp2 = mean(c(R_rnp))  # take the mean of both approaches


  #====================================================================
  # Calculate retreat with vegetation
  #TWLv = total_wsl_adj + Rnpveg1
  #print("TWL for erosion is Rnp1 + surge_elevation")
  TWLv = Rnpveg1 + total_wsl_adj

  outv <- ErosionKD(A, Ho, TWLv, B, D, W, mo, count, To, storm_duration)
  R1v <- outv$R0

  if(Rnpveg1 > Rnp1){
    Rnpveg1 = Rnp1
    R_rnpv = R1v
  } else {
    # scale beach retreat by runup change due to vegetation
    R_rnpv = R1v * (Rnpveg1 / Rnp1)
  }



  if(DissBF != -1) {
    R_dissipv = R1v * DissBF
  } else {
    R_dissipv = R_rnpv
  }


  temp1v = mean(c(R_rnpv, R_dissipv))  # take the mean of both approaches
  temp2v = mean(c(R_rnpv))             # take the mean of both approaches

  # MJB added - do not scale retreat value by veg presence
  # some transects will not have veg
  temp1 = R1   # new overwrite
  temp1v = R1v # new overwrite

  ret_obj <- data.frame(retreat_NoVeg = temp1)
  ret_obj$retreat_Veg <- temp1v

  ret_obj$runup_NoVeg <- Rnp1
  ret_obj$runup_Veg <- Rnpveg1


  # Fix issue with bad erosion estimate
  if((Rnpveg1 < Rnp1) & (temp1v > temp1)){
    print("Impossible... use veg estmiate")
    print(paste0("Run up No veg: ", round(Rnp1, 2)))
    print(paste0("Run up veg: ", round(Rnpveg1, 2)))
    print(paste0("Retreat No veg: ", round(temp1, 2)))
    print(paste0("Retreat veg: ", round(temp1v, 2)))
    print("Impossible... assigning retreat with veg to non veg value")
    ret_obj$retreat_Veg <- temp1 # Assign non-veg value as in runup above.
    print("#")
    print("##")
    print("####")
    print("#####")
    print("######")
    print("#####")
    print("####")
    print("###")
    print("##")
    print("#")
    print("#")
  }


  return(ret_obj)


}
