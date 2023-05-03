#' @title Mud Erosion
#'
#' @description Calculates erosion for mudflat transect.
#'
#' @param Uc Vector of zeros
#' @param Uw Bottom velocity vector (m/s)
#' @param h Depth vector (m)
#' @param To Wave period in seconds
#' @param me Erosion constant. Default value is 0.001m/s (kg/Ns)
#' @param Cm Dry density of bed. Default value is 70kg/m3
#'
#' @keywords internal
MudErosion <- function(
    Uc = NA,
    Uw = NA,
    h = NA,
    To = NA,
    me = 0.001,
    Cm = 70
) {

  # For testing
  # h = dsub$elev_smooth
  # Uw = dsub$Ubots
  # Uc = dsub$Ubots * 0
  # To = 3; me = 0.001; Cm = 70

  rho = 1024.0
  nu = 1.36e-6
  d50 = 0.00003
  ks = 2.5 * d50
  kap = 0.4

  # current
  if(max(Uc) != 0) {
    us1 = 0.01
    zo1 = 0.01
    dif = 10    # initial value for u* and zo
    Tc = h * 0  # shear stress
    while(dif > 1e-4) {
      zo2 = ks / 30 * (1 - exp(-us1 * ks / (27 * nu))) + nu / (9 * us1)
      us2 = kap * Uc / (log(h / zo2) - 1)
      dif1 = abs(us1 - us2)
      dif2 = abs(zo1 - zo2)
      dif = mean(dif1 + dif2)
      zo1 = zo2
      us1 = us2
    }
    Tc = rho * us1 ** 2  # shear stress due to current

  } else {
    Tc = h * 0
  }

  math.pi <- pi

  # waves
  Rw = Uw ** 2 * To / (2 * math.pi) / nu
  fw = 0.0521 * Rw ** (-0.187)  # smooth turbulent flow
  Tw = 0.5 * rho * fw * Uw ** 2

  # combined wave and current
  temp = Tc * (1 + 1.2 * (Tw / (Tc + Tw)) ** 3.2)
  Trms = (temp ** 2 + 0.5 * Tw ** 2) ** 0.5

  # erosion
  Te = h * 0 + 0.0012 * Cm ** 1.2  # erosion threshold
  dmdt = me * (Trms - Te)  # erosion rate
  dmdt[which(dmdt <= 0)] = 0
  Erosion = 3600 * dmdt / Cm * 100  # rate of bed erosion [cm/hr]

  # erosion: bed erosion rate,
  # Trms: wave and current shear stress,
  # Tc: current shear stress,
  # Tw: wave shear stress,
  # Te: erosion threshold
  ret_obj <- list(
       Erosion = Erosion,
       Trms = Trms,
       Tc = Tc,
       Tw = Tw,
       Te = Te)

  return(ret_obj)


}
