#' @title submergedStructure
#'
#' @description Estimates the reduction in wave height as a wave passes over a
#' submerged structure.
#'
#' @param Hi Initial wave height (m) at the seaward margin of a submerged
#' structure.
#' @param To Initial wave period (s) at the seaward margin of a submerged
#' structure.
#' @param hi Total water depth (m) at the seaward margin of a submerged
#' structure.
#' @param hc Height of the submerged structure (m) above the surrounding seabed.
#' @param Cwidth Crest width (m) at the top of the submerged structure.
#' @param Bwidth Base width (m) at the bottom of the submerged structure.
#'
#' @return The final wave height after passing over a submerged structure
#' reported as a fraction (0 to 1) or the original wave height.
#' @examples
#' pfinal <- submergedStructure(Hi = 2.5, To = 7, hi = 6, hc = 3, Cwidth = 2,
#' Bwidth = 5)
#' 2.5*pfinal       # Wave height after passing over submerged structure.
#' 2.5-(2.5*pfinal) # Wave height reduction in meters.
#' (1-pfinal)*100   # Wave height reduction percentage.
#' @export

submergedStructure <- function(Hi = 2.5,
                         To = 7,
                         hi = 6,
                         hc = 3,
                         Cwidth = 5,
                         Bwidth = 10
                         ) {

  # Could be modified in the future.
  OysterReefType <- "Trapezoidal"
  math.pi <- pi

  Lo <- 9.81 * To ** 2.0 / (2.0 * math.pi)
  Rc <- hc - hi  # depth of submergence
  Boff <- (Bwidth - Cwidth) / 2.0  # base dif on each side
  ksi <- (hc / Boff) / math.sqrt(Hi / Lo)

  # The reef is too submerged
  if (hc / hi < 0.5) {
    Kt <- 1
    print("Your reef is too small and we cannot compute the wave transmission
          coefficient. Please increase your reef height so it's closer to the
          water surface, or assume that it does not protect your shoreline.")
    return(Kt)
  }

  if (hc / hi > 1.25) {
    Kt <- 1
    print("Your reef is too high above the water level and we cannot compute
          the wave transmission coefficient. Please assume that no waves pass
          through and the transmission coefficient is zero.")
    return(0)
  }

  if (abs(Rc / Hi) > 5) {
    Kt <- 1
    print("Your reef is too small and outside the range of validity of our
          model. Please increase your reef height so it's closer to the
          water surface.")
    return(Kt)
  }



  if (OysterReefType == "Trapezoidal") {
    # it's not a reef ball
    # van der Meer (2005)

    Kt1 <- -0.4 * Rc / Hi + 0.64 * (Cwidth / Hi) ** (-.31) * (
      1.0 - exp(-0.5 * ksi))  # transmission coeff: d'Angremond
    Kt1 <- max(Kt1, 0.075)
    Kt1 <- min(0.8, Kt1)

    Kt2 <- -0.35 * Rc / Hi + 0.51 * (Cwidth / Hi) ** (-.65) * (
      1.0 - exp(-0.41 * ksi))  # transmission coeff: van der Meer
    Kt2 <- max(Kt2, 0.05)
    Kt2 <- min(Kt2, -0.006 * Cwidth / Hi + 0.93)

    if (Cwidth / Hi < 8.0) {
      # d'Angremond
      Kt <- Kt1
    } else {
      if (Cwidth / Hi > 12.0) {
        # van der Meer
        Kt <- Kt2
      } else {
        # linear interp
        temp1 <- (Kt2 - Kt1) / 4.0
        temp2 <- Kt2 - temp1 * 12.0
        Kt <- temp1 * Cwidth / Hi + temp2
      }
    }
  }


  return(Kt)



}
