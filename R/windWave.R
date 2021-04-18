#' @title windWave
#'
#' @description Estimates the wave height and period for a given wind speed,
#' fetch distance and water depth.
#'
#' @param WindSpeed Wind speed in units of meters per second.
#' @param FetchDistance Fetch distance in units of meters.
#' @param WaterDepth Average offshore water depth in units of meters.
#'
#' @return An estimate of the offshore wave height (meters) and wave period
#' (seconds).
#' @examples
#' waveHeightPeriod <- windWave(WindSpeed = 20, FetchDistance = 5000,
#' WaterDepth = 50)
#' print(waveHeightPeriod)
#' @export

windWave <- function(WindSpeed = 20, FetchDistance = 5000, WaterDepth = 100){

  Us <- WindSpeed
  Ft <- FetchDistance
  depth <- WaterDepth
  U <- Us
  Ft <- Ft

  # calculates wind waves which are a function of Wind Speed (U),
  # Fetch length (F),
  # and local depth (d) using empirical equations

  g <- 9.81
  rho <- 1024
  ds <- (g * depth) / U**2.0
  Fs <- (g * Ft) / U**2.0
  A <- math.tanh(0.343 * ds ** 1.14)
  B <- math.tanh(4.14e-4 * Fs ** 0.79 / A)
  H <- 0.24 * U ** 2 / g * (A * B) ** 0.572  # wave height
  A <- math.tanh(0.1 * ds ** 2.01)
  B <- math.tanh(2.77e-7 * Fs ** 1.45 / A)

  Ho <- H
  To <- 7.69 * U / g * (A * B) ** 0.18  # wave period

  out <- c(round(Ho,3),
           round(To,2))
  names(out) <- c("Height", "Period")

  return(out)

}
