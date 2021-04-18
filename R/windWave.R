#' @title windWave
#'
#' @description Estimates the wave height and period for a given wind speed,
#' fetch distance and water depth.
#'
#' @param wind_speed Wind speed in units of meters per second.
#' @param fetch_distance Fetch distance in units of meters.
#' @param water_depth Average offshore water depth in units of meters.
#'
#' @return An estimate of the offshore wave height (meters) and wave period
#' (seconds).
#' @examples
#' waveHeightPeriod <- windWave(wind_speed = 20, fetch_distance = 5000,
#' water_depth = 50)
#' print(waveHeightPeriod)
#' @export

windWave <- function(wind_speed = 20,
                     fetch_distance = 5000, water_depth = 100) {

  Us <- wind_speed
  Ft <- fetch_distance
  depth <- water_depth
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

  out <- c(round(Ho, 3), round(To, 2))
  names(out) <- c("Height", "Period")

  return(out)

}
