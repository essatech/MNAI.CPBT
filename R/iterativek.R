#' Calculates the wave number
#' @param sigma a number, angular frequency
#' @param dh a number, water depth
#' @keywords internal
iterativek <- function(sigma, dh) {
  g <- 9.81
  kestimated <- (sigma ** 2) /
    (g * (math.sqrt(math.tanh((sigma ** 2) * dh / g))))
  # intial guess for the wave number
  tol <- 1
  # initialize the tolerence that will
  # determine if the iteration should continue
  count <- 0
  # iterate over the dispersion relationship
  while ((tol > 0.00005) & (count < 1000)) {
    count <- 1 + count
    kh <- kestimated * dh
    tol <- kestimated - (sigma ** 2) / (
      math.tanh(kh) * g)
    # check the difference between the previous and the current iterations
    kcalculated <- (sigma ** 2) / (math.tanh(kh) * g)
    # k values for current iteration
    kestimated <- kcalculated
    # set the current value for k as the previous value for
    # the subsequent iteration
  }
  qk <- kcalculated
  # return the value of k when accuracy tolerance is achieved
  return(qk)
}
