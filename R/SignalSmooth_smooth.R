#' Internal SignalSmooth_smooth
#' @param x a cross shore profile elevaion vector
#' @param window_len length of the smoothing window
#' @param mm method. currently only hanning available.
#' @keywords internal
SignalSmooth_smooth <- function(x, window_len, mm = 'hanning') {

  window_len <- floor(window_len)

  s <- c(2 * x[1] - rev(x[1:window_len]),
        x,
        2 * x[length(x)] - rev(x)[1:window_len - 1])

  w <- e1071::hanning.window(window_len)
  temp1 <- w/sum(w)
  y1 <- pracma::conv(temp1, s)
  # Match mode same
  cpos <- length(y1) - max(length(temp1), length(s))
  cpos <- cpos/2
  end_pt <- length(y1) - cpos
  sel <- seq(cpos + 1, end_pt)
  y <- y1[sel]

  sel <- seq(window_len+1, (length(y) - window_len + 1))
  yout <- y[sel]

  return(yout)

}
