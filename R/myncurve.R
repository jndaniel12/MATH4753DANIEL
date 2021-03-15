#' @title A function to display a normal curve.
#'
#' @param a The probability that X will be less than or equal to.
#' @param mu The mean.
#' @param sigma The standard deviation.
#'
#' @return Makes a curve, shades the area from negative infinity to a,
#' calculates the probability that X <= a, P(X <= a), prints the calculated
#' probability to the console.
#' @export
#'
#' @examples
#' \dontrun{myncurve(3, 10, 5)}
myncurve = function(a, mu, sigma){
  # Draw the density curve within 3 standard deviations of the mean
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  # Shade the area from the lower x-limit to a, and draw the y curve
  # along the x-curve, convert region to a polygon
  xcurve = seq(mu-3*sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col = "Red")

  # Calculate probability using p-stem and use as text on graph
  prob = pnorm(a,mu,sigma)
  prob = round(prob,4)
  text(mu, dnorm(a, mu, sigma)/2, paste0("Area = ", prob))

  # Print the probability to the console
  return(print(round(prob,4)))
}
