#' @title Confidence Interval Function for Mu
#'
#' @param x the single sample input
#'
#' @return prints the c.i. of x
#' @export
#'
#' @examples
#' \dontrun{myci(x=rnorm(35,mean=7,sd=10))}
myci=function(x){
  n=length(x)
  alpha = 0.05
  t = qt(1 - alpha/2, n - 1)
  ci = c() # make a vector to hold upper and lower interval
  ci[1] = mean(x) - t*sd(x)/sqrt(n) # Lower goes in index 1
  ci[2] = mean(x) + t*sd(x)/sqrt(n) # Upper foes in index 2
  ci
}
