#' @title A function utilizing central limit theorem to produce a histogram
#' for a uniform distribution.
#'
#' @param n the sample size
#' @param iter the number of iterations
#' @param a the lowest value of x (for a Uniform)
#' @param b the highest value of x(for a Uniform)
#'
#' @return a histogram of the distribution of sums
#' @export
#'
#' @examples
#' \dontrun{myclt(n=20, iter=10000, a=3, b=6)}
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
w=myclt(n=50,iter=10000,a=5,b=10)
