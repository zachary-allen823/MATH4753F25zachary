#' myncurve
#'
#' @param mu a number
#' @param sigma a number
#' @param a a number less than mu +/- 3*sigma
#'
#' @returns a normal curve plot with shaded area -infinity to a
#' @export
#'
#' @examples
#' myncurve(mu=5, sigma=2, a=3)
myncurve = function(mu, sigma, a){
  xmin = mu-3*sigma
  xmax = mu + 3*sigma
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(xmin, xmax))
  xcurve = seq(xmin, a)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(xmin, xcurve, a), c(0, ycurve, 0), col="Black")
  prob = round(pnorm(a, mu, sigma), 4)

  list(mu = mu, sigma = sigma, prob = prob)
}
