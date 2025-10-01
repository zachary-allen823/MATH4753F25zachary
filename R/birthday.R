#' Birthday Problem Probability
#'
#' @param x a quantitative vector
#'
#' @returns The probability that at least two people have the same birthday within a sample of x people
#' @export
#'
#' @examples
#' birthday(20:24)
#'
birthday <- function(x) {
  1 - exp(lchoose(365, x) + lfactorial(x) - x*log(365))
}
