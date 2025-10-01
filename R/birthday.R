#' Birthday Problem Probability
#'
#' @param k a quantitative vector
#'
#' @returns The probability that at least two people have the same birthday within a sample of k people
#' @export
#'
#' @examples
#' birthday(20:24)
#'
birthday <- function(k) {
  1 - exp(lchoose(365, k) + lfactorial(k) - k*log(365))
}
