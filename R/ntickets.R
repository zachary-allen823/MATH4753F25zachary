#' N tickets
#'
#' @param N numeric number of seats
#' @param gamma probability of overbooking
#' @param p probability of a ticket holder attending
#'
#' @returns discrete and continuous n value and corresponding graphs
#' @import stats
#' @export
#'
#' @examples ntickets(N=400, gamma=0.02, p=0.95)
ntickets <- function(N, gamma, p) {
  # Discrete n value
  discrete <- function() {
    nd <- N + 1
    while((1 - pbinom(N, nd, p)) <= gamma) {
      nd <- nd + 1
    }
    nd - 1
  }
  nd <- discrete()

  # Continuous n value
  nc <- function() {
    nc <- N + 1
    while((1 - pnorm(N + 0.5, mean = nc*p, sd = sqrt(nc*p*(1-p)))) <= gamma) {
      nc <- nc + 1
    }
    nc - 1
  }
  nc <- nc()

  # print list
  mylist <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(mylist)

  # discrete plot
  nvals <- seq(N, N + 30, by = 1)
  plot(nvals, (1 - gamma - pbinom(N, nvals, p)), type = "b", pch = 19,
       main = "Objective Function vs n",
       xlab = "n", ylab = "Objective")
  abline(h = 0, lty = 2, col = "red")

  # allows iteration of function for range on n values in graph
  continuous <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    1 - gamma - pnorm(N + 0.5, mean = mu, sd = sigma)
  }

  obj_vals <- sapply(nvals, continuous)

  # plot continuous curve

  plot(nvals, obj_vals, type = "l", lwd = 2, col = "blue",
       main = "Objective Function vs n",
       xlab = "n", ylab = "Objective")
  abline(h = 0, lty = 2, col = "red")
}
