#' myclt
#'
#' @param n A numeric vector
#' @param iter A number
#'
#' @returns histogram of sample means as well as the vector of sample means
#' @export
#'
#' @examples myclt(n=20, iter=10000)
myclt=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sample_means = apply(data, 1, mean)
  hist(sample_means, probability = TRUE,
       main = paste("Histogram of Sample Means (n =", n, ")"),
       xlab = "Sample Means", col = "lightblue")
  return(sample_means)
}
