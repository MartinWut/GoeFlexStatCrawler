#' print faculty mean
#'
#' @param x faculty mean object
#' @param ... ...
#' @export

print.fac_mean <- function(x,...){
  cat("Mean = ", x$Mean,"\n")
  cat("Faculty = ", x$Faculty, "\n")
}
