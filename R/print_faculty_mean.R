#' print faculty mean
#'
#' @param x faculty mean object
#' @export

print.fac_mean <- function(x,...){
  cat("Mean = ", x$Mean,"\n")
  cat("Faculty = ", x$Faculty, "\n")
}
