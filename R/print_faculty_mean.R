#' print faculty mean
#'
#' @param obj faculty mean object
#' @export

print.fac_mean <- function(obj){
  cat("Mean = ", obj$Mean,"\n")
  cat("Faculty = ", obj$Faculty, "\n")
}
