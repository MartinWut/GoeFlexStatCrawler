#' module mean
#'
#' @param obj module mean object
#' @export

# Define the depiction of the module_mean2-function
print.module_mean <- function(obj){
  cat("Mean = ", obj$Mean,"\n")
  cat("Module = ", obj$Module, "\n")
}
