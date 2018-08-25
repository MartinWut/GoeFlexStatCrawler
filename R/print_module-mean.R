#' module mean
#'
#' @param x module mean object
#' @export

# Define the depiction of the module_mean-function
print.module_mean <- function(x,...){
  cat("Mean = ", x$Mean,"\n")
  cat("Module = ", x$Module, "\n")
}
