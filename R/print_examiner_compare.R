#' print examiner compare
#'
#' @param x eaxaminer_compare object
#' @param ... ...
#' @export

# define the representation of the examiner_compare function
print.examiner_compare <- function(x,...){
  cat("Examiner =", x$Examiner_names, "\n")
  cat("Mean =", x$Mean, "\n")
}
