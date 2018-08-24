#' print examiner compare
#'
#' @param obj eaxaminer_compare object
#' @export

# define the representation of the examiner_compare function
print.examiner_compare <- function(obj){
  cat("Examiner =", obj$Examiner_names, "\n")
  cat("Mean =", obj$Mean, "\n")
}
