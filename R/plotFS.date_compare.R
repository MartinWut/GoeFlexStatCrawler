#' plotFS.date_compare
#'
#' @param x An Object of the class date_compare
#' @export

# define the representation of the date_compare function
print.date_compare <- function(obj){
  cat("Exam date =", obj$Exam_date, "\n")
  cat("Mean =", obj$Mean, "\n")
}
