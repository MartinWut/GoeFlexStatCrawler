#' print.date_compare
#'
#' @param x An Object of the class date_compare
#' @export

# define the representation of the date_compare function
print.date_compare <- function(x,...){
  cat("Exam date =", x$Exam_date, "\n")
  cat("Mean =", x$Mean, "\n")
}
