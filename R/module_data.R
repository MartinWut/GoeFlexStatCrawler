#' module_data
#'
#' @description The function downloads the data for a specific module given the semester-, faculty- and modulenumber.
#' @param semester_nr A numeric value corresponding to a certain semester. See the function semester_data to get all semesternumbers.
#' @param faculty_nr A numeric value corresponding to a certain faculty. See the function faculty_data to get all facultynumbers.
#' @param module_nr A numeric value corresponding to a certain module. See the function list_modules to get all modulenumbers.
#' @export

module_data <- function(semester_nr, faculty_nr, module_nr){
  results_file <- readChar("json/request.json", file.info("json/request.json")$size)

  semester_list <- paste('"lastValue":"', semester_nr, '"')
  results_list <- sub('"lastValue":"60"',semester_list, results_file)

  faculty_list <- paste('"lastValue":"', faculty_nr, '"')
  results_list <- sub('"lastValue":"12"',faculty_list, results_list)

  module_list <- paste('"lastValue":"', module_nr, '"')
  results_list <- sub('"lastValue":"112"',module_list, results_list)
  bodyList <- list(data = results_list)
  records <- data.frame(matrix(nrow = 0, ncol = 21))
  flex_url <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"
  response <- POST(flex_url, body = bodyList, encode = "form")
  stop_for_status(response)
  responseJSON <- content(response, encoding = "UTF-8", type = "text")
  responseDF <- fromJSON(responseJSON)$data$records

  # ordnen der Spalten (nur, wenn der resultierende Data.Frame nicht leer ist)
  if(length(responseDF) != 0 ){
    responseDF <- responseDF[,c(17,12,2,15,20,14,3,18,5,4,6,21,7,16,1,8,13,19,9,11,10)]
  }

  return(responseDF)
}
