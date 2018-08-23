#' module_data
#'
#' @description The function downloads the data for a specific module given the semester-, faculty- and module-number.
#' @usage module_data(semester_nr, faculty_nr, module_nr)
#' @param semester_nr A numeric value corresponding to a certain semester. See the function semester_data to get all semester-numbers.
#' @param faculty_nr A numeric value corresponding to a certain faculty. See the function faculty_data to get all faculty-numbers.
#' @param module_nr A numeric value corresponding to a certain module. See the function list_modules to get all modulen-umbers.
#' @return The result is a data.frame containing the data for a module for one semester. Depending on the module, it is possible that there are several rows, containing the data for several exam dates.
#' @examples  # Download the data for the introductory course in statistics at the economic faculty for the summer semester 2017.
#' # First get the information for specific semester-, faculty- and module-number
#'
#' semester_data() # the required semester-number is 65
#' faculty_data() # the required faculty-number is 12
#' list_modules(12) # the required module-number is 109
#' module_data(65,12,109)
#' @export

module_data <- function(semester_nr, faculty_nr, module_nr){


  # create error messages for wrong data input
  # check semester value
  if (any(grepl(semester_nr, semester_data("all")$value)) == FALSE){
    stop("The chosen semester value was not entered in the correct form or does not exist.")
  }

  # check faculty value
  if (any(grepl(faculty_nr, faculty_data("all")$value)) == FALSE){
    stop("The chosen faculty value was not entered in the correct form or does not exist.")
  }

  # check module value
  module_list <- list_modules(faculty_nr)
  if (any(grepl(module_nr, module_list$value)) == FALSE){
    stop("The chosen module value was not entered in the correct form or does not exist for the chosen faculty.")
  }
  results_file <- requestJSON_file

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

  # Rearrange the rows (only, if the resulting data.frame isn't empty)
  if(length(responseDF) != 0 ){
    responseDF <- responseDF[,c(17,12,2,15,20,14,3,18,5,4,6,21,7,16,1,8,13,19,9,11,10)]
  }

  return(responseDF)
}
