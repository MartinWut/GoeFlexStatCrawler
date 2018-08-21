#' faculty_down
#'
#' @description Downloads the data for all modules of a faculty. The data can than be used for further analysis.
#' @usage faculty_down(facultyNr)
#' @param facultyNr A numeric value corresponding to a certain faculty. See the function faculty_data to get all facultynumbers.
#' @return
#' @examples
#' @export

# Help-function, needed for the faculty_down_function
single_request <- function(Semester, Fakultät, Modul){

  semester_all <- semester_data("all")
  faculty_all <- faculty_data("all")

  resultsURL <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"
  requestJSON <- requestJSON_file

  records <- data.frame(matrix(nrow = 0, ncol = 21))

  facultyString <- paste0('"lastValue":"',subset(faculty_all[,2], faculty_all[,1] == Fakultät | faculty_all[,2] == Fakultät) , '"') ### Hier den Wert der Fakultät angeben
  thisRequestJSON <- sub('"lastValue":"12"', facultyString, requestJSON)

  moduleString <- paste0('"lastValue":"', Modul, '"') ### Hier den Wert des Modules angeben
  thisRequestJSON <- sub('"lastValue":"112"', moduleString, requestJSON)

  semesterString <- paste0('"lastValue":"', subset(semester_all[,2], semester_all[,1] == Semester), '"') ### Hier den Wert des Semesters angeben
  thisRequestJSON <- sub('"lastValue":"60"', semesterString, thisRequestJSON)

  bodyList <- list(data = thisRequestJSON)
  request <- POST(resultsURL, body = bodyList, encode = "form")
  stop_for_status(request)

  responseJSON <- content(request, encoding = "UTF-8", type = "text")
  responseDataFrame <- fromJSON(responseJSON)$data$records
  results <<- data.frame(matrix(nrow = max(length(Semester),length(Fakultät), length(Modul)), ncol = 21))
  results <- responseDataFrame
  return(results)
}

faculty_down <- function(facultyNr){

  # download the necessary data

  module_all <- as.numeric(list_modules(facultyNr)$value)

  # define the output

  fac_mod_list <- list()

  # download the data for one module and (looped)
  for (moduleNr in 1:length(module_all)) {
    res <- data.frame(matrix( ncol = 21))

    if (class(single_request("all", facultyNr, module_all[moduleNr])) == "data.frame") {
      res <- single_request("all", facultyNr, module_all[moduleNr])

    }
    Spaltennamen <- c("2_3" ,"Studienmodul" ,"Nicht bestanden", "Ohne Note", "Notenschnitt (nur Bestanden)", "1_0" ,"1_7", "2_7", "3_7" ,"5_0" ,"4_0", "Klausurtermin", "3_0", "Bestanden", "Prüfer" ,"2_0" ,"Semester","Notenschnitt" ,"3_3", "Anzahl" ,"1_3" )
    colnames(res) <- Spaltennamen
    res <- res[,c(17,12,2,15,20,14,3,18,5,4,6,21,7,16,1,8,13,19,9,11,10)]
    res <- na.omit(res) # Falls NA's entfernt werden sollen -> erzeugt einen Leeren Data.Frame (Falls lediglich NA's für ein Modul vorhanden sind)
    fac_mod_list[[moduleNr]] <- res
  }

  # Remove entries with an empty data.frame
  index_df <- which(sapply(fac_mod_list, nrow) == 0)
  tmp <- fac_mod_list[-index_df]
  result_list <- tmp

  # return the result
  return(result_list)
}
