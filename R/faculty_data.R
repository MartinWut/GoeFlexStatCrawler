#' faculty_data
#'
#' @description This is a function to download and display the faculty-number corresponding to a certain faculty-name for the FlexStat-Platform at the  Goerg-August University.
#' @usage faculty_data(faculty_name)
#'
#'     ## Default method:
#'
#'     faculty_data(faculty_name = "all")
#' @param faculty_name A is an object of the class character. If faculty_name = "all" all facultynumbers are displayed. If a single faculty-name is entered, it has to be the same expression that is used on the FlexStat-Platform.
#' @details FlexStat stores every faculty under a certain value. These values have to be used for all function of the GoeFlexStatCrawler-Package.
#' Below are faculty-labels and the corresponding faculty-numbers
#'
#' Fakultät für Agrarwissenschaften                     11
#'
#' Fakultät für Biologie und Psychologie                9
#'
#' Fakultät für Chemie                                  7
#'
#' Fakultät für Forstwissenschaften und Waldökologie    10
#'
#' Fakultät für Geowissenschaften und Geographie        8
#'
#' Fakultät für Mathematik und Informatik               5
#'
#' Fakultät für Physik                                  6
#'
#' Gemeinsame und Zentrale Einrichtungen                17
#'
#' Juristische Fakultät                                 2
#'
#' Medizinische Fakultät                                3
#'
#' Philosophische Fakultät                              4
#'
#' Sozialwissenschaftliche Fakultät                     13
#'
#' Theologische Fakultät                                1
#'
#' Wirtschaftswissenschaftliche Fakultät                12
#' @seealso semester_data()
#' @examples faculty_data("all") # Downloading all faculty-numbers
#' faculty_data("Philosophische Fakultät") # Downloading a single faculty-number
#' @export

faculty_data <- function(faculty_name="all"){# input either "all" or a certain faculty (e.g. "Wirtschaftswissenschaftliche Fakultät ")
  faculty_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916300&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")
  faculty_df <- jsonlite::fromJSON(txt = content(faculty_get, as="text"))
  faculty_df$label <- str_trim(faculty_df$label, "right")   #remove whitespace at the end of some faculty names
  faculty_df$value <- as.numeric(faculty_df$value)
  if (faculty_name == "all"){
    return(faculty_df)
  } else{
    if (any(faculty_name == faculty_df$label)){
      return(faculty_df$value[faculty_df$label == x])
    } else{
      stop("Input not in the correct form.")
    }
  }
}
