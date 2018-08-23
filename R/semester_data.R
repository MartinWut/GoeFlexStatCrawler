#' semester_data
#'
#' @description This is a function to download and display the semester-number corresponding to a certain semester for the FlexStat-Platform at the Goerg-August University in Goettingen.
#' @usage semester_data(semester)
#'
#'     ## Default method:
#'
#'     faculty_data(semester = "all")
#' @param semester  A is an object of the class character. If semester = "all" all semester-numbers are displayed. If a single semester is entered, it has to be the same expression that is used on the FlexStat-Platform.
#' @return Data.Frame of semester-numbers corresponding to the semesterterm
#' @details FlexStat stores every faculty under a certain value. These values have to be used for all function of the GoeFlexStatCrawler-Package.
#' Below are faculty-labels and the corresponding faculty-numbers
#' WS 2003/2004    38
#'
#' SS 2004         39
#'
#' ...
#'
#' SS 2019         69
#'
#' WS 2019/2020    70
#' @examples semester_data("all") # Downloading all semester-numbers
#' semester_data("SS 2004") # Downloading a single semester-number
#' @export
#'
semester_data <- function(semester="all"){ # input either "all" or a certain semester in form of "Semesterterm year" (e.g. "WS 2016/2017")

  # Downloading the necessary semester-information from the FlexStat-homepage
  semester_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916076&type=SEMESTERNR&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")
  semester_df <- jsonlite::fromJSON(txt = content(semester_get, as="text"))
  semester_df$value <- as.numeric(semester_df$value)
  head(semester_df)

  if (semester == "all") {
    return(semester_df)
  }else{
    if (any(semester == semester_df$label)) {
      return(semester_df$value[semester_df$label==semester])
    }else{
      stop("Input not in the correct form. Put in \"all\" or \n \"semesterterm year\" ")
    }
  }
}

NULL
