#' semester_data
#'
#' @description Description text here
#' @usage  Usage text here
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import tidyr
#' @param semester The expression for the semesterterm. The default value is all semesters
#' @return Data.Frame of semester numbers corresponding to the semesterterm
#' @export
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
