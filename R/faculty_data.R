#' faculty_data
#'
#' @description The function loads the faculty information from FlexStat-platform
#' @export

faculty_data <- function(x){# input either "all" or a certain faculty (e.g. "Wirtschaftswissenschaftliche Fakultät ")
  faculty_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916300&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")
  faculty_df <- jsonlite::fromJSON(txt = content(faculty_get, as="text"))
  faculty_df$label <- str_trim(faculty_df$label, "right")   #remove whitespace at the end of some faculty names
  faculty_df$value <- as.numeric(faculty_df$value)
  if (x == "all"){
    return(faculty_df)
  } else{
    if (any(x == faculty_df$label)){
      return(faculty_df$value[faculty_df$label == x])
    } else{
      stop("Input not in the correct form.")
    }
  }
}
