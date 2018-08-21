#' list_modules
#'
#' @description The function downloads the faculty-specific module-information
#' @param faculty A numeric value corresponding to a certain faculty. Use the function faculty_data() to display all faculty numbers.
#' @export

list_modules <- function(faculty){
  module_url2 <- paste(module_url_part1, as.character(faculty), module_url_part2, sep = "")
  module_get <- GET(module_url2)
  module_df <- jsonlite::fromJSON(txt = content(module_get, as="text"))
  return(module_df)
}
