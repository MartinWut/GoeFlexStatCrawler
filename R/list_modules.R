#' list_modules
#'
#' @description The function downloads the faculty-specific module-information
#' @param faculty A numeric value corresponding to a certain faculty. Use the function faculty_data() to display all faculty numbers.
#' @export

list_modules <- function(faculty){

  # Maipulate the FlexStat-URL

  module_url <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710933664&type=STUDIENMODUL&path=FAK%3D12&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"

  module_url_part1 <- substr(module_url, 1, 128)
  module_url_part2 <- substr(module_url, 131, nchar(module_url))

  module_url2 <- paste(module_url_part1, as.character(faculty), module_url_part2, sep = "")
  module_get <- GET(module_url2)
  module_df <- jsonlite::fromJSON(txt = content(module_get, as="text"))
  return(module_df)
}
