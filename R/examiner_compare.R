#' examiner_compare
#'
#' @description This is a function for computing the mean values for the different examiners for one module over all or some chosen semesters.
#' @usage examiner_compare(faculty_nr, module_nr, semester_vector, download, FacData)
#'
#'   ##Default method
#'
#'   examiner_compare(faculty_nr, module_nr, semester_vector="all", download=FALSE, FacData=NA)
#' @param faculty_nr A numeric value corresponding to a certain faculty. See the function faculty_data to get all faculty numbers.
#' @param module_nr A numeric value corresponding to a certain module. See the function list_modules to get all module numbers.
#' @param semester_vector A vector of numeric values containing the semester-numbers. By default all semesters are considered. Use the function semester_data to get the specific values.
#' @param download Logical. If TRUE the corresponding data is downloaded and used for computing the examiner mean values. If False the faculty data has to be provided using the function faculty_down in the first place.
#' @param FacData FacData A List containing the faculty or module data. Typically produced by the function faculty_down.
#' @examples # Compute the mean values for the different examiners for the introductory
#' # course in statistics at the economic faculty over all semesters.
#'
#' faculty_data() # the required faculty-number is 12
#' list_modules(12) # the required module-number is 109
#'
#' # Computing the mean for the economic faculty without providing any data in advance
#' examiner_compare(faculty_nr = 12, module_nr = 109, download = TRUE)
#'
#' # Downloading the data for economic faculty using the faculty_down-function
#' economic_data <- faculty_down(12)
#' examiner_compare(faculty_nr = 12, module_nr = 109, download = FALSE, FacData = economic_data)
#' @export


examiner_compare <- function(faculty_nr, module_nr, semester_vector="all", download=FALSE, FacData=NA){

  # create error messages for wrong data input (faculty_nr and module_nr)
  # check faculty_nr
  if (any(grepl(faculty_nr, faculty_data("all")$value)) == FALSE){
    stop("The chosen faculty_nr is not in the correct form or does not exist.")
  }
  # check module_nr
  module_list <- list_modules(faculty_nr)
  if (any(grepl(module_nr, module_list$value)) == FALSE){
    stop("The chosen module_nr does not exist for the chosen faculty.")
  }

  # create the data depending on the parameters
  if (download==FALSE && is.na(FacData)){
    stop("Wrong data type. A list with the faculty data is required. Either set download to TRUE or provide
         the faculty data, if download is set to FALSE")
  }else{
    if (download==TRUE){

      # use module_data function to get the data for the chosen module and semesters
      FacData <- lapply(semester_vector, module_data, faculty = faculty_nr, module = module_nr)
    } #else: FacData = FacData if download = FALSE and data provided
  }

  ## find the correponding semester names for the semester values in the semester vector
  semester_all <- semester_data("all")
  semester_all <- semester_all[nrow(semester_all):1, ] # order semester_all with smallest semester value as the first and largest semester value as the last entry

  # create index variable to find the semester entries in semester all corresponding to the semester_vector values
  index_df <- c()
  for (i in 1:length(semester_vector)) {
    index_df[i] <- which(semester_all$value == semester_vector[i])
  }

  # extract the semester names from semester_all which correspond to the values in semester_vector and and change the names to the same form they have in the FacData variable ("WSYY/YY" for winter semester and "SoSeYY" for summer semester)
  semester_names <- semester_all$label[index_df]
  semester_names <- gsub("WS 20","WS", semester_names)
  semester_names <- gsub("/20", "/", semester_names)
  semester_names <- gsub("SS 20", "SoSe", semester_names)

  # check semester_vector (create error messages for wrong data input)
  for (i in 1:length(semester_names)) {
    if (semester_vector != "all" && any(grepl(semester_names[i], FacData)) == FALSE){
      stop("One or more semester entries of the semester_vector were not entered in the correct form or
           are not available for the chosen module.")
    }
    }

  #replace module_nr by module name
  module_list <- list_modules(faculty_nr)
  module_info <- module_list[grepl(module_nr, module_list$value) == TRUE, ]
  start_val <- regexpr(" ", module_info$label)[1] + 1 #get first letter of course name = first letter after first whitespace (after module label, e.g "M-WIWI...")
  stop_val <- nchar(module_info$label)
  module_name <- substr(module_info$label, start_val, stop_val)

  # extract semesters for all modul entries
  semester_entries <- unlist(sapply(FacData, function(x)x[1][x[3] == module_name]))

  # extract examiner names for all modul entries
  examiner_entries <- unlist(sapply(FacData, function(x)x[4][x[3] == module_name]))

  # extract the grade means for all modul entries and replace the missing values by NAs
  grade_entries <- unlist(sapply(FacData, function(x)x[8][x[3] == module_name]))
  grade_entries <- as.numeric(gsub("-", NA,  grade_entries))

  # save examiner names and the corresponding grade means in a data frame
  res_df <- na.omit(data.frame(semester_entries, examiner_entries, grade_entries))

  # if not all semester shall be considered, create a subset of info_df for the corresponding semesters
  if (semester_vector != "all"){
    sub_df <- data.frame(semester_entries=character(), examiner_entries=character(), grade_entries=numeric())
    # subset data frame
    for (i in 1:length(res_df$semester_entries)) {
      for (j in 1:length(semester_names)) {
        if (res_df$semester_entries[i] == semester_names[j]){
          sub_df <- rbind(sub_df, res_df[i,])
        }
      }
    }
    res_df <- sub_df
  }

  # compute grade means for all examiners and sort the entries according to the grade means in increasing order
  ex_comp <- sort(tapply(res_df$grade_entries, list(res_df$examiner_entries), mean))
  ex_comp <- list(Examiner_names = names(ex_comp), Mean = ex_comp)

  # define a class object (S3)
  attr(ex_comp, "class") <- "examiner_compare"

  # return the result
  return(ex_comp)
  }

