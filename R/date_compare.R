#' date_compare
#'
#' @description This is a function for computing the mean values for each exam date per semester over all or some chosen semesters
#' @usage date_compare(faculty_nr, module_nr, semester_vector, download, FacData)
#'
#'   ## Default method:
#'
#'   date_compare(faculty_nr, module_nr, semester_vector="all", download=FALSE, FacData=NA)
#' @param faculty_nr A numeric value corresponding to a certain semester. See the function semester_data to get all semesternumbers.
#' @param modul_nr A numeric value corresponding to a certain module. See the function list_modules to get all modulenumbers
#' @param semester_vector A vector of numeric values containing the semester-numbers. By default all semesters are considered. Use the function semester_data to get the specific values.
#' @param download Logical. If TRUE the corresponding data is downloaded and used for computing the module mean values per semester. If False the faculty data has to be provided using the function faculty_down in the first place.
#' @param FacData FacData A List containing the faculty or module data. Typically produced by the function faculty_down.
#' @return The Return is an object of class "date_compare" and contains the information of the number exam date (Date 1, Date 2, etc.) of the chosen module and the mean values of the corresponding dates over the chosen semesters.
#' @examples # Compute the mean values for the different exam dates within a semester for the introductory course in statistics at the economic faculty over all semesters.
#'
#' faculty_data() # the required faculty-number is 12
#' list_modules(12) # the required module-number is 109
#'
#' # Computing the mean for the economic faculty without providing any data in advance
#' date_compare <- function(faculty_nr = 12, module_nr = 109, semester_vector = "all", download = TRUE)
#'
#' # Downloading the data for economic faculty using the faculty_down-function
#' economic_data <- faculty_down(12)
#' date_compare <- function(faculty_nr = 12, module_nr = 109, semester_vector = "all", download = FALSE, FacData = economic_data)
#' @export


date_compare <- function(faculty_nr, module_nr, semester_vector="all", download=FALSE, FacData=NA){
  # semster_vec has to be a character vector with semester entries in the form "WSYY" for winter semester
  # and "SoSeYY" for summer semester

  # create error messages for wrong data input (faculty_nr and module)
  # check faculty_nr
  if (any(grepl(faculty_nr, faculty_data("all")$value)) == FALSE){
    stop("The chosen faculty_nr is not in the correct form or does not exist.")
  }
  # check module
  module_list <- list_modules(faculty_nr)
  if (any(grepl(module_nr, module_list$value)) == FALSE){
    stop("The chosen module number does not exist for the chosen faculty.")
  }

  # create the data depending on the parameters
  if (download==FALSE && is.na(FacData)){
    stop("Wrong data type. A list with the faculty data is required. Either set download to TRUE or provide
         the faculty data, if download is set to FALSE")
  }else{
    if (download==TRUE){

      # replace semester names by semester values
      if (semester_vector == "all"){
        semester_list <- lapply(semester_vector, semester_data)
        semester_nr <- semester_list[[1]][,2]
      }else{
        sem_replace <- gsub("WS", "WS 20", semester_vector)
        sem_replace <- gsub("/", "/20", sem_replace)
        sem_replace <- gsub("SoSe", "SS 20", sem_replace)
        semester_nr <- unlist(lapply(sem_replace, semester_data))
      }

      # use module_data function to get the data for the chosen mosule and semesters
      FacData <- lapply(semester_nr, module_data, faculty = faculty_nr, module = module_nr)
    } #else: FacData = FacData if download = FALSE and data provided
  }

  # check semester_vector (create error messages for wrong data input)
  for (i in 1:length(semester_vector)) {
    if (semester_vector != "all" && any(grepl(semester_vector[i], FacData)) == FALSE){
      stop("One or more semester entries of the semester_vector were not entered in the correct form or
           are not available for the chosen module.")
    }
    }

  #replace module_nr by module name
  module_info <- module_list[grepl(module_nr, module_list$value) == TRUE, ]
  start_val <- regexpr(" ", module_info$label)[1] + 1 #get first letter of course name = first letter after first whitespace (after module label, e.g "M-WIWI...")
  stop_val <- nchar(module_info$label)
  module_name <- substr(module_info$label, start_val, stop_val)

  # extract semesters for all modul entries and reorder the resulting vector bottom-up
  # to have the semester entries for the first exam dates always listed first
  sem_info <- unlist(sapply(FacData, function(x)x[1][x[3] == module_name]))
  sem_info <- sem_info[length(sem_info):1]

  # extract exam dates for all modul entries and reorder the resulting vector bottom-up
  # to have the first exam dates always listed first
  date_info <- unlist(sapply(FacData, function(x)x[2][x[3] == module_name]))
  date_info <- date_info[length(date_info):1]

  # extract the grade means for all modul entries, reorder the resulting vector bottom-up
  # and replace the missing values by NAs
  mean_info  <- unlist(sapply(FacData, function(x)x[8][x[3] == module_name]))
  mean_info <- mean_info[length(mean_info):1]
  mean_info <- as.numeric(gsub("-", NA, mean_info))
  # mean_info <- as.numeric(gsub("", NA, mean_info))

  # save semester, date and mean information in a data frame
  info_df <- na.omit(data.frame(sem_info,date_info, mean_info))

  # create a count variable for grouping the grade means according to exam dates per semester
  # and append count variable to the data frame info_df
  sem_fac <- as.numeric(table(factor(info_df$sem_info, levels=unique(info_df$sem_info))))
  count_var <- unlist(lapply(sem_fac, seq))
  for (i in 1:length(count_var)) {
    count_var[i] <- paste("Date ", count_var[i])
  }
  info_df$count_var <- count_var

  # if not all semester shall be considered, create a subset of info_df for the corresponding semesters
  if (semester_vector != "all"){
    sub_df <- data.frame(sem_info=character(), date_info=character(), mean_info=numeric(), count_var=integer())
    # subset data frame
    for (i in 1:length(info_df$sem_info)) {
      for (j in 1:length(semester_vector)) {
        if (info_df$sem_info[i] == semester_vector[j]){
          sub_df <- rbind(sub_df, info_df[i,])
        }
      }
    }
    info_df <- sub_df
  }

  # convert exam date in info_df to date format
  info_df$date_info <- as.Date(info_df[,2], "%d.%m.%Y")

  # transform info_df to wide format to group exam dates by count variable
  info_df <- spread(info_df, count_var, mean_info) #transform to wide format
  res <- apply(info_df[,c(3:ncol(info_df))], MARGIN = 2, FUN = mean, na.rm = T)
  result_vector <- list(Exam_date = names(res), Mean = res)

  # define a class object (S3)
  attr(result_vector, "class") <- "date_compare"

  # return the result
  return(result_vector)
  }

# define the representation of the date_compare2 function
print.date_compare <- function(obj){
  cat("Exam date =", obj$Exam_date, "\n")
  cat("Mean =", obj$Mean, "\n")
}

