#' module_mean
#'
#' @description Function for computing the mean value of a specific module
#' @usage module_mean(semester_vector, faculty_nr, module_nr)
#'    ## Default method:
#'    module_mean(semester_vector = "all", faculty_nr= NA, module_nr=NA)
#' @param semester_vector A vector of numeric values. The default value takes all semesters.
#' @param faculty_nr A numeric value corresponding to a certain faculty. See the function faculty_data to get all facultynumbers.
#' @param module_nr A numeric value corresponding to a certain module. See the function list_modules to get all modulenumbers.
#' @return
#' @examples
#' @export

module_mean <- function(semester_vector = "all", faculty_nr= NA, module_nr=NA){

  # create additional data for semesternumbers and facultynumbers
  semester_all <- semester_data("all")[,2]
  faculty_all <- faculty_data("all")
  modules_faculty <- list_modules(faculty_nr)

  # First Case: wrong Input
  if (length(faculty_nr) != 1 || is.na(faculty_nr) == TRUE || semester_vector == "wrong" || is.na(module_nr) == TRUE) {
    stop("Wrong or missing input")
  }else{

    ## Second Case: all Semesters, one Module
    # load the data for every semester
    if (semester_vector == "all" && length(module_nr) == 1 && length(faculty_nr) == 1 ) {

      tmp1 <- lapply(semester_all, module_data, faculty = faculty_nr, module = module_nr)

      if (sum(lengths(tmp1)) == 0 ) {
        overall_mean <- 0
      }else{

        # clean the data -> remove empty semester entries
        index_df <- which(sapply(tmp1, length) == 0)
        if (length(index_df) > 0) {
          tmp1 <- tmp1[-index_df]
        }

        # remove entries with NA's as mean-value
        for (j in 1:length(tmp1)) {
          tmp1[[j]] <- subset(tmp1[[j]], tmp1[[j]][8] != "-" & tmp1[[j]][,8] != "" )
        }

        # in some cases this will create a new list element with NA's because there is sometime just one observation for a certain module
        index_df <- which(sapply(tmp1, nrow) == 0)
        if (length(index_df) > 0) {
          tmp1 <- tmp1[-index_df]
        }

        if (sum(lengths(tmp1)) == 0 ) {
          overall_mean <- 0
        }else{

          # compute the mean for the cleaned data
          mean_values <- sapply(tmp1,function(x){x[8]})
          mean_values <- unlist(mean_values)
          numeric_values <- as.numeric(mean_values)
          overall_mean <- mean(numeric_values)
        }
      }

    }else{

      # Third Case: Mean value for certain semesters ( more/equal than/to 1 but not all semesters)
      if (length(semester_vector) >= 1 && semester_vector != "all") {

        sem_tmp <- semester_vector

        tmp1 <- lapply(sem_tmp, module_data, faculty = faculty_nr, module = module_nr)

        # clean the data -> remove empty semester entries
        index_df <- which(sapply(tmp1, length) == 0)
        if (length(index_df) > 0) {
          tmp1 <- tmp1[-index_df]
        }

        # remove entries with NA's for mean-value
        for (j in 1:length(tmp1)) {
          tmp1[[j]] <- subset(tmp1[[j]], tmp1[[j]][8] != "-" & tmp1[[j]][,8] != "" )
        }

        # in some cases this will create a new list element with NA's because there is sometime just one observation for a certain module
        index_df <- which(sapply(tmp1, nrow) == 0)
        if (length(index_df) > 0) {
          tmp1 <- tmp1[-index_df]
        }

        # compute the mean for the cleaned data
        mean_values <- sapply(tmp1,function(x){x[8]})
        mean_values <- unlist(mean_values)
        numeric_values <- as.numeric(mean_values)
        overall_mean <- mean(numeric_values)
      }
    }
  }

  # define the result as the mean and the name of the module
  module_name <- subset(modules_faculty[,1], modules_faculty[,2] == module_nr)
  result <- list(Mean = overall_mean,Module = module_name )

  # define a class object (S3)
  #class(result) <- append(class(result), "faculty_mean")
  attr(result, "class") <- "module_mean"

  # return the result
  result

}
