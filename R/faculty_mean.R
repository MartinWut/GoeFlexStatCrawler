#' faculty_mean
#'
#' @description This is a function for computing the mean value of a faculty over all semesters.
#' @usage faculty_mean(faculty_nr, download, FacData)
#'
#'    ## Default method:
#'
#'    faculty_mean(faculty_nr, download=FALSE, FacData=NA)
#' @param faculty_nr A numeric value corresponding to a certain faculty. See the function faculty_data to get all faculty numbers.
#' @param download Logical. If TRUE the corresponding data is downloaded and used for computing the faculty mean value. If False the faculty data has to be provided using the function faculty_down in the first place.
#' @param FacData A List containing the faculty data. Typically produced by the function faculty_down.
#' @return The Return is an object of class "fac_mean" and contains the mean value of the faculty and the faculty-name.
#' @examples # Computing the mean for the economic faculty without providing any data in advance
#' faculty_mean(12, download = TRUE)
#'
#' # Downloading the data for economic faculty using the faculty_down-function
#' economic_data <- faculty_down(12)
#' faculty_mean(12, download = FALSE, FacData = economic_data)
#' @export

faculty_mean <- function(faculty_nr, download=FALSE, FacData=NA){ # download= FALSE bedeutet, dass die Daten vorab geladen wurden. Die Liste mit den Faculty-Daten muss dann unter FacData angegeben werden

  # load the needed data for result-format
  faculty_vec <- faculty_data("all")
  faculty_vec <- faculty_vec[order(faculty_vec$value),]

  # create the data depending on the parameters
  if (download==FALSE && is.na(FacData)) {
    stop("Wrong data-type. A list with the faculty data is required. Either set download to FALSE or provide the faculty data if download is set to TRUE")
  }else{
    if (download==TRUE) {
      tmp1 <- faculty_down(faculty_nr)
    }else{ # Means download = FALSE and data provided
      tmp1 <- FacData
    }
  }

  # exclude the observations without a mean grade for an exam (this is seen by "-" or "")
  tmp2 <- tmp1
  for (j in 1:length(tmp1)) {
    tmp2[[j]] <- subset(tmp2[[j]], tmp2[[j]][8] != "-" & tmp2[[j]][,8] != "" )
  }

  # in some cases this will create a new list element with NA's because there is sometime just one observation for a certain module
  index_df <- which(sapply(tmp2, nrow) == 0)
  tmp2 <- tmp2[-index_df]

  # now the data is "clean". Compute the mean for every module the whole mean
  tmp3 <- NA
  for (j in 1:length(tmp2)) {
    tmp3[j] <- mean(as.numeric(tmp2[[j]][,8]))
  }
  mean_tmp <- mean(tmp3)
  faculty_tmp <- faculty_vec[faculty_nr,1]
  result <- list(Mean = mean_tmp,Faculty = faculty_tmp )

  # define a class object (S3)
  #class(result) <- append(class(result), "faculty_mean")
  attr(result, "class") <- "fac_mean"

  # return the result
  result
}
