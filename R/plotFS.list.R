#' plotFS.list
#'
#' @param x An object of the class list
#' @export

plotFS.list <- function(x){

  if (class(x[[1]]) == "fac_mean") {

    faculty_means <- NA
    faculty_names <- NA
    for (i in 1:length(x)) {
      faculty_means[i] <- x[[i]]$Mean
      faculty_names[i] <- x[[i]]$Faculty
    }
    df <- data.frame(faculty_means, faculty_names)
    ggplot(df, aes(x = faculty_names, y = faculty_means, fill=faculty_names))+
      geom_bar(stat = "identity")+
      xlab("Faculty") + ylab("Mean grades") +
      coord_cartesian(ylim=c(min(df$faculty_means-0.5),max(df$faculty_means)+0.5)) +
      guides(fill=guide_legend(title=NULL)) +
      ggtitle("Comparison of faculty means") +
      theme( axis.text.x = element_blank())

  }else{
    if (class(x[[1]]) == "module_mean") {

      module_means <- NA
      module_names <- NA

      for (i in 1:length(x)) {
        module_means[i] <- x[[i]]$Mean
        module_names[i] <- x[[i]]$Module
      }

      df <- data.frame(module_means, module_names)

      if (length(df[,1]) > 10) {
        print("More than 10 modules selected. The 10 highest module-means will be displayed")
        df <- arrange(df, desc(module_means))
        df <- df[1:10,]
      }
      ggplot(df, aes(x = module_names, y = module_means, fill=module_names))+
        geom_bar(stat = "identity")+
        xlab("Module") + ylab("Mean grades") +
        coord_cartesian(ylim=c(min(df$module_means-0.5),max(df$module_means)+0.5)) +
        guides(fill=guide_legend(title=NULL)) +
        theme( axis.text.x = element_blank())
    }
  }
}
