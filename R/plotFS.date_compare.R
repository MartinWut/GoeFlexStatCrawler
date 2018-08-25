#' plotFS.date_compare
#'
#' @param x An object of class date_compare
#' @export

plotFS.date_compare <- function(x){
  df <- data.frame("Date" = as.character(x[[1]]),"Mean" = x[[2]])
  ggplot(df, aes(x = Date, y = Mean, fill=Date))+
    geom_bar(stat = "identity")+
    xlab("Exam Date") + ylab("Mean Grade") +
    ylim(0,5) +
    labs(title="Comparison of Exam Dates",subtitle="Single Module") +
    guides(fill=guide_legend(title=NULL)) +
    theme( axis.text.x = element_blank())
}
