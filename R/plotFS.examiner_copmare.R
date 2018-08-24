#' plotFS.examiner_compare
#'
#' @param x object of class examiner_compare
#' @export

plotFS.fac_mean <- function(x){
  df <- data.frame(x[[1]], x[[2]])
  ggplot(test2, aes(x=x..1.., y=x..2..)) +
    geom_point(size=3) +
    geom_segment(aes(x=x..1..,
                     xend=x..1..,
                     y=0,
                     yend=x..2..)) +
    xlab("Examiners") + ylab("Mean grades") +
    labs(title="Examiner Mean grades",
         subtitle="Lollipop Chart") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
}
