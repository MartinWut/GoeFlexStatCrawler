#' plotFS.fac_mean
#'
#' @param x Object of the fac_mean
#' @export

plotFS.fac_mean <- function(x){
  df <- data.frame(x[[1]], x[[2]])
  ggplot(df, aes(x = x..1.., y = x..1.., fill=x..2..))+
    geom_bar(stat = "identity")+
    xlab("Faculty") + ylab("Mean grades") +
    coord_cartesian(ylim=c(min(df$x..1..-0.5),max(df$x..1..)+0.5)) +
    guides(fill=guide_legend(title=NULL)) +
    ggtitle("Faculty Mean") +
    theme( axis.text.x = element_blank())
}
