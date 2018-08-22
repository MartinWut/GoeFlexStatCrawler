#' plotFS.module_mean
#'
#' @param x Object of the module_mean
#' @export

plotFS.module_mean <- function(x){
  df <- data.frame(x[[1]], x[[2]])
  ggplot(df, aes(x = x..1.., y = x..1.., fill=x..2..))+
    geom_bar(stat = "identity")+
    xlab("Module") + ylab("Mean grades") +
    coord_cartesian(ylim=c(min(df$x..1..-0.5),max(df$x..1..)+0.5)) +
    guides(fill=guide_legend(title=NULL)) +
    theme( axis.text.x = element_blank())
}
