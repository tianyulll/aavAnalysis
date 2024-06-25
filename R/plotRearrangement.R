#' Plot Rearrangment Percentage Summary
#'
#' @param df output df from  \code{\link{getRearrangeDf}}
#'
#' @return ggplot2 object
#' @import ggplot2 RColorBrewer
#' @export plotRearrangment
#'
plotRearrangment <- function(df) {

  p <- ggplot2::ggplot(df, aes(x = sample, y = `breakBool%/count`, fill = sample)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0,100)) +
    ggplot2::scale_fill_manual( values = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(unique(df$sample))) ) +
    ggplot2::labs( x = "", y = "") +
    ggplot2::ggtitle("Percentage of Boolean Breaks over Sites per Sample") +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.line = element_line(color = "black", linewidth = 0.2),
                    legend.position = "none",
                    plot.title = element_text(hjust = 0.5))

  return(p)
}
