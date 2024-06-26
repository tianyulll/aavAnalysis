#' Plot Random Site in Gene and in TU
#'
#' @param df df after \code{\link{getGeneDf}}
#' @param random_exon random sites in Exon percentange
#' @param random_tu random sites in Transcription Unit Percentage
#'
#' @return ggplot2 object
#' @export plotRandomGene
#'
plotRandomGene <- function(df, random_exon = NULL, random_tu = NULL) {

  p <- ggplot(df, aes(y = description)) +
    geom_point(aes(x = `In Exon%`, shape = "exon"), alpha = 0.8) +
    geom_point(aes(x = `In Transcription Unit%`, shape = "gene"), alpha = 0.8, size = 2) +
    scale_shape_manual(values = c(16, 2), labels = c("exon", "TranscriptionUnit"), name = NULL) +
    labs(title = "", x = "") +
    scale_x_continuous(labels = scales::label_percent(accuracy = .1, scale = 1),
                       limits = c(0, 100)) +
    geom_vline(linetype = "dashed", aes(xintercept = random_exon, color = "random exon")) +
    geom_vline(linetype = "dotted", aes(xintercept = random_tu, color = "random TU")) +
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          axis.line = element_line(linewidth = 0.5),
          legend.position = "right")

  return(p)
}


