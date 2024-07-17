#' Plot Random Site in Gene and in TU
#'
#' @param df df after \code{\link{getGeneDf}}
#' @param exon_mean mean % of random site in exons
#' @param tu_mean mean % of random site in transcription unit
#' @param exon_sd std % of random site in exons
#' @param tu_sd std % of random site in transcription unit
#'
#' @return ggplot2 object
#' @export plotRandomGene
#'
plotRandomGene <- function(df, exon_mean = 0, exon_sd = 0, tu_mean = 0, tu_sd = 0) {

  df <- df %>%
    mutate(description = paste0(sample,"-", info))

  p <- ggplot(df, aes(y = description)) +
    geom_point(aes(x = `In Exon%`, shape = "exon"), alpha = 0.8) +
    geom_point(aes(x = `In Transcription Unit%`, shape = "gene"), alpha = 0.8, size = 2) +
    scale_shape_manual(values = c(16, 2), labels = c("exon", "TranscriptionUnit"), name = NULL) +
    labs(title = "", x = "") +
    scale_x_continuous(labels = scales::label_percent(accuracy = .1, scale = 1),
                       limits = c(0, 100)) +
    geom_vline(linetype = "dashed", aes(xintercept = exon_mean, color = "random exon")) +
    geom_rect(aes(xmin = exon_mean - exon_sd, xmax = exon_mean + exon_sd, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.01) +
    geom_vline(linetype = "dashed", aes(xintercept = tu_mean, color = "random TU")) +
    geom_rect(aes(xmin = tu_mean - tu_sd, xmax = tu_mean + tu_sd, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.01) +
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          axis.line = element_line(linewidth = 0.5),
          legend.position = "right")

  return(p)
}


