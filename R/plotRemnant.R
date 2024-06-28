#' Remnant plot for one sample
#'
#' @param x dataframe per sample
#' @param saveimg Default false. whether saving the image as PNG.
#' @param outDir path to save img.
#' @param buildAAVremnantPlots_ITRseqStart plot parameter: ITR start position. Default 57
#' @param buildAAVremnantPlots_ITRlength plot parameter: ITR length. Default 197
#' @param buildAAVremnantPlots_NTbinSize plot parameter: ITR bin size. Default 3
#'
#' @return ggplot2 object
#' @export plotSampleRemnant
#' @import ggplot2 RColorBrewer
#'
plotSampleRemnant <- function(x, saveimg = F, outDir = NULL,
                              buildAAVremnantPlots_ITRseqStart = 57, buildAAVremnantPlots_ITRlength = 197, buildAAVremnantPlots_NTbinSize = 3) {

  remnant_colors <- c('green4', 'green2', 'gold2', 'orange', 'orangered1', 'red4')


  range <- seq(0, buildAAVremnantPlots_ITRlength, buildAAVremnantPlots_NTbinSize)

  d <- tibble(remnantLen = as.integer(sub('\\.\\.', '', stringr::str_extract(x$repLeaderSeqMap, '\\.\\.\\d+'))) + buildAAVremnantPlots_ITRseqStart,
              bin = cut(remnantLen, breaks = c(-Inf, range, Inf), labels = FALSE) - (buildAAVremnantPlots_NTbinSize/2),
              r = stringr::str_count(x$repLeaderSeqMap, ';'),
              r2 = ifelse(r >= 5, '≥ 5', r)) %>% group_by(bin, r2) %>% summarise(n = n()) %>% ungroup() %>%
    mutate(r2 = factor(r2, levels = rev(c('0', '1', '2', '3', '4', expression(">= 5"))))) %>%
    droplevels() # drop unused factor levels

  range2 <- (range * buildAAVremnantPlots_NTbinSize) - buildAAVremnantPlots_NTbinSize

  p <- ggplot(d, aes(bin, n, fill = r2)) +
    theme_bw() +
    geom_col() +
    scale_fill_manual(name = 'Recombinations',
                      values = rev(remnant_colors[1:length(levels(d$r2))]),
                      drop = FALSE) +
    scale_x_continuous(breaks = range,
                       labels = range2,
                       limits = c(buildAAVremnantPlots_NTbinSize,
                                  cut(buildAAVremnantPlots_ITRlength, breaks = c(-Inf, range, Inf), labels = FALSE) - (buildAAVremnantPlots_NTbinSize/2))) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    #geom_vline(xintercept = cut(buildAAVremnantPlots_ITRdumbellTip1, breaks = c(-Inf, range, Inf), labels = FALSE), linetype = 'dashed') +
    #geom_vline(xintercept = cut(buildAAVremnantPlots_ITRdumbellTip2, breaks = c(-Inf, range, Inf), labels = FALSE), linetype = 'dashed') +
    ggtitle(paste0(x$sample[1], ' | ', x$info[1], '\n', formatC(n_distinct(x$posid), format="d", big.mark=","), ' sites')) +
    labs(x = 'ITR position', y = 'Integrations') +
    guides(fill=guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
    theme(text = element_text(size=16), plot.title = element_text(size = 14),
          legend.title = element_text(size=12),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"),
          legend.position="bottom", plot.margin=grid::unit(c(0.25, 0.25, 0.25, 0.25), "in")) +
    geom_point(data = tibble(x = cut(buildAAVremnantPlots_ITRseqStart, breaks = c(-Inf, range, Inf), labels = FALSE) - (buildAAVremnantPlots_NTbinSize/2)), aes(x, 0),
               size = 7, shape="\u27A1", inherit.aes = FALSE) +
    coord_cartesian(clip = "off")


  if (saveimg) {

    ggsave(file.path(outDir,"reportPlots/remnantPlots", paste0(x$trial[1], '-', x$subject[1], '-', x$sample[1], '.png')),
           p, dpi = 300, width = 10, height = 7, units = 'in', create.dir = T)

    }

  return(p)

}


#' Remnant Plot for all Samples
#'
#' @param df dataframe from \code{\link{getProcessedDf}}
#' @param saveimg Default false. whether saving the image as PNG.
#' @param outDir path to save img.
#' @param buildAAVremnantPlots_ITRseqStart plot parameter: ITR start position. Default 57
#' @param buildAAVremnantPlots_ITRlength plot parameter: ITR length. Default 197
#' @param buildAAVremnantPlots_NTbinSize plot parameter: ITR bin size. Default 3
#'
#' @return List of ggplot2 objects.
#' @export plotListRemnant
#'
plotListRemnant <- function(df, saveimg = F, outDir = NULL,
                            buildAAVremnantPlots_ITRseqStart = 57, buildAAVremnantPlots_ITRlength = 197, buildAAVremnantPlots_NTbinSize = 3){

  buildAAVremnantPlots_ITRdumbellTip1 <- 125
  buildAAVremnantPlots_ITRdumbellTip2 <- 147

  remnantPlots <- lapply(split(df, df$sample), function(sub_df) {
    plotSampleRemnant(x = sub_df, saveimg = saveimg, outDir = outDir,
                      buildAAVremnantPlots_ITRseqStart = buildAAVremnantPlots_ITRseqStart,
                      buildAAVremnantPlots_ITRlength = buildAAVremnantPlots_ITRlength,
                      buildAAVremnantPlots_NTbinSize = buildAAVremnantPlots_NTbinSize)
  })

  return(remnantPlots)

}
