#' Plot Abundance of one sample
#'
#' @param df  One sample(GTSP) from the dataframe after \code{\link{getAbundanceDf}}
#' @param saveimg boolean. If true will create directory and save the image as PNG.
#' @param outputDir Output Directory if saving the image.
#'
#' @description
#'   First, Calculate the rest/unselected clonotypes as low abundance.
#'   Second, Sort the clones by its size.
#'   Finally, generate clonal abundance plots and save if flagged.
#'
#'
#' @return ggplot2 object of the abundance plot
#' @import ggplot2 RColorBrewer
#' @export plotSampleAbundance
#'
plotSampleAbundance <- function(df, saveimg = F, outputDir = NULL) {

  totalClone <- df$totalClone[[1]] # total clones in one sample
  fileName = df$fileName[1] # using GTSP-ID as file name

  # Unselected Clones are summed as Low Abundance in the plot
  dfRow <- dplyr::tibble(sample = df$sample[1], sonicLengths = df$totalClone[1] - sum(df$sonicLengths),
                  abundantCloneName = "Low abund",
                  sonicPercent = (df$totalClone[1] - sum(df$sonicLengths)) / df$totalClone[1])
  df <- dplyr::bind_rows(dfRow, df)

  # order by abundance
  df2 <- subset(df, abundantCloneName != "Low abund")
  df2 <- df2[order(df2$sonicPercent, decreasing = TRUE),]
  df$abundantCloneName <- factor(df$abundantCloneName,
                                 levels = c("Low abund", unique(df2$abundantCloneName)))

  p <- ggplot2::ggplot(df, aes(x = sample, y = sonicPercent, fill = abundantCloneName), ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = c('#eeeeee', brewer.pal(10, "Paired"))) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::annotate('text', x=1:length(totalClone), y=1.04, label=totalClone, size=2.7, hjust=0.5) +
    #geom_text(aes(label = sonicLengths), position = position_stack(vjust = 0.5),size = 3) +
    ggplot2::theme(panel.background = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.line.y = element_line(linewidth = 0.2),
                legend.text = element_text(size = 8),
                legend.title = element_blank())

  if (saveimg) {

    if (is.null(outputDir)) {
      imgFile = paste0(fileName, '.png')
    } else{
      imgFile = file.path(outputDir,"reportPlots/abundancePlots", paste0(fileName, '.png'))
    }

    ggplot2::ggsave(filename = imgFile, plot = p, dpi = 300, create.dir = T)
  }

  return(p)
}

#' abundance plot for all samples
#'
#' @description
#' Iterate through the abundnace dataframe by samples.
#' Creates abundance plot per sample
#'
#' @param df dataframe from \code{\link{getAbundanceDf}}
#' @param saveimg boolean. If true will create directory and save the image as PNG.
#' @param outputDir Output Directory if saving the image.
#'
#' @return List of ggplot2 objects
#' @export plotListAbundance
#'
plotListAbundance <- function(df, saveimg = F, outputDir = NULL) {

  abundancePlots <- lapply(split(df, df$sample), function(sub_df) {
    plotSampleAbundance(sub_df, saveimg = saveimg, outputDir = outputDir)
  })

  return(abundancePlots)
}

