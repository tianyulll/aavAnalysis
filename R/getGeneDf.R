#' In gene and in TU Summary
#'
#' @param df df after \code{\link{getProcessedDf}}
#'
#' @return dataframe with summarized info about in_gene and in_transcription_unit columns.
#' @export getGeneDf
#'
getGeneDf <- function(df) {

  df <- df %>%
    select(sample, info, inGene, inExon) %>%
    group_by(sample, info) %>%
    mutate("In Exon%" = round(mean(inExon)*100, digits = 2),
           "In Transcription Unit%" = round(mean(inGene)*100, digits = 2)) %>%
    select(-inGene, -inExon) %>%
    unique() %>%
    arrange(sample)

  return(df)
}
