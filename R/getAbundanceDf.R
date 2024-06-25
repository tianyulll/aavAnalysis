#' Summarize Clonal Abundance
#'
#' @description
#' Summarize Clonal Abundance per sample. Finds the Top n largest clones.
#'
#' @param df df after \code{\link{getProcessedDf}}
#' @param topn how many top largest clone should be return. Default at 10.
#' Choosing values other than 10 will break the plotting function.
#' Will be fixed in future version.
#'
#' @return dataframe
#' @export getAbundanceDf
#' @import dplyr
#'
getAbundanceDf <- function(df, topn = 10) {

  df <- df %>%
    dplyr::select(sample, sonicLengths, nearestGene, posid, info) %>%
    dplyr::mutate(nearestGene = base::ifelse(nchar(nearestGene) > 10, base::substr(nearestGene, 1, 10), nearestGene)) %>%
    dplyr::mutate(abundantCloneName = paste0(posid, "\n", nearestGene, ":", sonicLengths)) %>%
    dplyr::select(-nearestGene, -posid) %>%
    dplyr::group_by(sample) %>%
    dplyr::mutate(totalClone = sum(sonicLengths)) %>%
    dplyr::slice_max(order_by = sonicLengths, n = topn, with_ties = F) %>%
    dplyr::mutate(sonicPercent = sonicLengths / totalClone) %>%
    dplyr::mutate(fileName = sample) %>%
    dplyr::mutate(sample = paste(sample, info, sep = "\n")) %>%
    dplyr::select(-info)

  return(df)
}

