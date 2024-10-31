#' summarize AAVengeR outputs
#'
#' @param df df after \code{\link{getProcessedDf}}
#' @param meta path to meta data in rds. If not provided, will use subject column in df
#' @param minreads  exclude sites with reads small than minreads
#'
#' @return summarized dataframe of integration sites per sample
#' @import dplyr
#' @export getProcessedDf
#'
getProcessedDf <- function(df, meta = NULL, minreads = 0) {

  # reformat old data style
  if ("estAbund" %in% colnames(df)) {
    message("Detected old INSPIRE naming, converting to AAVengeR")
    df <- df %>%
      mutate(sonicLengths = estAbund, nearestGene = nearestFeature, sample = internalSampleID) %>%
      mutate(posid = paste(chromosome, strand, position, sep = ""))
  }

  if (is.null(meta)) {
    message("meta data not provided. using subject column")
    df$info <- df$subject
  } else {
    message("reading meta data from path...")
    meta <- readRDS(meta)
    df <- df %>% dplyr::left_join(meta, by = "sample")
  }

  df <- df %>%
    dplyr::filter(reads >= minreads)

  return(df)
}

#' Summarize aavenger result by sample
#'
#' @param df preprocessed dataframe
#'
#' @return dataframe summarized by sample
#' @importFrom vegan estimateR
#' @import dplyr
#' @export getSummary
#'
getSummary <- function(df) {

  o <- df %>%
    dplyr::select(subject, sample, info, sonicLengths, reads) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::group_by(sample, subject, info) %>%
    dplyr::summarize(ChaoLengths = list(sonicLengths),"Total Reads" = sum(reads),
                     "Unique Sites" = sum(count), "Inferred Cells" = sum(sonicLengths)) %>%
    dplyr::mutate("Chao1" = vegan::estimateR(unlist(ChaoLengths))["S.chao1"]) %>%
    dplyr::select(- ChaoLengths) %>%
    dplyr::rename(patientID = subject)

  return(o)
}
