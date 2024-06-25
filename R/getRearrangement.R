#'
#' @param x column value in repLeaderSeqMap
#'
#' @return the length of the site. last value before"["
#' @import stringr
#' @keywords internal
#'
getRearrangeLength <- function(x) {
  if (is.na(x)) {
    return(0)
  }
  matches <- str_match_all(x, "\\.\\.(\\d+)\\[")
  values <- unlist(matches)
  last_value <- tail(values, n=1)
  return(as.numeric(last_value))
}

#' get rearrangement summary table
#'
#' @param df aavenger outputs after getProcessedDf()
#'
#' @return dataframe with ITR rearrangement summary
#' @import stringr
#' @export
#'
getRearrangeDf <- function(df) {

  o <- df %>%
    select(sample, repLeaderSeqMap) %>%
    mutate(breaks = str_count(repLeaderSeqMap, ";")) %>%
    mutate(breaks = ifelse(is.na(breaks), 0, breaks)) %>%
    mutate(boolBreak = ifelse(breaks == 0, 0, 1)) %>%
    mutate(count = 1) %>%
    mutate(rearrangeLength = sapply(repLeaderSeqMap, getRearrangeLength)) %>%
    group_by(sample) %>%
    summarise(totalBreaks = sum(breaks), totalSites = sum(count),
              totalLength = sum(rearrangeLength), totalBreakBool = sum(boolBreak)) %>%
    mutate("break\u2030/length" = round(totalBreaks / totalLength*1000, 2),
           "breakBool\u2030/length" = round(totalBreakBool / totalLength*1000, 2),
           "break%/count" = round(totalBreaks / totalSites * 100, 2),
           "breakBool%/count" = round(totalBreakBool / totalSites * 100, 2)) %>%
    select(-totalBreaks, -totalSites, -totalLength, -totalBreakBool)

  return(o)
}
