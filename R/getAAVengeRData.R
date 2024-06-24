#' get available trials in aavenger database
#' @description
#' To use this function, the user must set up database credentials in ~/.my.cnf
#'
#' @return NULL. prints available trial names
#' @import RMariaDB
#' @export getAvailAAVengeR
#'
#' @examples getAvailAAVengeR()
getAvailAAVengeR <- function(){
  dbConn <- dbConnect(RMariaDB::MariaDB(), group = 'AAVengeR')
  o <- dbGetQuery(dbConn, paste0("SELECT DISTINCT trial FROM sites"))
  dbDisconnect(dbConn)
  print(o)
}

tmpFile <- function(){ paste0(paste0(stringi::stri_rand_strings(30, 1, '[A-Za-z0-9]'), collapse = ''), '.tmp') }

#' @param o
#' @param tmpDirPath
#'
#' @author Dr. John Everett
reconstructDBtable <- function(o, tmpDirPath){
  r <- tibble()

  if(nrow(o) > 0){
    r <- bind_rows(lapply(split(o, 1:nrow(o)), function(y){
      f <- tmpFile()
      writeBin(unserialize(y$data[[1]]), file.path(tmpDirPath, paste0(f, '.xz')))
      system(paste0('unxz ', file.path(tmpDirPath, paste0(f, '.xz'))))
      d <- readr::read_tsv(file.path(tmpDirPath, f))
      invisible(file.remove(file.path(tmpDirPath, f)))
      d
    }))
  }

  r
}

#' @param dbConn
#' @param trial
#' @param tmpDirPath
#'
#' @author Dr. John Everett
pullDBTrialSites <- function(dbConn, trial, tmpDirPath){
  o <- dbGetQuery(dbConn, paste0("select * from sites where trial = '", trial, "'"))
  reconstructDBtable(o, tmpDirPath)
}

#' Pull AAVengeR data from input or database
#'
#' @param file accepts multiple format. If in RDS/excel, will be read directly.
#' if in string, will try to pull from aavenger database.
#'
#' @return aavenger outputs in a dataframe.
#' @description
#' Get inputs from AAVengeR pipeline outputs.
#' It accepts input in rds or excel. It also accepts a string for trial name in aavenger database.
#' To use this function, the user must set up database credentials in ~/.my.cnf
#'
#' @import tools
#' @import RMariaDB
#' @export getAAVengerData
#'
#' @examples aavAnalysis::getAAVengerData("AAVHelaTopo")
#' getAAVengerData("/path/to/my/aavenger.rds")
#'
getAAVengerData <- function(file) {
  file_extension <- tools::file_ext(file)

  if (file_extension == "rds") {
    message("reading rds as df")
    data <- readRDS(file)
  } else if (file_extension %in% c("xls", "xlsx")) {
    message("reading excel as df")
    data <- read_excel(file)
  } else {
    message("name given - trying pulling from database")
    dbConn <- dbConnect(RMariaDB::MariaDB(), group = 'AAVengeR')
    data <- suppressMessages(pullDBTrialSites(dbConn, file, './'))
    dbDisconnect(dbConn)
  }

  if ( nrow(data) == 0 ) {
    stop("reading NOT successful")
  } else(
    message("Success! Number of rows read in:", nrow(data))
  )

  return(data)
}
