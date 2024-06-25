#' Writes database connection
#'
#' @description
#' By default, writes aavenger read credentials to ~/.my.cnf
#' must give password.
#'
#' @param password to access the database
#' @export setDbConfig
#'
setDbConfig <- function(user = "aavenger", password, host = "174.129.238.44",
                        port = "3306", database = "AAVengeR", file_path = "~/.my.cnf") {

  if (is.null(password)) {
    stop("please provide password")
  }

  # Create the configuration content
  config_content <- paste0(
    "[AAVengeR]\n",
    "user=", user, "\n",
    "password=", password, "\n",
    "host=", host, "\n",
    "port=", "\n",
    "database=", database, "\n"
  )

  # Write the content to the specified file
  writeLines(config_content, con = file_path)

  # Set the file permissions to be readable and writable only by the user
  Sys.chmod(file_path, mode = "600")

  cat("Credentials have been written to", file_path, "\n")
}

