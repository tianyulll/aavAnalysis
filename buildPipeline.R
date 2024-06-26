library(usethis)
library(devtools)

create_package("~/project/rproj/aavAnalysis/")

# usethis::use_github_action()
usethis::use_package("ggplot2")
usethis::use_package("RColorBrewer")
usethis::use_package("vegan")
usethis::use_package("RMariaDB")
usethis::use_package("readxl")
usethis::use_package("stringr")
usethis::use_package("tools")
usethis::use_package("dplyr")

usethis::use_r("getSummaryDf")
usethis::use_r("getAAVengeRData")
usethis::use_r("getRearrangement")
usethis::use_r("getAbundanceDf")
usethis::use_r("getRandomSiteDf")
usethis::use_r("getGeneDf")

usethis::use_r("plotAbundance")
usethis::use_r("plotRemnant")
usethis::use_r("plotRearrangement")
usethis::use_r("plotGene")

usethis::use_r("setDBConfig")

usethis::use_package_doc()
usethis::use_readme_rmd()
devtools::build_readme()

usethis::use_mit_license(copyright_holder = "Bushman Lab")

# library(tidyr)
# library(argparse)
# library(rtracklayer)
