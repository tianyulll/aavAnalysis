
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AAVengeR Analysis Toolkit

<!-- badges: start -->
<!-- badges: end -->

This toolkit can be used to generate a set of tables and plots for
outputs from AAVengeR outputs.

## Installation

You can install the development version of aavAnalysis with:

``` r
# install.packages("devtools")
devtools::install_github("tianyulll/aavAnalysis")
```

This package uses
[RMariaDB](https://github.com/r-dbi/RMariaDB?tab=readme-ov-file), which
requires MariaDB Connector/C and should be installed first

    sudo apt-get install -y libmariadb-dev

## Example

An example workflow:

``` r
library(aavAnalysis)

## one-time setup for database connection
aavAnalysis::setDbConfig(password = "iAmAavengeR1")

## get a list of existing runs
aavAnalysis::getAvailAAVengeR()

## pull AAVengeR result from database
df <- aavAnalysis::getAAVengerData("AAVHelaTopo")

## process output with filter or meta data
df <- aavAnalysis::getProcessedDf(df, meta = NULL, minreads = 0)

## run a summary table
df.summary <- aavAnalysis::getSummary(df)

## Clonal Abundnace Summary
df.abund <- aavAnalysis::getAbundanceDf(df)

## Get a list of abundance plots
plots <- aavAnalysis::plotListAbundance(df.abund)
```

## Contact
[Tianyu](mailto:tianyu.lu@pennmedicine.upenn.edu) 
