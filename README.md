
# AAVengeR Analysis Toolkit

<!-- badges: start -->
<!-- badges: end -->

This toolkit can be used to generate a set of tables and plots for outputs from AAVengeR outputs.


## Installation

You can install the development version of aavAnalysis from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tianyulll/aavAnalysis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(aavAnalysis)

## get a list of existing runs
aavAnalysis::getAvailAAVengeR()

## pull AAVengeR result from database
df <- aavAnalysis::getAAVengerData("AAVHelaTopo")

## run a summary table
df.summary <- aavAnalysis::getSummary(df)
```

