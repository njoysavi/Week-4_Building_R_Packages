## ------------------------------------------------------------------------
library(Week4BuildingRPackages)
library(maps)
library(tidyr)
library(magrittr)
library(dplyr)
library(readr)

## ------------------------------------------------------------------------
list.files(system.file("extdata", package = "Week4BuildingRPackages"))

## ------------------------------------------------------------------------
yr <- 2013
make_filename(yr)

## ------------------------------------------------------------------------
fars_read_years(2013)

## ------------------------------------------------------------------------
fars_summarize_years(2013)

## ------------------------------------------------------------------------
state_num <- 1
yr <- 2013
fars_map_state(state_num, yr)

