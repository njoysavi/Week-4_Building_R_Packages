#  COURSERA Project: BUILDING R PACKAGES
#  File: fars_functions.R
#  email: njoysavi@gmail.com
#  Start: July 10 2018
#  End:   July 13 2018

#' Read file with FARS data
#'
#' This function takes in \strong{US National Highway Traffic Safety Administration's}
#' data from .csv file, stored on disk. This data contains a nationwide census, providing the
#' American public yearly data, regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @param filename name of the file to read. This contains the data
#' @return This returns a dataframe after reading the csv file. If no file is present then it
#' returns an error
#'
#' @import readr
#' @import dplyr
#'
#' @examples
#' fars_read('accident_2014.csv.bz2')
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Makes a filename
#'
#' This function makes a file in CSV format which is related to the given \code{year}
#'
#' @param year Input a year \code{year} in form of character or integer
#'
#' @return This function returns a file in CSV format which is related to the given \code{year}
#'
#' @examples
#' make_filename(2014)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS years
#'
#' This function takes an input as \code{years} and then filters the information for that given list
#'
#' @param years Input list of years \code{years}
#'
#' @import dplyr
#' @import magrittr
#'
#' @examples
#' fars_read_years(2014)
#'
#' @return This function returns a data frame which includes data for a given list of years \code{years}
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      l=system.file("extdata", package = "Week4BuildingRPackages")
      f=file
      file=paste(l,f, sep="/")
      fars_data <- fars_read(file)
      dplyr::mutate(fars_data, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize FARS data
#'
#' This function takes an input as \code{years} and then summarizes the information for that given list
#'
#' @param years Input list of years \code{years}
#'
#' @return This function returns a data frame which includes data for a given list of years \code{years}
#' It summarizes the information by counting number of accidents happening in a given month, year.
#'
#' @import dplyr
#' @import tidyr
#' @import magrittr
#'
#' @examples
#' fars_summarize_years(2013)
#'
#' @export
fars_summarize_years <- function(years) {
  fars_data <- fars_read_years(years)
  dplyr::bind_rows(fars_data) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = "n()") %>%
    tidyr::spread_("year", "n")
}

#' Display FARS data
#'
#' This function takes an input as \code{year} and \code{state.num} . It then display that information
#' in graphical format
#'
#' @param state.num An Integer with the State Code
#' @param year Input a year \code{year} in form of character or integer
#'
#' @return This function does not return anything. It presents the data in graphical format
#'
#' @import maps
#' @import dplyr
#' @import graphics
#'
#' @examples
#' fars_map_state(49, 2015)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  l=system.file("extdata", package = "Week4BuildingRPackages")
  f=filename
  file=paste(l,f, sep="/")
  data <- fars_read(file)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
