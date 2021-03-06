% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Display FARS data}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An Integer with the State Code}

\item{year}{Input a year \code{year} in form of character or integer}
}
\value{
This function does not return anything. It presents the data in graphical format
}
\description{
This function takes an input as \code{year} and \code{state.num} . It then display that information
in graphical format
}
\examples{
fars_map_state(49, 2013)

}
