% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_ahar.R
\name{process_ahar}
\alias{process_ahar}
\title{Clean AHAR data}
\usage{
process_ahar(year, file)
}
\arguments{
\item{year}{Year of data to pull, from 2007 to 2023.}

\item{file}{File path and name of saved .xlsx point-in-time count file.}
}
\value{
A dataframe of sheltered, unsheltered, and overall homelessness counts
by state and for the US for years selected.
}
\description{
Puts HUD AHAR data into a usable table format with sheltered, unsheltered,
and overall homelessness counts by state and for the US
}
\note{
HUD posts the data as an xlsb, which is very hard to work with. First
download the data from
https://www.huduser.gov/portal/sites/default/files/xls/
2007-2023-PIT-Counts-by-CoC.xlsb
and save locally as an xlsx. Use 2021 data with caution: unsheltered
and overall counts were not complete.
}
\examples{
file <- "C:/Data/2007-2023-PIT-Counts-by-State.xlsx"
# single year
process_ahar(2023, file)

# multiple years
yrs <- 2007:2023
ahar_st <- map(yrs, ahar_table, file = file) |>
  reduce(left_join, by = "State")

}
