% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inflate_ls.R
\name{inflate_ls}
\alias{inflate_ls}
\title{Inflate with CPI-U Less Shelter}
\usage{
inflate_ls(initial_rent, reference_year, target_year)
}
\arguments{
\item{initial_rent}{A numeric value or vector to be inflated.}

\item{reference_year}{The year the values to be inflated are from.}

\item{target_year}{The year values should be inflated to.}
}
\value{
A numeric vector of inflated values.
}
\description{
Inflate rents from reference to target dollars using CPI-U Less Shelter, NSA.
}
\note{
CPI-U inflation rates are pulled from FRED using tidyquant. Monthly
values are averaged for the year.
}
\examples{
# inflate rent from 2000 dollars to 2023 dollars
inflate_ls(500, 2000, 2023)

data("acs22")
# inflate contract rents to target year dollars in new variable
acs22 |> mutate(rntp_infl_23 = inflate_ls(rntp, unique(year), 2023))

}
