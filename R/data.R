#' random sample of 2022 American Community Survey PUMS
#'
#' Household data for a random sample of 300 observations taken from the 2022 American Community Survey Public Use Microdata 1-Year Estimates.
#'
#' @source U.S. Census Bureau, 2022 American Community Survey 1-Year PUMS.
#' @format A data frame with columns:
#' \describe{
#'  \item{serialno}{Unique household idnetifier}
#'  \item{region}{Region of the country the household lives in}
#'  \item{ten}{Household tenure}
#'  \item{dis}{Disability status of householder}
#'  \item{hincp}{Household income}
#'  \item{rntp}{Monthly contract rent}
#'  \item{wgtp}{Household weight}
#' }
#' @examples
#' data(acs22)
"acs22"