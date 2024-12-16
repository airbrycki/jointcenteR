#' Load JCHS multiyear ACS file
#'
#' Reads JCHS multiyear ACS file into global environment
#'
#' @param yr The year to extract.
#'
#' @param multdf The name of the dataframe to split. If using load_mult first,
#'        the name should be multiyear.
#'
#' @return Dataframes loaded into global environment named with convention acs2001.
#'
#' @note Can extract multiple years at once using purrr::map (see examples).
#'       Recommend running rm(multiyear) once years of interest are extracted.
#'
#' @examples
#' acspath <- "C:/Data/ACS/"
#' multfilename<- "ACS_multiyear_hhplus_RentersPlusVacant.csv"
#'
#' # split out multiple years
#' load_mult()
#' purrr::map(c(2001:2005), split_mult)
#'
#' split_mult(2006)
#'
#' @export
split_mult <- function(yr, multdf = multiyear) {
    df <- multiyear |>
      dplyr::filter(year == yr)

    dfyr <- paste0("acs", yr)
    assign(dfyr, df, envir = .GlobalEnv)

    rm(df)
}
