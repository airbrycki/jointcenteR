#' Load annual ACS household plus files
#'
#' Reads annual ACS household plus files into global environment
#'
#' @param year The year of the data file to load in
#'
#' @param path A path to the location of ACS .csv files.
#'
#' @return Dataframes loaded into global environment named with convention
#' acs2023.
#'
#' @note Annual ACS files should be in one location and saved as .csv files. Load
#' multiple years with purrr::map.
#'
#' @examples
#' acspath <- "C:/Data/ACS/"
#' # single year
#' load_acs(2022, path = acspath)
#'
#' # multiple years
#' purrr::map(c(2019, 2021:2023), load_acs, path = acspath)
#'
#' @export
load_acs <- function(year, path = acspath) {

  # use tidycensus to get state names
  fips_codes <- tidycensus::fips_codes |>
    dplyr::mutate(st = as.numeric(state_code),
           StateName = state_name) |>
    dplyr::select(st, StateName) |>
    dplyr::distinct() |>
    dplyr::filter(st <= 56)

  # load annual file - works for 2015-2021 (except 2020)
  inputfile <- paste0("ACS_", year, "_hhplus.csv")
    df <- data.table::fread(file.path(path, inputfile))
    if (year > 2018) {
      df <- df |>
        dplyr::mutate(agecat4 = factor(dplyr::case_when(agep < 25 ~ 1,
                                                       agep >= 25 & agep < 55 ~ 2,
                                                       agep >= 55 & agep < 65 ~ 3,
                                                       agep >= 65 ~ 4),
                                      levels = c(1:4),
                                      labels = c("Under 25", "25-54", "55-64",
                                                 "65+")))
    }
    if (year > 2022) {
      df <- df |>
        dplyr::mutate(st = state) |>
        dplyr::mutate(hh_inccat2 = factor(dplyr::case_when(hincp < 30000 ~ 1,
                                      hincp >= 30000 & hincp < 75000 ~ 2,
                                      hincp >= 75000 ~ 3),
                                      levels = c(1:3),
                                      labels = c("Under 30,000", "30,000-74,999",
                                                 "75,000+")))
    }
    df <- df |>
      dplyr::left_join(fips_codes, by="st")

    dfyr <- paste0("acs", year)
    assign(dfyr, df, envir = .GlobalEnv)

    rm(df)

}
