#' Load annual ACS metro files
#'
#' Reads annual ACS metro files into global environment
#'
#' @param yr The year of the data file to load in
#'
#' @param path A path to the location of metro .csv files.
#'
#' @return Dataframes loaded into global environment named with convention met2023.
#'
#' @note Annual metro files should be in one location and saved as .csv files.
#'
#' @examples
#' acspath <- "C:/Data/ACS/"
#' load_met(2022, path = acspath)
#' 
#' @export
load_met <- function(yr, path = acspath) {
  #metro cbsas to merge on
  cbsapop <- tidycensus::get_acs(geography = "cbsa", 
                                 variables = "B01003_001",
                                 year=2022) |> 
    dplyr::select(-moe, -variable) |> 
    dplyr::rename(cbsapop_5yr = estimate) |> 
    dplyr::filter(!grepl(", PR", NAME)) |> 
    dplyr::mutate(cbsa20 = as.numeric(GEOID),
                  poprank_22_5yr = dplyr::min_rank(dplyr::desc(cbsapop_5yr))) |> 
    dplyr::rename(CBSA_name_5yr = NAME) |> 
    dplyr::select(-GEOID)
  
  inputfile_met <- paste0("ACS_", yr, "_hhplus_metro.csv")
  df <- data.table::fread(file.path(path, inputfile_met))
  
  if (yr == 2019) {
    df <- df |> dplyr::rename(cbsaname20 = CBSAName20)
  }
  
  df <- df |> 
    dplyr::left_join(cbsapop, by = "cbsa20") |> 
    dplyr::mutate(metro = dplyr::if_else(grepl("Metro", CBSA_name_5yr), 1, 0),
                 micro = dplyr::if_else(grepl("Micro", CBSA_name_5yr), 1, 0),
                 rural = dplyr::if_else(is.na(CBSA_name_5yr), 1, 0),
                 metsize_22_5yr = dplyr::case_when(metro==1 & cbsapop_5yr>=1000000 ~ "1 largestmet",
                                                    metro==1 & cbsapop_5yr>=250000 & cbsapop_5yr<1000000 ~ "2 midmet",
                                                    (metro==1 & cbsapop_5yr<250000) | micro==1 ~ "3 smallmet_micro",
                                                    rural==1 ~ "4 rural"),
                 metsize2_22_5yr = dplyr::case_when(metro==1 & poprank_22_5yr <= 100 ~ "1 top100",
                                                     metro==1  ~ "2 other metro",
                                                     micro==1 ~ "3 micro",
                                                     rural==1 ~ "4 rural"))
  
  dfyr <- paste0("met", yr)
  assign(dfyr, df, envir = .GlobalEnv)
  rm(df)
}
