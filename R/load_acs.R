#' Load annual ACS household plus files
#'
#' Reads annual ACS household plus files into global environment
#'
#' @param year The year of the data file to load in
#'
#' @param path A path to the location of ACS .csv files.
#'
#' @param type Type of file to load, either "csv", "dta", or "parquet"
#'
#' @return Dataframes loaded into global environment named with convention
#' acs2023.
#'
#' @note Annual ACS files should be in one location and saved as .csv, .dta, or .parquet files. Load
#' multiple years with purrr::map.
#'
#' @examples
#' acspath <- "C:/Data/ACS/"
#' # single year
#' load_acs(2022, path = acspath, type = "csv")
#'
#' # multiple years
#' purrr::map(c(2019, 2021:2023), load_acs, path = acspath, type = "csv")
#'
#' # multiple years, different file types
#' year_file <- data.frame(year = c(2001, 2019), type = c("parquet", "csv"))
#' purrr::map2(year_file$year, year_file$type, ~load_acs(.x, path = acspath, type = .y))
#'
#' @export
load_acs <- function(year, path = acspath, type = "csv") {
  # load acs years-----
  ## .csv files-----
  if (type == "csv") {
    ### create input file name-----
    # annual files
    if (year >= 2015) {
      inputfile <- paste0("ACS_", year, "_hhplus.csv")
      df <- data.table::fread(file.path(path, inputfile))

      if (year > 2022) {
        df <- df |> dplyr::mutate(st = state)
      }

      # multiyear file
    } else if (year >= 2001 & year < 2015) {
      inputfile <- "ACS_multiyear_full.csv"
      full_path <- file.path(path, inputfile)

      con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      query <- glue::glue_sql(
        "SELECT * FROM read_csv_auto({full_path}) WHERE year = {year}",
        .con = con
      )
      df <- data.table::data.table(DBI::dbGetQuery(con, query))
      DBI::dbDisconnect(con)
    } else {
      warning("year not supported")
      stop()
    }

    ## .dta files-----
  } else if (type == "dta") {
    message("this could take a while. you might want to convert your files.")
    ### create input file name-----
    # annual files
    if (year >= 2015) {
      inputfile <- paste0("ACS_", year, "_hhplus.dta")
      df <- haven::read_dta(file.path(path, inputfile))

      if (year > 2022) {
        df <- df |> dplyr::mutate(st = state)
      }

      # multiyear file
    } else if (year >= 2001 & year < 2015) {
      warning("this is a bad idea. convert your file to a parquet.")
      stop()
    } else {
      warning("year not supported")
      stop()
    }

    ## .parquet files
  } else if (type == "parquet") {
    if (year >= 2015) {
      inputfile <- paste0("ACS_", year, "_hhplus.parquet")
    } else if (year >= 2001 & year < 2015) {
      inputfile <- "ACS_multiyear_full.parquet"
    }

    full_path <- file.path(path, inputfile)

    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    query <- glue::glue_sql(
      "SELECT * 
        FROM {`full_path`} 
        WHERE year = {year}",
      .con = con
    )
    df <- data.table::data.table(DBI::dbGetQuery(con, query))
    duckdb::dbDisconnect(con)

    if (year > 2022) {
      df <- df |> dplyr::mutate(st = state)
    }

    ## any other file type
  } else {
    warning("file type not supported")
    stop()
  }

  dfyr <- paste0("acs", year)
  assign(dfyr, df, envir = .GlobalEnv)

  rm(df)
}
