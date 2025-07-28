#' Get New Residential Construction data
#'
#' Pulls and organizes New Residential Construction data using tidyquant
#'
#' @param type Specifies the interval of interest, either "monthly" or "annual"
#'
#' @param fred_api_key Your unique FRED API key.
#'
#' @return A tibble of HVS variables for each quarter from 2Q2001 through most
#'         recent available.
#'
#' @note Adapted from Len Kiefer:
#'       http://lenkiefer.com/2017/09/18/a-tidyquant-um-of-solace/ and uses
#'       fredr instead of tidyquant. Set your unique FRED API key here:
#'       https://fredaccount.stlouisfed.org/apikeys and add it to your R global
#'       environment using Sys.setenv(FRED_API_KEY = '{put your key here}').
#'       Annual counts are totals of monthly NSA values, except annual under
#'       construction counts are the value in December of a given year. If
#'       current year is incomplete, it is omitted from annual table. Monthly
#'       counts are seasonally adjusted annual rates, except under construction
#'       counts are the seasonally adjusted rate at end of month
#'       (not annualized).
#'
#' @examples
#' # pull annual data
#' nrc_a <- get_nrc(type = "annual") |>
#'   # keep vars for year and units in buildings with 2+ units
#'   select(year, contains('_2pl')) |>
#'   # keep years in range of interest
#'   filter(year >= 2000)
#'
#' # pull monthly data to calculate ytd SA average
#' nrc_m <- get_nrc("monthly") |>
#'   # reshape to make calculations easier
#'   pivot_longer(!c("year", "month"), names_to = "var", values_to = "value") |>
#'   # keep current year & mf vars
#'   filter(stringr::str_detect(var, '_2pl') & year == 2024) |>
#'   group_by(year, var) |>
#'   mutate(avg = mean(value)) |>
#'   filter(month == 1) |>
#'   select(-month, -value) |>
#'   pivot_wider(names_from = var, values_from = avg)
#'
#' #combine annual and ytd dfs
#' nrc <- rbind(nrc_a, nrc_m)
#'
#' @export
get_nrc <- function(type = "monthly", fred_api_key = Sys.getenv("FRED_API_KEY")) {

  # check if FRED API key is available
  if (is.null(fred_api_key) || fred_api_key == "") {
    stop("FRED API key is missing. Pass it as an argument or set it using Sys.setenv(FRED_API_KEY = 'your_key').")
  }

  # set FRED API key for the session
  fredr::fredr_set_key(fred_api_key)

  tickers <- c(
    'PERMIT', 'PERMIT1', 'PERMIT5',
    'HOUST', 'HOUST1F', 'HOUST5F',
    'UNDCONTSA', 'UNDCON1USA', 'UNDCON5MUSA',
    'COMPUTSA', 'COMPU1USA', 'COMPU5MUSA',
    'PERMITNSA', 'PERMIT1NSA', 'PERMIT5NSA',
    'HOUSTNSA', 'HOUST1FNSA', 'HOUST5FNSA',
    'UNDCONTNSA', 'UNDCON1UNSA', 'UNDCON5MUNSA',
    'COMPUTNSA', 'COMPU1UNSA', 'COMPU5MUNSA'
  )

  variable_labels <- c(
    'permit_saar', 'permit_1_saar', 'permit_5pl_saar',
    'start_saar', 'start_1_saar', 'start_5pl_saar',
    'underconst_sa', 'underconst_1_sa', 'underconst_5pl_sa',
    'compl_saar', 'compl_1_saar', 'compl_5pl_saar',
    'permit_nsa', 'permit_1_nsa', 'permit_5pl_nsa',
    'start_nsa', 'start_1_nsa', 'start_5pl_nsa',
    'underconst_nsa', 'underconst_1_nsa', 'underconst_5pl_nsa',
    'compl_nsa', 'compl_1_nsa', 'compl_5pl_nsa'
  )

  # Create a lookup dataset
  variable_table <- data.frame(symbol = tickers, var = variable_labels, stringsAsFactors = FALSE)

  # pull data using fredr
  all_data <- lapply(tickers, function(ticker) {
    fredr::fredr(
      series_id = ticker,
      observation_start = as.Date("1970-01-01")
    ) |>
      dplyr::select(date, value) |>
      dplyr::mutate(symbol = ticker)
  }) |>
    dplyr::bind_rows()

  df <- all_data |>
    dplyr::left_join(variable_table, by = "symbol") |>
    dplyr::mutate(
      month = lubridate::month(date),
      year = lubridate::year(date)
    ) |>
    dplyr::select(-symbol, -date, year, month, var, value)

  if (type == "annual") {
    annual <- df |>
      dplyr::filter(stringr::str_detect(var, "_nsa") &
                      !stringr::str_detect(var, "underconst")) |>
      dplyr::group_by(year, var) |>
      dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = var, values_from = tot) |>
      dplyr::mutate(
        permit_2pl = permit_nsa - permit_1_nsa,
        start_2pl = start_nsa - start_1_nsa,
        compl_2pl = compl_nsa - compl_1_nsa
      )

    names(annual) <- sub("_nsa", "", names(annual))

    annual_underco <- df |>
      dplyr::filter(stringr::str_detect(var, "_nsa") &
                      stringr::str_detect(var, "underconst")) |>
      dplyr::filter(month == 12) |>
      tidyr::pivot_wider(names_from = var, values_from = value) |>
      dplyr::mutate(
        underconst_2pl = underconst_nsa - underconst_1_nsa
      ) |>
      dplyr::select(-month)

    names(annual_underco) <- sub("_nsa", "", names(annual_underco))

    annual_full <- annual |>
      dplyr::left_join(annual_underco, by = "year")

    lastyear <- max(annual_full$year)
    lastmonth <- max(df$month[df$year == lastyear], na.rm = TRUE)

    if (lastmonth < 12) {
      annual_full <- annual_full |>
        dplyr::filter(year != lastyear)
    }

    annual_full <- annual_full |>
      dplyr::select(year, dplyr::everything()) |>
      dplyr::select(year, dplyr::starts_with("permit"), dplyr::starts_with("start"),
                    dplyr::starts_with("under"), dplyr::starts_with("compl"))

    return(annual_full)

  } else {

    monthly <- df |>
      dplyr::filter(!stringr::str_detect(var, "_nsa")) |>
      tidyr::pivot_wider(names_from = var, values_from = value) |>
      dplyr::mutate(
        permit_2pl = permit_saar - permit_1_saar,
        start_2pl = start_saar - start_1_saar,
        underconst_2pl = underconst_sa - underconst_1_sa,
        compl_2pl = compl_saar - compl_1_saar
      )

    names(monthly) <- sub("_saar|_sa", "", names(monthly))

    monthly <- monthly |>
      dplyr::select(year, month, dplyr::everything()) |>
      dplyr::select(year, month, dplyr::starts_with("permit"),
                    dplyr::starts_with("start"),
                    dplyr::starts_with("under"),
                    dplyr::starts_with("compl"))

    return(monthly)
  }
}
